open Lwt.Syntax
open Yojson.Safe.Util

(* Very small, best-effort Open Graph / tweet scraper.
   Returns (title, description, image_url) if we can find them.

   Implementation note: this uses the system's [curl] binary (via [Lwt_process]).
   For generic URLs we scrape <meta> OG tags from HTML. For Twitter/X status URLs
   we call the internal GraphQL TweetResultByRestId endpoint, following the
   pattern used by Scweet's v4 engine (bearer token + auth_token/ct0 cookies). *)

let contains_sub (needle : string) (haystack : string) : bool =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec aux i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else aux (i + 1)
  in
  if nlen = 0 then true else aux 0

let index_sub (needle : string) (haystack : string) : int option =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec aux i =
    if i + nlen > hlen then None
    else if String.sub haystack i nlen = needle then Some i
    else aux (i + 1)
  in
  if nlen = 0 then Some 0 else aux 0

let extract_meta ~name_or_prop ~(html : string) : string option =
  let len = String.length html in
  let rec find_from i =
    if i >= len then
      None
    else
      match String.index_from_opt html i '<' with
      | None -> None
      | Some j ->
          if j + 5 >= len then None
          else if String.sub html j 5 <> "<meta" then
            find_from (j + 1)
          else
            (* crude slice of the tag up to '>' *)
            let k =
              match String.index_from_opt html j '>' with
              | None -> len - 1
              | Some k -> k
            in
            let tag = String.sub html j (k - j + 1) in
            let target1 = "property=\"" ^ name_or_prop ^ "\"" in
            let target2 = "name=\"" ^ name_or_prop ^ "\"" in
            if contains_sub target1 tag || contains_sub target2 tag then
              (* naive content="..." extraction *)
              let content_key = "content=\"" in
              match index_sub content_key tag with
              | None -> find_from (k + 1)
              | Some idx ->
                  let start = idx + String.length content_key in
                  let rec find_quote i =
                    if i >= String.length tag then i
                    else if tag.[i] = '"' then i
                    else find_quote (i + 1)
                  in
                  let stop = find_quote start in
                  if stop <= start then
                    find_from (k + 1)
                  else
                    Some (String.sub tag start (stop - start))
            else
              find_from (k + 1)
  in
  find_from 0

let rec find_legacy (json : Yojson.Safe.t) : Yojson.Safe.t option =
  match json with
  | `Assoc fields ->
      (* If this object itself has a [legacy] field, prefer that. *)
      (match List.assoc_opt "legacy" fields with
       | Some v -> Some v
       | None ->
           (* Otherwise, search all child values. *)
           let rec search = function
             | [] -> None
             | (_, v) :: rest ->
                 (match find_legacy v with
                  | Some _ as some -> some
                  | None -> search rest)
           in
           search fields)
  | `List xs ->
      let rec search = function
        | [] -> None
        | v :: rest ->
            (match find_legacy v with
             | Some _ as some -> some
             | None -> search rest)
      in
      search xs
  | _ -> None

let extract_tweet_id (url : string) : string option =
  (* Extract numeric tweet id from .../status/<digits>[non-digit or end]. *)
  let needle = "/status/" in
  match index_sub needle url with
  | None -> None
  | Some idx ->
      let start = idx + String.length needle in
      let len = String.length url in
      let rec find_end i =
        if i >= len then i
        else
          match url.[i] with
          | '0' .. '9' -> find_end (i + 1)
          | _ -> i
      in
      let stop = find_end start in
      if stop <= start then
        None
      else
        Some (String.sub url start (stop - start))

let is_twitter_status_url (url : string) : bool =
  (contains_sub "://x.com/" url || contains_sub "://twitter.com/" url)
  && Option.is_some (extract_tweet_id url)

let fetch_twitter_tweet (url : string) : (string option * string option * string option * string option) Lwt.t =
  match extract_tweet_id url with
  | None -> Lwt.return (None, None, None, None)
  | Some tweet_id -> (
      let bearer =
        match Sys.getenv_opt "X_BEARER" with
        | Some s when String.length s > 0 -> s
        | _ ->
            (* DEFAULT_X_BEARER_TOKEN from Scweet v4 account_session.py *)
            "AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs%3D1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA"
      in
      let auth_token = Sys.getenv_opt "X_AUTH_TOKEN" |> Option.value ~default:"" in
      let ct0 = Sys.getenv_opt "X_CT0" |> Option.value ~default:"" in
      if auth_token = "" || ct0 = "" then
        (* Without cookies we generally can't access tweet details reliably. *)
        Lwt.return (None, None, None, None)
      else
        let query_id =
          match Sys.getenv_opt "X_TWEET_QUERY_ID" with
          | Some s when String.length s > 0 -> s
          | _ ->
              (* Default taken from a working TweetDetail call captured from the browser. *)
              "KwGBbJZc6DBx8EKmyQSP7g"
        in
        let variables =
          Printf.sprintf
            {|{"focalTweetId":"%s","with_rux_injections":false,"includePromotedContent":false,"withCommunity":true,"withQuickPromoteEligibilityTweetFields":true,"withBirdwatchNotes":true,"withVoice":true,"withV2Timeline":true}|}
            tweet_id
        in
        let features =
          {|{"rweb_lists_timeline_redesign_enabled":false,"blue_business_profile_image_shape_enabled":true,"responsive_web_graphql_exclude_directive_enabled":true,"verified_phone_label_enabled":false,"creator_subscriptions_tweet_preview_api_enabled":false,"responsive_web_graphql_timeline_navigation_enabled":true,"responsive_web_graphql_skip_user_profile_image_extensions_enabled":false,"tweetypie_unmention_optimization_enabled":true,"vibe_api_enabled":true,"responsive_web_edit_tweet_api_enabled":true,"graphql_is_translatable_rweb_tweet_is_translatable_enabled":true,"view_counts_everywhere_api_enabled":true,"longform_notetweets_consumption_enabled":true,"tweet_awards_web_tipping_enabled":false,"freedom_of_speech_not_reach_fetch_enabled":true,"standardized_nudges_misinfo":true,"tweet_with_visibility_results_prefer_gql_limited_actions_policy_enabled":false,"interactive_text_enabled":true,"responsive_web_text_conversations_enabled":false,"longform_notetweets_rich_text_read_enabled":true,"longform_notetweets_inline_media_enabled":false,"responsive_web_enhance_cards_enabled":false}|}
        in
        let endpoint =
          Printf.sprintf
            "https://x.com/i/api/graphql/%s/TweetDetail"
            query_id
        in
        let cmd_str =
          Printf.sprintf
            "curl -fsSL '%s' \
             -G --data-urlencode 'variables=%s' \
                --data-urlencode 'features=%s' \
             -H 'Authorization: Bearer %s' \
             -H 'x-csrf-token: %s' \
             -H 'Cookie: auth_token=%s; ct0=%s' \
             -H 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36'"
            endpoint variables features bearer ct0 auth_token ct0
        in
        let cmd = Lwt_process.shell cmd_str in
        let* body_str =
          Lwt.catch
            (fun () -> Lwt_process.pread cmd)
            (fun _ex -> Lwt.return "")
        in
        if body_str = "" then
          Lwt.return (None, None, None, None)
        else
          (* Parse TweetDetail (or similar) response.
             We search the JSON tree for a [legacy] object (as used by X's
             internal APIs) and then read full_text and media from it. *)
          let parse () =
            let json = Yojson.Safe.from_string body_str in
            let legacy =
              match find_legacy json with
              | Some l -> l
              | None -> `Null
            in
            let full_text =
              match member "full_text" legacy with
              | `String s when String.length s > 0 -> s
              | _ ->
                  (match member "text" legacy with
                  | `String s -> s
                  | _ -> "")
            in
            let strip_urls (s : string) : string =
              let parts = String.split_on_char ' ' s in
              let filtered =
                List.filter
                  (fun tok ->
                     let len = String.length tok in
                     len = 0
                     || not (String.starts_with ~prefix:"http://" tok
                             || String.starts_with ~prefix:"https://" tok))
                  parts
              in
              String.concat " " filtered |> String.trim
            in
            let base =
              strip_urls full_text
            in
            let title =
              if base = "" then None else Some base
            in
            let description =
              if base = "" then None else Some base
            in
            let media_list =
              match member "extended_entities" legacy |> member "media" with
              | `List ms ->
                  List.filter_map
                    (fun m ->
                       match member "media_url_https" m, member "type" m with
                       | `String url, `String ty when String.length url > 0 ->
                           Some (`Assoc [ "url", `String url; "type", `String ty ])
                       | _ -> None)
                    ms
              | _ -> []
            in
            let image_url =
              match media_list with
              | (`Assoc fields) :: _ ->
                  (match List.assoc_opt "url" fields with
                   | Some (`String url) when String.length url > 0 -> Some url
                   | _ -> None)
              | _ -> None
            in
            let media_json =
              match media_list with
              | [] -> None
              | xs -> Some (Yojson.Safe.to_string (`List xs))
            in
            (title, description, image_url, media_json)
          in
          Lwt.return
            (try parse () with _ -> (None, None, None, None)))

let fetch (url : string) : (string option * string option * string option * string option) Lwt.t =
  if is_twitter_status_url url then
    fetch_twitter_tweet url
  else
    let cmd =
      Lwt_process.shell (Printf.sprintf "curl -fsSL %s" (Filename.quote url))
    in
    let* body_str =
      Lwt.catch
        (fun () -> Lwt_process.pread cmd)
        (fun _ex -> Lwt.return "")
    in
    if body_str = "" then
      Lwt.return (None, None, None, None)
    else
      (* Try OG tags first, then fall back to twitter:card / basic title. *)
      let title =
        match extract_meta ~name_or_prop:"og:title" ~html:body_str with
        | Some _ as v -> v
        | None -> extract_meta ~name_or_prop:"twitter:title" ~html:body_str
      in
      let description =
        match extract_meta ~name_or_prop:"og:description" ~html:body_str with
        | Some _ as v -> v
        | None -> extract_meta ~name_or_prop:"twitter:description" ~html:body_str
      in
      let image_url =
        match extract_meta ~name_or_prop:"og:image" ~html:body_str with
        | Some _ as v -> v
        | None -> extract_meta ~name_or_prop:"twitter:image" ~html:body_str
      in
      Lwt.return (title, description, image_url, None)

