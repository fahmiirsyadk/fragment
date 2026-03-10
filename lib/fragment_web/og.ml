open Lwt.Syntax
open Yojson.Safe.Util

(* Small, best-effort Open Graph / tweet scraper.
   Returns (title, description, image_url, media_json) if we can find them.
*)

type og_result =
  string option * string option * string option * string option

let ( let*? ) opt f =
  match opt with
  | Some x -> f x
  | None -> None

let contains_sub (needle : string) (haystack : string) : bool =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec aux i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else aux (i + 1)
  in
  if nlen = 0 then true else aux 0

let index_sub ?(from = 0) (needle : string) (haystack : string) : int option =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec aux i =
    if i + nlen > hlen then None
    else if String.sub haystack i nlen = needle then Some i
    else aux (i + 1)
  in
  if nlen = 0 then Some from else aux from
let meta_content (soup : Soup.soup Soup.node) (keys : string list) :
    string option =
  let open Soup in
  let rec first = function
    | [] -> None
    | key :: rest ->
        let selector =
          Printf.sprintf "meta[property='%s'], meta[name='%s']" key key
        in
        (match soup $? selector with
         | None -> first rest
         | Some elt ->
             (match attribute "content" elt with
              | Some v ->
                  let v = String.trim v in
                  if v = "" then first rest else Some v
              | None -> first rest))
  in
  first keys

let extract_title_tag (soup : Soup.soup Soup.node) : string option =
  let open Soup in
  match soup $? "title" with
  | None -> None
  | Some elt ->
      let txt = R.leaf_text elt |> String.trim in
      if txt = "" then None else Some txt


let strip_urls (s : string) : string =
  let parts = String.split_on_char ' ' s in
  let filtered =
    List.filter
      (fun tok ->
         let len = String.length tok in
         len = 0
         || not
              (String.starts_with ~prefix:"http://" tok
              || String.starts_with ~prefix:"https://" tok))
      parts
  in
  String.concat " " filtered |> String.trim

let shell_quote_single (s : string) =
  "'" ^ String.concat "'\"'\"'" (String.split_on_char '\'' s) ^ "'"

let http_fetch
    ?(headers = [])
    ?body
    ?(meth = `GET)
    (url : string) : string option Lwt.t =
  let method_str =
    match meth with
    | `GET -> "GET"
    | `POST -> "POST"
    | `PUT -> "PUT"
    | `DELETE -> "DELETE"
    | `HEAD -> "HEAD"
    | `CONNECT -> "CONNECT"
    | `OPTIONS -> "OPTIONS"
    | `TRACE -> "TRACE"
    | `PATCH -> "PATCH"
    | `Method custom -> custom
  in
  let header_args =
    headers
    |> List.map (fun (k, v) ->
           "-H " ^ shell_quote_single (k ^ ": " ^ v))
    |> String.concat " "
  in
  let body_arg =
    match body with
    | None -> ""
    | Some b -> " --data-binary " ^ shell_quote_single b
  in
  let cmd_str =
    Printf.sprintf
      "curl -fsSL -X %s %s%s %s"
      (shell_quote_single method_str)
      header_args
      body_arg
      (shell_quote_single url)
  in
  let cmd = Lwt_process.shell cmd_str in
  Lwt.catch
    (fun () ->
       let* response_body = Lwt_process.pread cmd in
       Lwt.return (Some response_body))
    (fun _ -> Lwt.return_none)

let string_field name (json : Yojson.Safe.t) : string option =
  match member name json with
  | `String s when String.length s > 0 -> Some s
  | _ -> None

let int_field name (json : Yojson.Safe.t) : int option =
  match member name json with
  | `Int n -> Some n
  | _ -> None

let list_field name (json : Yojson.Safe.t) : Yojson.Safe.t list option =
  match member name json with
  | `List xs -> Some xs
  | _ -> None

let has_tweet_fields (json : Yojson.Safe.t) : bool =
  match json with
  | `Assoc fields ->
      List.exists
        (fun (k, _) -> k = "full_text" || k = "extended_entities")
        fields
  | _ -> false

let tweet_result_matches_id (tweet_id : string) (json : Yojson.Safe.t) : bool =
  let matches_id value_opt =
    match value_opt with
    | Some s when s = tweet_id -> true
    | _ -> false
  in
  matches_id (string_field "rest_id" json)
  ||
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "legacy" fields with
      | Some legacy -> matches_id (string_field "id_str" legacy)
      | None -> false
    )
  | _ -> false

let rec find_tweet_result (tweet_id : string) (json : Yojson.Safe.t) :
    Yojson.Safe.t option =
  match json with
  | `Assoc fields ->
      if tweet_result_matches_id tweet_id json then
        Some json
      else
        let rec search = function
          | [] -> None
          | (_, v) :: rest -> (
              match find_tweet_result tweet_id v with
              | Some _ as some -> some
              | None -> search rest
            )
        in
        search fields
  | `List xs ->
      let rec search = function
        | [] -> None
        | v :: rest -> (
            match find_tweet_result tweet_id v with
            | Some _ as some -> some
            | None -> search rest
          )
      in
      search xs
  | _ -> None

let rec find_legacy (json : Yojson.Safe.t) : Yojson.Safe.t option =
  match json with
  | `Assoc fields ->
      let from_self =
        match List.assoc_opt "legacy" fields with
        | Some legacy when has_tweet_fields legacy -> Some legacy
        | _ -> None
      in
      (match from_self with
       | Some _ as some -> some
       | None ->
           let rec search = function
             | [] -> None
             | (_, v) :: rest -> (
                 match find_legacy v with
                 | Some _ as some -> some
                 | None -> search rest
               )
           in
           search fields)
  | `List xs ->
      let rec search = function
        | [] -> None
        | v :: rest -> (
            match find_legacy v with
            | Some _ as some -> some
            | None -> search rest
          )
      in
      search xs
  | _ -> None

let extract_tweet_id (url : string) : string option =
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
      if stop <= start then None
      else Some (String.sub url start (stop - start))

let is_twitter_status_url (url : string) : bool =
  (contains_sub "://x.com/" url || contains_sub "://twitter.com/" url)
  && Option.is_some (extract_tweet_id url)

let note_text_of_result (json : Yojson.Safe.t) : string option =
  let*? note_tweet =
    match json with
    | `Assoc fields -> List.assoc_opt "note_tweet" fields
    | _ -> None
  in
  string_field "text" (member "note_tweet_results" note_tweet |> member "result")

let profile_image_of_result (json : Yojson.Safe.t) : string option =
  string_field "profile_image_url_https"
    (member "core" json
     |> member "user_results"
     |> member "result"
     |> member "legacy")

let media_type_field (json : Yojson.Safe.t) : string option =
  string_field "type" json

let video_variant_of_json (json : Yojson.Safe.t) : (string * string * int) option =
  let*? url = string_field "url" json in
  let*? content_type = string_field "content_type" json in
  let bitrate = Option.value (int_field "bitrate" json) ~default:0 in
  Some (url, content_type, bitrate)

let best_video_variant (json : Yojson.Safe.t) : (string * string) option =
  let*? variants = list_field "variants" json in
  match
    variants
    |> List.filter_map video_variant_of_json
    |> List.sort (fun (_, _, b1) (_, _, b2) -> compare b2 b1)
  with
  | (url, content_type, _) :: _ -> Some (url, content_type)
  | [] -> None

let media_base_fields (json : Yojson.Safe.t) : (string * Yojson.Safe.t) list option =
  let*? url = string_field "media_url_https" json in
  let*? media_type = media_type_field json in
  Some [ "url", `String url; "type", `String media_type ]

let media_fields_of_tweet_media (json : Yojson.Safe.t) :
    (string * Yojson.Safe.t) list option =
  let*? base_fields = media_base_fields json in
  let*? media_type = media_type_field json in
  match media_type with
  | "video" | "animated_gif" ->
      let extra_fields =
        match best_video_variant (member "video_info" json) with
        | Some (video_url, content_type) ->
            [ "video_url", `String video_url
            ; "content_type", `String content_type
            ]
        | None -> []
      in
      Some (extra_fields @ base_fields)
  | _ -> Some base_fields

let media_json_of_tweet_media (json : Yojson.Safe.t) : Yojson.Safe.t option =
  Option.map (fun fields -> `Assoc fields) (media_fields_of_tweet_media json)

let media_list_of_legacy (legacy : Yojson.Safe.t) : Yojson.Safe.t list =
  match list_field "media" (member "extended_entities" legacy) with
  | Some media_items -> List.filter_map media_json_of_tweet_media media_items
  | None -> []

module Twitter = struct
  type tweet_id = string
  type json = Yojson.Safe.t

  type credentials = {
    bearer : string;
    auth_token : string;
    ct0 : string;
  }

  type summary = {
    title : string option;
    description : string option;
    image_url : string option;
    media_json : string option;
  }

  let default_bearer =
    "AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs%3D1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA"

  let default_query_id = "KwGBbJZc6DBx8EKmyQSP7g"

  let env_nonempty name default =
    match Sys.getenv_opt name with
    | Some s when String.length s > 0 -> s
    | _ -> default

  let credentials () : credentials option =
    let bearer = env_nonempty "X_BEARER" default_bearer in
    let auth_token = env_nonempty "X_AUTH_TOKEN" "" in
    let ct0 = env_nonempty "X_CT0" "" in
    if auth_token = "" || ct0 = "" then
      None
    else
      Some { bearer; auth_token; ct0 }

  let variables (tweet_id : tweet_id) =
    Printf.sprintf
      {|{"focalTweetId":"%s","with_rux_injections":false,"includePromotedContent":false,"withCommunity":true,"withQuickPromoteEligibilityTweetFields":true,"withBirdwatchNotes":true,"withVoice":true,"withV2Timeline":true}|}
      tweet_id

  let features =
    {|{"rweb_lists_timeline_redesign_enabled":false,"blue_business_profile_image_shape_enabled":true,"responsive_web_graphql_exclude_directive_enabled":true,"verified_phone_label_enabled":false,"creator_subscriptions_tweet_preview_api_enabled":false,"responsive_web_graphql_timeline_navigation_enabled":true,"responsive_web_graphql_skip_user_profile_image_extensions_enabled":false,"tweetypie_unmention_optimization_enabled":true,"vibe_api_enabled":true,"responsive_web_edit_tweet_api_enabled":true,"graphql_is_translatable_rweb_tweet_is_translatable_enabled":true,"view_counts_everywhere_api_enabled":true,"longform_notetweets_consumption_enabled":true,"tweet_awards_web_tipping_enabled":false,"freedom_of_speech_not_reach_fetch_enabled":true,"standardized_nudges_misinfo":true,"tweet_with_visibility_results_prefer_gql_limited_actions_policy_enabled":false,"interactive_text_enabled":true,"responsive_web_text_conversations_enabled":false,"longform_notetweets_rich_text_read_enabled":true,"longform_notetweets_inline_media_enabled":false,"responsive_web_enhance_cards_enabled":false}|}

  let request_url (tweet_id : tweet_id) =
    let query_id = env_nonempty "X_TWEET_QUERY_ID" default_query_id in
    let endpoint =
      Printf.sprintf "https://x.com/i/api/graphql/%s/TweetDetail" query_id
    in
    let encoded_variables = Dream.to_percent_encoded (variables tweet_id) in
    let encoded_features = Dream.to_percent_encoded features in
    Printf.sprintf
      "%s?variables=%s&features=%s"
      endpoint
      encoded_variables
      encoded_features

  let request_headers ~(tweet_id : tweet_id) (creds : credentials) =
    let referer =
      Printf.sprintf "https://x.com/i/web/status/%s" tweet_id
    in
    [
      "Accept", "*/*";
      "Authorization", "Bearer " ^ creds.bearer;
      "x-csrf-token", creds.ct0;
      "x-twitter-active-user", "yes";
      "x-twitter-auth-type", "OAuth2Session";
      "x-twitter-client-language", "en";
      "Referer", referer;
      "Origin", "https://x.com";
      "Cookie", Printf.sprintf "auth_token=%s; ct0=%s" creds.auth_token creds.ct0;
      ( "User-Agent",
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 \
         (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36" );
    ]

  let legacy (json : json) (tweet_result : json) : json =
    match tweet_result with
    | `Assoc _ ->
        let legacy = member "legacy" tweet_result in
        if legacy = `Null then
          Option.value (find_legacy json) ~default:`Null
        else
          legacy
    | _ ->
        Option.value (find_legacy json) ~default:`Null

  let text (tweet_result : json) (legacy : json) =
    let note_text = note_text_of_result tweet_result in
    let fallback_text =
      match string_field "full_text" legacy with
      | Some s -> s
      | None -> Option.value (string_field "text" legacy) ~default:""
    in
    match note_text with
    | Some long when String.length long > String.length fallback_text -> long
    | Some long -> long
    | None -> fallback_text

  let media_json (media_list : Yojson.Safe.t list) =
    match media_list with
    | [] -> None
    | xs -> Some (Yojson.Safe.to_string (`List xs))

  let title_and_description ~(base : string) ~(has_media : bool) =
    if base = "" then
      (None, None)
    else if has_media then
      (Some "x.com", Some base)
    else
      (Some base, None)

  let image_url (media_list : Yojson.Safe.t list) (profile_image : string option) =
    let from_media =
      match media_list with
      | media_json :: _ -> string_field "url" media_json
      | [] -> None
    in
    match from_media, profile_image with
    | Some _ as img, _ -> img
    | None, Some avatar -> Some avatar
    | None, None -> None

  let summary_of_json (tweet_id : tweet_id) (body_str : string) : summary =
    let json = Yojson.Safe.from_string body_str in
    let tweet_result =
      match find_tweet_result tweet_id json with
      | Some t -> t
      | None -> `Null
    in
    let legacy = legacy json tweet_result in
    let base = text tweet_result legacy |> strip_urls in
    let media_list = media_list_of_legacy legacy in
    let profile_image = profile_image_of_result tweet_result in
    let title, description =
      title_and_description ~base ~has_media:(media_list <> [])
    in
    {
      title;
      description;
      image_url = image_url media_list profile_image;
      media_json = media_json media_list;
    }

  let result_of_json (tweet_id : tweet_id) (body_str : string) : og_result =
    let summary = summary_of_json tweet_id body_str in
    (summary.title, summary.description, summary.image_url, summary.media_json)
end

let fetch_twitter_tweet (url : string) : og_result Lwt.t =
  match extract_tweet_id url, Twitter.credentials () with
  | Some tweet_id, Some creds ->
      let headers = Twitter.request_headers ~tweet_id creds in
      let* body_opt = http_fetch ~headers (Twitter.request_url tweet_id) in
      let result =
        match body_opt with
        | Some body_str -> (
            try Twitter.result_of_json tweet_id body_str with
            | _ -> (None, None, None, None)
          )
        | None -> (None, None, None, None)
      in
      Lwt.return result
  | _ ->
      Lwt.return (None, None, None, None)

let fetch_generic (url : string) : og_result Lwt.t =
  let headers =
    [
      ( "User-Agent",
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 \
         (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36" );
      "Accept",
      "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8";
    ]
  in
  let* body_opt = http_fetch ~headers url in
  match body_opt with
  | None -> Lwt.return (None, None, None, None)
  | Some body_str ->
      let soup = Soup.parse body_str in
      let title =
        match meta_content soup [ "og:title"; "twitter:title" ] with
        | Some _ as v -> v
        | None -> extract_title_tag soup
      in
      let description =
        meta_content soup [ "og:description"; "twitter:description" ]
      in
      let image_url =
        meta_content soup [ "og:image"; "twitter:image" ]
      in
      Lwt.return (title, description, image_url, None)

let fetch (url : string) : og_result Lwt.t =
  if is_twitter_status_url url then
    fetch_twitter_tweet url
  else
    fetch_generic url
