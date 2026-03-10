open Dream_html
open HTML
open Lwt.Syntax

(* ── Public landing page ─────────────────────────────────────────────────── *)

let index _request =
  let body =
    [ Pages.pill "Fragment API";
      div [ class_ "grid grid-cols-[2.5fr_1fr] gap-16 mt-5 pb-10 border-b border-stone-200" ] [
        section [] [
          h1 [ class_ "text-3xl font-normal tracking-tight text-stone-900 mt-2 mb-3" ]
             [ txt "A quiet backend for your links." ];
          p  [ class_ "text-stone-500 text-sm leading-relaxed" ]
             [ txt "Sign in, then connect any UI you like — web, native, or CLI." ];
        ];
        section [ class_ "text-xs text-stone-400 space-y-1 mt-2" ] [
          p [] [ txt "Built on Dream + SQLite" ];
          p [] [ txt "Private by default." ];
        ];
      ];
      div [ class_ "mt-10" ] [
        p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400 mb-3" ]
          [ txt "Get started" ];
        p [ class_ "text-sm text-stone-500" ] [
          txt "Create an account via ";
          Pages.inline_code "/signup";
          txt " or sign in at ";
          Pages.inline_code "/login";
          txt ".";
        ];
      ];
    ]
  in
  Pages.respond ~request:_request ~title:"Fragment · Home" body

(* ── Helpers ─────────────────────────────────────────────────────────────── *)

let who_for request =
  match Dream.session_field request "user_email" with
  | Some email -> email
  | None -> "you"

let fetch_channels request =
  match Dream.session_field request "user_id" with
  | None -> Lwt.return []
  | Some user_id ->
      let* res = Dream.sql request (fun db ->
        Fragment.Channel.list_by_user db ~user_id
      ) in
      (match res with
      | Ok cs -> Lwt.return cs
      | Error _ -> Lwt.return [])

(* ── Overview section ────────────────────────────────────────────────────── *)

let channel_row (c : Fragment.Channel.t) =
  li [ class_ "flex items-center justify-between py-3 border-b border-stone-100 group" ] [
    a [ href "%s" (Printf.sprintf "/app/channels/%s" c.id);
        Hx.get "%s" (Printf.sprintf "/app/channels/%s" c.id);
        Hx.target "#app-main";
        Hx.push_url "true";
        class_ "text-sm text-stone-800 group-hover:text-stone-500 transition-colors no-underline flex-1" ]
      [ txt "%s" c.title ];
    span [ class_ "text-[0.65rem] text-stone-300 font-mono" ]
      [ txt "%s" c.id ];
  ]

let overview_body ~who ~channels =
  let count = List.length channels in
  [ Pages.pill "Overview";
    div [ class_ "grid grid-cols-[2.5fr_1fr] gap-16 mt-5 pb-8 border-b border-stone-200" ] [
      section [] [
        h1 [ class_ "text-3xl font-normal tracking-tight text-stone-900 mt-2 mb-3" ]
           [ txt "Your channels." ];
        p  [ class_ "text-stone-500 text-sm leading-relaxed" ]
           [ txt "Channels hold links, images, and notes. Start one from the header." ];
      ];
      section [ class_ "text-xs text-stone-400 space-y-2 mt-2" ] [
        p [] [ txt "Signed in as "; Pages.inline_code who ];
        p [] [ txt "%d channel%s" count (if count = 1 then "" else "s") ];
      ];
    ];
    div [ class_ "mt-8" ] [
      (if channels = [] then
         p [ class_ "text-sm text-stone-400 italic" ]
           [ txt "No channels yet — create one from the header." ]
       else
         ul [] (List.map channel_row channels));
    ];
  ]

(* ── Handlers (used by router.ml) ───────────────────────────────────────── *)

let app request =
  let who = who_for request in
  let* channels = fetch_channels request in
  Pages.respond ~request ~title:"Fragment · Overview"
    (overview_body ~who ~channels)

let app_overview request =
  let who = who_for request in
  let* channels = fetch_channels request in
  let body = overview_body ~who ~channels in
  if Middleware.is_htmx request then
    Dream_html.respond (div [] body)
  else
    Pages.respond ~request ~title:"Fragment · Overview" body

let app_api request =
  let body =
    [ Pages.pill "API surface";
      div [ class_ "mt-8" ] [
        p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400 mb-3" ]
          [ txt "Available routes" ];
        ul [ class_ "space-y-2 text-sm text-stone-500" ] [
          li [] [ Pages.inline_code "GET /api/auth/me";    txt " — session info" ];
          li [] [ Pages.inline_code "GET /api/channels";   txt " — list channels" ];
          li [] [ Pages.inline_code "POST /api/channels";  txt " — create channel" ];
          li [] [ Pages.inline_code "POST /api/blocks";    txt " — create block" ];
        ];
      ];
    ]
  in
  if Middleware.is_htmx request then
    Dream_html.respond (div [] body)
  else
    Pages.respond ~request ~title:"Fragment · API" body
