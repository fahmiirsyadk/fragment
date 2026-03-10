open Dream_html
open HTML

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
  Pages.respond ~title:"Fragment · Home" body

let who_for request =
  match Dream.session_field request "user_email" with
  | Some email -> email
  | None -> "you"

let app_tabs active =
  let base =
    "cursor-pointer border-b px-0 pb-1 text-xs tracking-widest uppercase"
  in
  let cls tab =
    if tab = active
    then base ^ " border-stone-900 text-stone-900"
    else base ^ " border-transparent text-stone-400 hover:text-stone-900"
  in
  div [ class_ "flex gap-4 mb-6" ] [
    a [ href "/app/overview";
        Hx.get "/app/overview";
        Hx.target "#app-main";
        Hx.push_url "true";
        class_ "%s" (cls "overview") ]
      [ txt "Overview" ];
    a [ href "/app/api";
        Hx.get "/app/api";
        Hx.target "#app-main";
        Hx.push_url "true";
        class_ "%s" (cls "api") ]
      [ txt "API surface" ];
  ]

let app_overview_section who =
  [ Pages.pill "Workspace";
    app_tabs "overview";
    div [ class_ "grid grid-cols-[2.5fr_1fr] gap-16 mt-5 pb-10 border-b border-stone-200" ] [
      section [] [
        h1 [ class_ "text-3xl font-normal tracking-tight text-stone-900 mt-2 mb-3" ]
           [ txt "Your channels and blocks." ];
        p  [ class_ "text-stone-500 text-sm leading-relaxed" ]
           [ txt "Authenticated surface. Bookmark data routes coming next." ];
      ];
      section [ class_ "text-xs text-stone-400 space-y-1 mt-2" ] [
        p [] [ txt "Signed in as "; Pages.inline_code who; txt "." ];
      ];
    ];
  ]

let app_api_section =
  [ Pages.pill "Workspace";
    app_tabs "api";
    div [ class_ "mt-10" ] [
      p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400 mb-3" ]
        [ txt "API surface" ];
      ul [ class_ "space-y-2 text-sm text-stone-500" ] [
        li [] [ Pages.inline_code "GET /api/auth/me";    txt " — session debug" ];
        li [] [ Pages.inline_code "POST /api/channels";  txt " — create channels" ];
        li [] [ Pages.inline_code "POST /api/blocks";    txt " — create blocks" ];
      ];
    ];
  ]

let app request =
  let who = who_for request in
  let body = app_overview_section who in
  Pages.respond ~title:"Fragment · App" body

let app_overview request =
  let who = who_for request in
  let body = app_overview_section who in
  if Middleware.is_htmx request then
    Dream_html.respond (div [] body)
  else
    Pages.respond ~title:"Fragment · App" body

let app_api request =
  let body = app_api_section in
  if Middleware.is_htmx request then
    Dream_html.respond (div [] body)
  else
    Pages.respond ~title:"Fragment · App · API" body
