open Dream_html
open HTML

(* ── Shared components ───────────────────────────────────────────────────── *)

let pill label =
  span [ class_ "inline-flex items-center gap-1.5 text-[0.7rem] uppercase tracking-widest text-stone-400" ]
    [ span [ class_ "w-1.5 h-1.5 bg-stone-300 inline-block" ] [];
      txt "%s" label ]

let inline_code s =
  code [ class_ "text-[0.8em] bg-stone-100 text-stone-600 px-1 py-0.5 font-mono" ]
    [ txt "%s" s ]

(* ── Root layout ─────────────────────────────────────────────────────────── *)

let layout ~page_title children =
  html [ lang "en" ] [
    head [] [
      meta  [ charset "utf-8" ];
      meta  [ name "viewport"; content "width=device-width, initial-scale=1" ];
      title [] "%s" page_title;
      link  [ rel "stylesheet"; href "/static/app.css" ];
    ];
    body [ class_ "bg-stone-50 text-stone-900 min-h-screen" ] [
      div [ class_ "max-w-4xl mx-auto px-8 py-10" ] [
        header [ class_ "flex items-center justify-between pb-5 border-b border-stone-200 mb-10" ] [
          div [] [
            a [ href "/";
                class_ "text-sm font-medium tracking-[0.18em] uppercase text-stone-900 no-underline hover:text-stone-500 transition-colors" ]
              [ txt "fragment" ];
          ];
          nav [ class_ "flex items-center gap-6 text-sm text-stone-500" ] [
            a [ href "/login";  class_ "hover:text-stone-900 transition-colors no-underline" ] [ txt "Log in"  ];
            a [ href "/signup"; class_ "hover:text-stone-900 transition-colors no-underline" ] [ txt "Sign up" ];
          ];
        ];
        main [] children;
        Dream_html.Livereload.script;
      ];
    ];
  ]

let respond ~title body =
  Dream_html.respond (layout ~page_title:title body)
