open Dream_html
open HTML

let page request =
  let has_error =
    match Dream.query request "error" with
    | Some _ -> true
    | None   -> false
  in
  let error_block =
    if has_error then
      [ p [ class_ "text-xs text-red-500 mb-4" ]
          [ txt "We couldn't create that account. Try a different email." ] ]
    else []
  in
  let body =
    [ Pages.pill "Sign up";
      div [ class_ "grid grid-cols-[2.5fr_1fr] gap-16 mt-5 pb-10 border-b border-stone-200" ] [
        section [] [
          h1 [ class_ "text-3xl font-normal tracking-tight text-stone-900 mt-2 mb-3" ]
             [ txt "Create your Fragment account." ];
          p  [ class_ "text-stone-500 text-sm leading-relaxed" ]
             [ txt "Just an email and password. Plug any UI into it." ];
        ];
      ];
      div [ class_ "mt-10 max-w-sm" ] (
        error_block @ [
          form [ action "/signup"; method_ `POST; class_ "space-y-5" ] [
            Dream_html.csrf_tag request;
            div [] [
              label [ for_ "email";
                      class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
                    [ txt "Email" ];
              input [ id "email"; name "email"; type_ "email"; required;
                      autocomplete `email;
                      class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
            ];
            div [] [
              label [ for_ "password";
                      class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
                    [ txt "Password" ];
              input [ id "password"; name "password"; type_ "password"; required;
                      autocomplete `new_password;
                      class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
            ];
            button [ type_ "submit";
                     class_ "w-full bg-stone-900 text-white text-sm px-4 py-2 hover:bg-stone-700 transition-colors" ]
                   [ txt "Create account" ];
          ];
          p [ class_ "text-xs text-stone-400 mt-4" ]
            [ txt "Already have one? ";
              a [ href "/login"; class_ "underline text-stone-500 hover:text-stone-900 transition-colors" ]
                [ txt "Sign in" ];
              txt "." ];
        ])
    ]
  in
  Pages.respond ~request ~title:"Fragment · Sign up" body
