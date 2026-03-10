open Dream_html
open HTML

let user_id_or_redirect request =
  match Dream.session_field request "user_id" with
  | None -> Error (Dream.redirect request "/login")
  | Some user_id -> Ok user_id

let short_url_label (url : string) : string =
  (* Extract a compact host-style label from a URL, falling back to the raw
     string if parsing fails. *)
  let len = String.length url in
  if len = 0 then url
  else
    try
      (* Strip scheme like https:// if present. *)
      let start =
        match String.index_opt url ':' with
        | Some i when i + 2 < len && String.sub url (i + 1) 2 = "//" -> i + 3
        | _ -> 0
      in
      let rest = String.sub url start (len - start) in
      let slash_index =
        match String.index_opt rest '/' with
        | Some i -> i
        | None -> String.length rest
      in
      String.sub rest 0 slash_index
    with _ ->
      url

let badge_for_kind (kind : string) : string =
  match String.lowercase_ascii kind with
  | "link" -> "Link"
  | "image" -> "Image"
  | "text" -> "Text"
  | "pdf" -> "PDF"
  | other -> other

let list_view request ~(channels : Fragment.Channel.t list) =
  let channel_item (c : Fragment.Channel.t) =
    let href_ = Printf.sprintf "/app/channels/%s" c.id in
    li [ class_ "flex items-center justify-between py-2 border-b border-stone-200" ] [
      a [ href "%s" href_;
          Hx.get "%s" href_;
          Hx.target "#app-main";
          Hx.push_url "true";
          class_ "text-sm text-stone-700 hover:text-stone-900 transition-colors no-underline" ]
        [ txt "%s" c.title ];
      span [ class_ "text-xs text-stone-400" ] [
        txt "%s" c.id
      ];
    ]
  in
  let create_form =
    form
      [ action "/app/channels"; method_ `POST; class_ "space-y-3 mt-8";
        Hx.post "/app/channels";
        Hx.target "#app-main";
        Hx.push_url "true" ]
      [ Dream_html.csrf_tag request;
        div [] [
          label [ for_ "title";
                  class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
            [ txt "New channel" ];
          input [ id "title"; name "title"; required;
                  class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
        ];
        div [] [
          label [ for_ "description";
                  class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
            [ txt "Description (optional)" ];
          input [ id "description"; name "description";
                  class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
        ];
        button [ type_ "submit";
                 class_ "bg-stone-900 text-white text-sm px-4 py-2 hover:bg-stone-700 transition-colors" ]
          [ txt "Create" ];
      ]
  in
  [ Pages.pill "Channels";
    div [ class_ "flex items-end justify-between mb-6" ] [
      h1 [ class_ "text-3xl font-normal tracking-tight text-stone-900 mt-2" ]
        [ txt "Channels" ];
      a [ href "/app";
          Hx.get "/app/overview";
          Hx.target "#app-main";
          Hx.push_url "true";
          class_ "text-xs uppercase tracking-widest text-stone-400 hover:text-stone-900 transition-colors no-underline" ]
        [ txt "Back" ];
    ];
    ul [ class_ "mt-4" ] (List.map channel_item channels);
    create_form;
  ]

let respond_maybe_partial request ~title body =
  if Middleware.is_htmx request then
    Dream_html.respond (div [] body)
  else
    Pages.respond ~request ~title body

let index request ~(channels : Fragment.Channel.t list) =
  respond_maybe_partial request ~title:"Fragment · Channels" (list_view request ~channels)

let channel_detail_view
    request
    ~(channel : Fragment.Channel.t)
    ~(connections_with_blocks : (Fragment.Connection.t * Fragment.Block.t option) list) =
  let is_image (_, block_opt) =
    match block_opt with
    | Some (b : Fragment.Block.t)
      when String.lowercase_ascii b.kind = "image" -> true
    | Some (b : Fragment.Block.t)
      when b.media_json <> None -> true
    | _ -> false
  in
  let image_items, other_items = List.partition is_image connections_with_blocks in
  let render_image_item (_c, block_opt) =
    match block_opt with
    | Some (b : Fragment.Block.t) ->
        let label =
          match b.title with
          | Some t when t <> "" -> t
          | _ -> short_url_label b.content
        in
        let badge = badge_for_kind b.kind in
        let media_count =
          match b.media_json with
          | None -> 0
          | Some s ->
              (try
                 match Yojson.Safe.from_string s with
                 | `List xs -> List.length xs
                 | _ -> 0
               with _ -> 0)
        in
        let primary_src =
          match b.image_url, String.lowercase_ascii b.kind with
          | Some src, _ when src <> "" -> src
          | _, "image" -> b.content
          | _ -> b.content
        in
        let href_block = Printf.sprintf "/app/blocks/%s" b.id in
        li [ class_ "border border-stone-200 rounded-md overflow-hidden bg-white" ]
          [ a [ href "%s" href_block;
                Hx.get "%s" href_block;
                Hx.target "#app-main";
                Hx.push_url "true";
                class_ "block no-underline" ]
              [ div [ class_ "relative aspect-[4/3] bg-stone-100 overflow-hidden" ] [
                  img [ src "%s" primary_src;
                        alt "%s" label;
                        class_ "w-full h-full object-cover" ];
                  (if media_count > 1 then
                     let more = media_count - 1 in
                     div [ class_ "absolute bottom-1 right-1 bg-black/70 text-[0.6rem] text-white px-1.5 py-0.5 rounded-full" ]
                       [ txt "+%d" more ]
                   else
                     span [] []);
                ];
                div [ class_ "px-3 py-2 border-t border-stone-200" ] [
                  p [ class_ "text-sm text-stone-800 truncate" ]
                    [ txt "%s" label ];
                  p [ class_ "text-[0.7rem] text-stone-400 mt-1 uppercase tracking-widest" ]
                    [ txt "%s" badge ];
                ];
              ];
          ]
    | None ->
        li [] []
  in
  let render_other_item ((c : Fragment.Connection.t), (block_opt : Fragment.Block.t option)) =
    match (c.child_type, block_opt) with
    | "block", Some (b : Fragment.Block.t) ->
        let label = short_url_label b.content in
        let badge = badge_for_kind b.kind in
        let has_card =
          (match b.title with Some _ -> true | None -> false)
          || (match b.description with Some _ -> true | None -> false)
          || (match b.image_url with Some _ -> true | None -> false)
        in
        if has_card then
          let title_text =
            match b.title with
            | Some t when t <> "" -> t
            | _ -> label
          in
          let desc_text = match b.description with Some d -> d | None -> "" in
          let media_count =
            match b.media_json with
            | None -> 0
            | Some s ->
                (try
                   match Yojson.Safe.from_string s with
                   | `List xs -> List.length xs
                   | _ -> 0
                 with _ -> 0)
          in
          let thumb =
            match b.image_url with
            | Some src when src <> "" ->
                div [ class_ "relative w-24 h-24 rounded-md overflow-hidden bg-stone-100 border border-stone-200 flex items-center justify-center mr-4" ]
                  [ img [ HTML.src "%s" src;
                          alt "%s" title_text;
                          class_ "w-full h-full object-cover" ];
                    (if media_count > 1 then
                       let more = media_count - 1 in
                       div [ class_ "absolute bottom-1 right-1 bg-black/70 text-[0.6rem] text-white px-1.5 py-0.5 rounded-full" ]
                         [ txt "+%d" more ]
                     else
                       span [] []);
                  ]
            | _ ->
                let short_badge =
                  let len = String.length badge in
                  if len <= 3 then badge else String.sub badge 0 3
                in
                div [ class_ "w-10 h-10 rounded-md bg-stone-100 border border-stone-200 flex items-center justify-center mr-4" ]
                  [ span [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400" ]
                      [ txt "%s" short_badge ] ]
          in
          let href_block = Printf.sprintf "/app/blocks/%s" b.id in
          li [ class_ "py-3 border-b border-stone-200" ]
            [ a [ href "%s" href_block;
                  Hx.get "%s" href_block;
                  Hx.target "#app-main";
                  Hx.push_url "true";
                  class_ "flex items-start no-underline text-left" ]
                [ thumb;
                  div [ class_ "flex-1 min-w-0" ] [
                    p [ class_ "text-sm text-stone-900 truncate" ]
                      [ txt "%s" title_text ];
                    (if desc_text <> "" && desc_text <> title_text then
                       p [ class_ "text-xs text-stone-500 mt-1 line-clamp-2" ]
                         [ txt "%s" desc_text ]
                     else
                       p [ class_ "text-xs text-stone-400 mt-1" ]
                         [ txt "%s" label ]);
                    p [ class_ "text-[0.65rem] text-stone-400 mt-2 uppercase tracking-widest" ]
                      [ txt "%s" badge ];
                  ];
                ] ]
        else
          let href_block = Printf.sprintf "/app/blocks/%s" b.id in
          li [ class_ "py-2 border-b border-stone-200 text-sm text-stone-700 flex items-center justify-between" ]
            [ a [ href "%s" href_block;
                  Hx.get "%s" href_block;
                  Hx.target "#app-main";
                  Hx.push_url "true";
                  class_ "text-sm text-stone-800 hover:text-stone-500 transition-colors no-underline flex-1 truncate" ]
                [ txt "%s" label ];
              span [ class_ "ml-4 text-[0.7rem] uppercase tracking-widest text-stone-400" ]
                [ txt "%s" badge ];
            ]
    | _ ->
        li [ class_ "py-2 border-b border-stone-200 text-sm text-stone-600" ]
          [ Pages.inline_code c.child_type;
            txt " ";
            Pages.inline_code c.child_id;
          ]
  in
  let add_link_form =
    form
      [ action "%s" (Printf.sprintf "/app/channels/%s/blocks/link" channel.id);
        method_ `POST;
        class_ "space-y-4 mt-4";
        Hx.post "%s" (Printf.sprintf "/app/channels/%s/blocks/link" channel.id);
        Hx.target "#app-main";
        Hx.push_url "true" ]
      [ Dream_html.csrf_tag request;
        div [] [
          label [ for_ "url";
                  class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
            [ txt "Link URL (required)" ];
          input [ id "url"; name "url"; required;
                  placeholder "https://…";
                  class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
        ];
        div [] [
          label [ for_ "title";
                  class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
            [ txt "Title (optional)" ];
          input [ id "title"; name "title";
                  placeholder "If empty, we’ll use the page title";
                  class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
        ];
        div [] [
          label [ for_ "description";
                  class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
            [ txt "Description (optional)" ];
          textarea [ id "description"; name "description";
                     class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors min-h-[4rem]" ]
            "";
        ];
        button [ type_ "submit";
                 class_ "w-full bg-stone-900 text-white text-sm px-4 py-2 hover:bg-stone-700 transition-colors" ]
          [ txt "Save link" ];
      ]
  in
  let add_image_form =
    form
      [ action "%s" (Printf.sprintf "/app/channels/%s/blocks/image" channel.id);
        method_ `POST;
        class_ "space-y-4 mt-4";
        Hx.post "%s" (Printf.sprintf "/app/channels/%s/blocks/image" channel.id);
        Hx.target "#app-main";
        Hx.push_url "true" ]
      [ Dream_html.csrf_tag request;
        div [] [
          label [ for_ "image_url";
                  class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
            [ txt "Image URL (required)" ];
          input [ id "image_url"; name "image_url"; required;
                  placeholder "https://…";
                  class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
        ];
        div [] [
          label [ for_ "image_title";
                  class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
            [ txt "Title (optional)" ];
          input [ id "image_title"; name "title";
                  placeholder "Short label for this image";
                  class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
        ];
        div [] [
          label [ for_ "image_description";
                  class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1.5" ]
            [ txt "Description (optional)" ];
          textarea [ id "image_description"; name "description";
                     class_ "block w-full border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 outline-none focus:border-stone-900 transition-colors min-h-[4rem]" ]
            "";
        ];
        button [ type_ "submit";
                 class_ "w-full bg-stone-900 text-white text-sm px-4 py-2 hover:bg-stone-700 transition-colors" ]
          [ txt "Save image" ];
      ]
  in
  let images_section =
    match image_items with
    | [] -> div [] []
    | items ->
        let count = List.length items in
        let cols =
          match count with
          | 1 -> "grid-cols-1"
          | 2 -> "grid-cols-2"
          | 3 -> "grid-cols-3"
          | _ -> "grid-cols-4"
        in
        let grid_classes = Printf.sprintf "grid gap-4 %s" cols in
        div [] [
          p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400 mb-3" ]
            [ txt "Images" ];
          ul [ class_ "%s" grid_classes ]
            (List.map render_image_item items);
        ]
  in
  let connections_section =
    div [] [
      p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400 mb-3 mt-6" ]
        [ txt "Connections" ];
      ul [] (List.map render_other_item other_items);
    ]
  in
  let add_modal =
    div [ id "add-block-modal";
          class_ "fixed inset-0 bg-black/40 flex items-center justify-center hidden z-40" ]
      [ div [ class_ "bg-white rounded-lg shadow-lg w-full max-w-md p-6 space-y-4" ] [
          div [ class_ "flex items-center justify-between mb-2" ] [
            h2 [ class_ "text-sm font-medium tracking-tight text-stone-900" ]
              [ txt "Add to channel" ];
            button
              [ type_ "button";
                class_ "text-xs text-stone-400 hover:text-stone-600";
                onclick "%s" "document.getElementById('add-block-modal').classList.add('hidden')" ]
              [ txt "Close" ];
          ];
          p [ class_ "text-xs text-stone-500" ]
            [ txt "Paste a URL and (optionally) add a title and description." ];
          div [ class_ "inline-flex text-[0.7rem] uppercase tracking-widest border border-stone-200 rounded-full overflow-hidden" ] [
            button
              [ type_ "button";
                id "add-mode-link";
                class_ "px-3 py-1.5 bg-stone-900 text-white";
                onclick "%s" "document.getElementById('add-mode-link').classList.add('bg-stone-900','text-white');document.getElementById('add-mode-image').classList.remove('bg-stone-900','text-white');document.getElementById('add-link-pane').classList.remove('hidden');document.getElementById('add-image-pane').classList.add('hidden')" ]
              [ txt "Link" ];
            button
              [ type_ "button";
                id "add-mode-image";
                class_ "px-3 py-1.5 text-stone-500";
                onclick "%s" "document.getElementById('add-mode-link').classList.remove('bg-stone-900','text-white');document.getElementById('add-mode-image').classList.add('bg-stone-900','text-white');document.getElementById('add-link-pane').classList.add('hidden');document.getElementById('add-image-pane').classList.remove('hidden')" ]
              [ txt "Image" ];
          ];
          div [ id "add-link-pane"; class_ "mt-2" ] [ add_link_form ];
          div [ id "add-image-pane"; class_ "mt-2 hidden" ] [ add_image_form ];
        ] ]
  in
  [ Pages.pill "Channel";
    div [ class_ "flex items-end justify-between mb-6" ] [
      h1 [ class_ "text-3xl font-normal tracking-tight text-stone-900 mt-2" ]
        [ txt "%s" channel.title ];
      a [ href "%s" "/app/channels";
          Hx.get "/app/channels";
          Hx.target "#app-main";
          Hx.push_url "true";
          class_ "text-xs uppercase tracking-widest text-stone-400 hover:text-stone-900 transition-colors no-underline" ]
        [ txt "All channels" ];
    ];
    (match channel.description with
    | None -> div [] []
    | Some d -> p [ class_ "text-sm text-stone-500 mb-6" ] [ txt "%s" d ]);
    images_section;
    connections_section;
    button
      [ type_ "button";
        class_ "mt-6 inline-flex items-center px-4 py-2 text-xs font-medium tracking-widest uppercase border border-stone-300 text-stone-700 hover:border-stone-900 hover:text-stone-900 transition-colors";
        onclick "%s" "document.getElementById('add-block-modal').classList.remove('hidden')" ]
      [ txt "Add to channel" ];
    add_modal;
  ]

let channel
    request
    ~(channel : Fragment.Channel.t)
    ~(connections_with_blocks : (Fragment.Connection.t * Fragment.Block.t option) list) =
  respond_maybe_partial request ~title:"Fragment · Channel"
    (channel_detail_view request ~channel ~connections_with_blocks)

