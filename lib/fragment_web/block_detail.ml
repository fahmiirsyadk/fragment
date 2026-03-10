open Dream_html
open HTML

let media_items_of_block (b : Fragment.Block.t) :
    (string * string * string option) list =
  (* (poster_url, kind, video_url) *)
  match b.media_json with
  | Some s -> (
      try
        match Yojson.Safe.from_string s with
        | `List xs ->
            xs
            |> List.filter_map (function
                 | `Assoc fields ->
                     let url =
                       match List.assoc_opt "url" fields with
                       | Some (`String u) -> u
                       | _ -> ""
                     in
                     let kind =
                       match List.assoc_opt "type" fields with
                       | Some (`String k) -> k
                       | _ -> "image"
                     in
                     let video_url =
                       match List.assoc_opt "video_url" fields with
                       | Some (`String u) when String.length u > 0 -> Some u
                       | _ -> None
                     in
                     if url = "" then None else Some (url, kind, video_url)
                 | _ -> None)
        | _ -> []
      with _ -> [])
  | None ->
      (* Fall back: image block uses content as a single image. *)
      if String.lowercase_ascii b.kind = "image" then
        [ (b.content, "image", None) ]
      else
        []

let body ~(request : Dream.request) ~(block : Fragment.Block.t) =
  let title_text =
    match block.title with
    | Some t when String.length t > 0 -> t
    | _ -> block.content
  in
  let description_text =
    match block.description with
    | Some d when String.length d > 0 -> d
    | _ -> ""
  in
  let media = media_items_of_block block in
  let media_section =
    match media with
    | [] ->
        div [ class_ "border border-dashed border-stone-200 rounded-lg h-64 flex items-center justify-center text-xs text-stone-400" ]
          [ txt "No media for this block yet." ]
    | items ->
        let count = List.length items in
        let cols =
          match count with
          | 1 -> "grid-cols-1"
          | 2 -> "grid-cols-2"
          | 3 -> "grid-cols-3"
          | _ -> "grid-cols-4"
        in
        div [] [
          ul
            [ class_ "%s grid gap-4" cols ]
            (List.map
               (fun (url, kind, video_opt) ->
                  let is_video =
                    let k = String.lowercase_ascii kind in
                    k = "video" || k = "animated_gif"
                  in
                  let media_node =
                    match (is_video, video_opt) with
                    | true, Some vurl ->
                        let proxied =
                          Printf.sprintf "/proxy/twitter?url=%s"
                            (Dream.to_base64url vurl)
                        in
                        video
                          [ HTML.src "%s" proxied;
                            autoplay;
                            loop;
                            muted;
                            playsinline;
                            class_ "w-full h-full object-cover" ]
                          []
                    | _ ->
                        img [ src "%s" url;
                              alt "%s" title_text;
                              class_ "w-full h-full object-cover" ]
                  in
                  let badge =
                    if is_video then
                      let label =
                        if String.lowercase_ascii kind = "animated_gif"
                        then "GIF" else "VIDEO"
                      in
                      div
                        [ class_ "absolute bottom-1 right-1 bg-black/70 text-[0.6rem] text-white px-1.5 py-0.5 rounded-full" ]
                        [ txt "%s" label ]
                    else
                      span [] []
                  in
                  li [ class_ "border border-stone-200 rounded-md overflow-hidden bg-stone-50" ]
                    [ div [ class_ "relative" ]
                        [ media_node; badge ] ])
               items);
        ]
  in
  let original_link =
    a
      [ href "%s" block.content;
        target "_blank";
        class_ "inline-flex items-center text-xs text-stone-500 hover:text-stone-900 underline decoration-stone-300 hover:decoration-stone-900" ]
      [ txt "%s" block.content ]
  in
  let edit_href = Printf.sprintf "/app/blocks/%s?edit=1" block.id in
  let right_panel =
    div [ class_ "space-y-4" ] [
      h2 [ class_ "text-sm font-medium tracking-tight text-stone-900" ]
        [ txt "Block details" ];
      div [ class_ "space-y-1" ] [
        p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400" ]
          [ txt "Title" ];
        p [ class_ "text-sm text-stone-900" ]
          [ txt "%s" title_text ];
      ];
      div [ class_ "space-y-1" ] [
        p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400" ]
          [ txt "Description" ];
        (if description_text = "" || description_text = title_text then
           p [ class_ "text-xs text-stone-400 italic" ]
             [ txt "None yet." ]
         else
           p [ class_ "text-sm text-stone-700 whitespace-pre-line" ]
             [ txt "%s" description_text ]);
      ];
      div [ class_ "space-y-1" ] [
        p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400" ]
          [ txt "Original link" ];
        original_link;
      ];
      div [ class_ "space-y-1" ] [
        p [ class_ "text-[0.7rem] uppercase tracking-widest text-stone-400" ]
          [ txt "Kind" ];
        p [ class_ "text-xs text-stone-500" ]
          [ txt "%s" block.kind ];
      ];
      a
        [ href "%s" edit_href;
          Hx.get "%s" edit_href;
          Hx.target "#app-main";
          Hx.push_url "true";
          class_ "mt-2 inline-flex justify-center w-full border border-stone-300 text-stone-700 text-[0.7rem] px-3 py-1.5 hover:border-stone-900 hover:text-stone-900 transition-colors no-underline" ]
        [ txt "Edit block" ];
      form
        [ action "%s" (Printf.sprintf "/app/blocks/%s/delete" block.id);
          method_ `POST;
          Hx.post "%s" (Printf.sprintf "/app/blocks/%s/delete" block.id);
          Hx.target "#app-main";
          Hx.push_url "true";
          class_ "pt-1" ]
        [ Dream_html.csrf_tag request;
          button
            [ type_ "submit";
              class_ "w-full border border-red-200 text-red-600 text-[0.7rem] px-3 py-1.5 hover:border-red-500 hover:text-red-700 transition-colors";
              onclick "%s" "return confirm('Delete this block? This cannot be undone.')" ]
            [ txt "Delete block" ];
        ];
    ]
  in
  let edit_mode =
    match Dream.query request "edit" with
    | Some _ -> true
    | None -> false
  in
  let modal_inner =
    if not edit_mode then
      []
    else
      let form_node =
        form
          [ action "%s" (Printf.sprintf "/app/blocks/%s/update" block.id);
            method_ `POST;
            Hx.post "%s" (Printf.sprintf "/app/blocks/%s/update" block.id);
            Hx.target "#app-main";
            Hx.push_url "true";
            class_ "bg-white rounded-lg shadow-lg w-full max-w-md p-6 space-y-4" ]
          [ Dream_html.csrf_tag request;
            div [ class_ "flex items-center justify-between mb-1" ] [
              h2 [ class_ "text-sm font-medium tracking-tight text-stone-900" ]
                [ txt "Edit block" ];
              button
                [ type_ "button";
                  class_ "text-xs text-stone-400 hover:text-stone-600";
                  onclick "%s" "document.getElementById('block-edit-modal').classList.add('hidden')" ]
                [ txt "Close" ];
            ];
            div [] [
              label [ for_ "edit_title";
                      class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1" ]
                [ txt "Title" ];
              input [ id "edit_title"; name "title";
                      value "%s" title_text;
                      class_ "block w-full border border-stone-300 bg-white px-2 py-1 text-xs text-stone-900 outline-none focus:border-stone-900 transition-colors" ];
            ];
            div [] [
              label [ for_ "edit_description";
                      class_ "block text-[0.7rem] uppercase tracking-widest text-stone-400 mb-1" ]
                [ txt "Description" ];
              textarea [ id "edit_description"; name "description";
                         class_ "block w-full border border-stone-300 bg-white px-2 py-1 text-xs text-stone-900 outline-none focus:border-stone-900 transition-colors min-h-[3rem]" ]
                "%s" description_text;
            ];
            button
              [ type_ "submit";
                class_ "w-full bg-stone-900 text-white text-[0.7rem] px-3 py-1.5 hover:bg-stone-700 transition-colors" ]
              [ txt "Save changes" ];
          ]
      in
      [ div
          [ class_ "flex items-center justify-center w-full h-full";
            onclick "%s" "document.getElementById('block-edit-modal').classList.add('hidden')" ]
          [ div
              [ onclick "%s" "event.stopPropagation()" ]
              [ form_node ] ]
      ]
  in
  [ Pages.pill "Block";
    div [ class_ "grid grid-cols-[2.5fr_1fr] gap-12 mt-5 pb-10 border-b border-stone-200" ] [
      section [] [
        media_section;
      ];
      section [] [ right_panel ];
    ];
    (* modal container for block edit form; visible and populated when ?edit=1 *)
    div [ id "block-edit-modal";
          class_ (if edit_mode
                  then "fixed inset-0 bg-black/40 flex items-center justify-center z-40"
                  else "fixed inset-0 bg-black/40 flex items-center justify-center hidden z-40") ]
      modal_inner;
  ]

let show request ~(block : Fragment.Block.t) =
  let body = body ~request ~block in
  Pages.respond ~request ~title:"Fragment · Block" body

let show_partial request ~(block : Fragment.Block.t) =
  let body = body ~request ~block in
  Dream_html.respond (div [] body)

