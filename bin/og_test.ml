let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "Usage: og_test <url>";
    exit 1
  );
  let url = Sys.argv.(1) in
  let open Lwt.Syntax in
  let main () =
    let* (title, description, image_url, media_json) = Fragment_web.Og.fetch url in
    let print_opt label = function
      | None -> Printf.printf "%s: <none>\n%!" label
      | Some v -> Printf.printf "%s: %s\n%!" label v
    in
    print_opt "title" title;
    print_opt "description" description;
    print_opt "image_url" image_url;
    print_opt "media_json" media_json;
    Lwt.return_unit
  in
  Lwt_main.run (main ())

