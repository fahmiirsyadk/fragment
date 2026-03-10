let load_dotenv_if_present () =
  let path = ".env" in
  if Sys.file_exists path then (
    let ic = open_in path in
    let rec loop () =
      match input_line ic with
      | line ->
          let line = String.trim line in
          if line <> "" && line.[0] <> '#' then (
            match String.index_opt line '=' with
            | Some idx ->
                let key = String.sub line 0 idx |> String.trim in
                let value =
                  String.sub line (idx + 1) (String.length line - idx - 1)
                  |> String.trim
                in
                if key <> "" then Unix.putenv key value
            | None -> ());
          loop ()
      | exception End_of_file -> ()
    in
    loop ();
    close_in_noerr ic
  )

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "Usage: og_test <url>";
    exit 1
  );
  (* Load .env so X cookies / bearer are available, like main. *)
  load_dotenv_if_present ();
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

