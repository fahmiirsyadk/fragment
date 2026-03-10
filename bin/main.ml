(** Entry point.

    Runtime configuration is handled by dream-cli — pass --help to see all
    available flags (--port, --interface, --log-level, etc.). *)

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
  (* Load environment variables from .env if present (for local dev). *)
  load_dotenv_if_present ();
  (* Ensure the SQLite schema is applied before the server starts. Safe to call
     multiple times because schema.sql uses CREATE TABLE IF NOT EXISTS. *)
  let db_url =
    match Sys.getenv_opt "DATABASE_URL" with
    | Some s when String.length s > 0 -> s
    | _ -> "sqlite3:database.sqlite"
  in
  (* If DATABASE_URL is a sqlite3 URL, keep the existing file-based bootstrap. *)
  (match String.length db_url >= 7 && String.sub db_url 0 7 = "sqlite3:" with
  | true ->
      let db_file = String.sub db_url 7 (String.length db_url - 7) in
      Fragment.Db.apply_schema ~db_file ();
  | false ->
      ());
  let secret =
    match Sys.getenv_opt "FRAGMENT_SECRET" with
    | Some s when String.length s > 0 -> s
    | _ ->
      (match Sys.getenv_opt "DREAM_SECRET" with
      | Some s when String.length s > 0 -> s
      | _ ->
          (* Dev default: generates a new secret each run (sessions/CSRF won't persist). *)
          let s = Dream.to_base64url (Dream.random 32) in
          prerr_endline "WARN: FRAGMENT_SECRET (or DREAM_SECRET) not set; generating ephemeral secret.";
          s)
  in
  Dream_cli.run
  @@ Dream.logger
  @@ Dream.sql_pool db_url
  @@ Dream.set_secret secret
  @@ Dream.cookie_sessions
  @@ Dream.router Fragment_web.Router.routes
