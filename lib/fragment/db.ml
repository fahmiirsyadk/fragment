(** Database bootstrap utilities. *)

let apply_schema ?(db_file = "database.sqlite") ?(schema_file = "schema.sql") () =
  (* Use the sqlite3 CLI to apply schema.sql. The schema uses
     CREATE TABLE IF NOT EXISTS, so this is safe to run repeatedly. *)
  if Sys.file_exists schema_file then
    let cmd =
      Printf.sprintf "sqlite3 %s < %s" (Filename.quote db_file) (Filename.quote schema_file)
    in
    ignore (Sys.command cmd)
  else
    ()

