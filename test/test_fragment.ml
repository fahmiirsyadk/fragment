open Fragment

(* ── Auth tests ──────────────────────────────────────────────────────────── *)

let test_hash_is_string () =
  let h = Auth.hash "hunter2" in
  Alcotest.(check bool) "hash is non-empty" true (String.length h > 0)

let test_roundtrip () =
  let plaintext = "correct-horse-battery-staple" in
  let h = Auth.hash plaintext in
  Alcotest.(check bool) "correct password verifies" true (Auth.verify plaintext h)

let test_wrong_password_rejected () =
  let h = Auth.hash "rightPassword" in
  Alcotest.(check bool) "wrong password fails" false (Auth.verify "wrongPassword" h)

let test_empty_password_rejected () =
  let h = Auth.hash "somePassword" in
  Alcotest.(check bool) "empty string fails" false (Auth.verify "" h)

let test_corrupt_hash_is_safe () =
  Alcotest.(check bool) "corrupt hash returns false" false
    (Auth.verify "password" "this-is-not-a-valid-bcrypt-hash")

(* ── Auth.payload JSON parsing ───────────────────────────────────────────── *)

let test_payload_parse_ok () =
  let json = Yojson.Safe.from_string {|{"email":"a@b.com","password":"s3cr3t"}|} in
  match Auth.payload_of_yojson json with
  | Ok p ->
      Alcotest.(check string) "email"    "a@b.com" p.email;
      Alcotest.(check string) "password" "s3cr3t"  p.password_raw
  | Error e -> Alcotest.fail ("parse error: " ^ e)

let test_payload_parse_missing_field () =
  let json = Yojson.Safe.from_string {|{"email":"a@b.com"}|} in
  match Auth.payload_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should have failed on missing password field"

(* ── Runner ──────────────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "fragment" [
    "Auth.hash / verify", [
      Alcotest.test_case "hash produces a string"          `Quick test_hash_is_string;
      Alcotest.test_case "correct password roundtrips"     `Quick test_roundtrip;
      Alcotest.test_case "wrong password is rejected"      `Quick test_wrong_password_rejected;
      Alcotest.test_case "empty password is rejected"      `Quick test_empty_password_rejected;
      Alcotest.test_case "corrupt hash is safe (no raise)" `Quick test_corrupt_hash_is_safe;
    ];
    "Auth.payload JSON", [
      Alcotest.test_case "valid payload parses"            `Quick test_payload_parse_ok;
      Alcotest.test_case "missing field fails gracefully"  `Quick test_payload_parse_missing_field;
    ];
    "Storage (SQLite)", [
      Alcotest.test_case "channels insert + list_by_user" `Quick (fun () ->
        let user_id = "user_test" in
        let db_file =
          Filename.concat (Filename.get_temp_dir_name ())
            (Printf.sprintf "fragment-test-%s.sqlite" (Dream.to_base64url (Dream.random 8)))
        in
        at_exit (fun () -> try Sys.remove db_file with _ -> ());

        (* Apply a minimal schema via sqlite3 CLI. *)
        let schema =
          {|
PRAGMA foreign_keys = ON;
CREATE TABLE IF NOT EXISTS users (
  id TEXT PRIMARY KEY,
  email TEXT UNIQUE NOT NULL,
  password_hash TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS channels (
  id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  title TEXT NOT NULL,
  description TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
|}
        in
        let schema_file = db_file ^ ".schema.sql" in
        let oc = open_out schema_file in
        output_string oc schema;
        close_out oc;
        at_exit (fun () -> try Sys.remove schema_file with _ -> ());
        Fragment.Db.apply_schema ~db_file ~schema_file ();

        let handler =
          Dream.sql_pool ("sqlite3:" ^ db_file)
          @@ fun req ->
          let open Lwt.Syntax in
          let* r1 =
            Dream.sql req (fun db ->
              Fragment.Channel.insert db ~id:"c1" ~user_id ~title:"One" ~description:None
            )
          in
          let* r2 =
            Dream.sql req (fun db ->
              Fragment.Channel.insert db ~id:"c2" ~user_id ~title:"Two" ~description:(Some "desc")
            )
          in
          let* rlist =
            Dream.sql req (fun db ->
              Fragment.Channel.list_by_user db ~user_id
            )
          in
          (match r1, r2, rlist with
          | Ok (), Ok (), Ok channels ->
              let titles = channels |> List.map (fun (c : Fragment.Channel.t) -> c.title) in
              Alcotest.(check bool) "contains One" true (List.mem "One" titles);
              Alcotest.(check bool) "contains Two" true (List.mem "Two" titles);
              Dream.empty `OK
          | _ ->
              Dream.empty `Internal_Server_Error)
        in
        ignore (Dream.test handler (Dream.request "")));
    ];
  ]
