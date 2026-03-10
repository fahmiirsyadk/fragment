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
  ]
