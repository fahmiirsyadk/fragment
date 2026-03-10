(** All route handlers and the application router. *)

open Lwt.Syntax

(* ── JSON API handlers ───────────────────────────────────────────────────── *)

let register_route request =
  let* body = Dream.body request in
  match Fragment.Auth.payload_of_yojson (Yojson.Safe.from_string body) with
  | Error _ ->
      Dream.json ~status:`Bad_Request {|{"error": "Invalid JSON"}|}
  | Ok payload ->
      let user_id   = Dream.random 16 |> Dream.to_base64url in
      let hashed_pw = Fragment.Auth.hash payload.password_raw in
      let* result = Dream.sql request (fun db ->
        Fragment.User.insert db ~id:user_id ~email:payload.email ~password_hash:hashed_pw
      ) in
      (match result with
      | Ok () ->
          let* () = Middleware.set_user_session request ~id:user_id ~email:payload.email in
          Dream.json {|{"status": "ok", "message": "Registered and logged in"}|}
      | Error _ ->
          Dream.json ~status:`Bad_Request {|{"error": "Email already exists"}|})

let login_route request =
  let* body = Dream.body request in
  match Fragment.Auth.payload_of_yojson (Yojson.Safe.from_string body) with
  | Error _ ->
      Dream.json ~status:`Bad_Request {|{"error": "Invalid JSON"}|}
  | Ok payload ->
      let* result = Dream.sql request (fun db ->
        Fragment.User.get_by_email db ~email:payload.email
      ) in
      (match result with
      | Ok (Some user) ->
          if Fragment.Auth.verify payload.password_raw user.Fragment.User.password_hash then begin
            let* () = Middleware.set_user_session request ~id:user.id ~email:user.email in
            Dream.json {|{"status": "ok", "message": "Logged in"}|}
          end else
            Dream.json ~status:`Unauthorized {|{"error": "Invalid password"}|}
      | Ok None ->
          Dream.json ~status:`Unauthorized {|{"error": "User not found"}|}
      | Error _ ->
          Dream.json ~status:`Internal_Server_Error {|{"error": "Database error"}|})

let logout_route request =
  let* () = Dream.invalidate_session request in
  Dream.json {|{"status": "ok", "message": "Logged out"}|}

let me_route request =
  let user_id    = Dream.session_field request "user_id" in
  let user_email = Dream.session_field request "user_email" in
  match user_id, user_email with
  | Some id, Some email ->
      Dream.json (Printf.sprintf {|{"status": "ok", "user_id": "%s", "email": "%s"}|} id email)
  | _ ->
      Dream.json ~status:`Unauthorized {|{"error": "Not logged in"}|}

(* ── HTML handlers ───────────────────────────────────────────────────────── *)

let index_route request =
  match Dream.session_field request "user_id" with
  | Some _ -> Dream.redirect request "/app"
  | None   -> Home.index request

let login_page request =
  match Dream.session_field request "user_id" with
  | Some _ -> Dream.redirect request "/app"
  | None   -> Login.page request

let signup_page request =
  match Dream.session_field request "user_id" with
  | Some _ -> Dream.redirect request "/app"
  | None   -> Signup.page request

let login_form_route request =
  match form with
  | `Ok fields ->
      let find name = List.assoc_opt name fields |> Option.value ~default:"" in
      let email    = find "email" in
      let password = find "password" in
      if email = "" || password = "" then
        Dream.redirect request "/login?error=1"
      else
        let* result = Dream.sql request (fun db ->
          Fragment.User.get_by_email db ~email
        ) in
        (match result with
        | Ok (Some user) when Fragment.Auth.verify password user.Fragment.User.password_hash ->
            let* () = Middleware.set_user_session request ~id:user.id ~email:user.email in
            Dream.redirect request "/app"
        | Ok _ | Error _ ->
            Dream.redirect request "/login?error=1")
  | _ ->
      Dream.redirect request "/login?error=1"

let signup_form_route request =
  match form with
  | `Ok fields ->
      let find name = List.assoc_opt name fields |> Option.value ~default:"" in
      let email    = find "email" in
      let password = find "password" in
      if email = "" || password = "" then
        Dream.redirect request "/signup?error=1"
      else
        let user_id   = Dream.random 16 |> Dream.to_base64url in
        let hashed_pw = Fragment.Auth.hash password in
        let* result = Dream.sql request (fun db ->
          Fragment.User.insert db ~id:user_id ~email ~password_hash:hashed_pw
        ) in
        (match result with
        | Ok () ->
            let* () = Middleware.set_user_session request ~id:user_id ~email in
            Dream.redirect request "/app"
        | Error _ ->
            Dream.redirect request "/signup?error=1")
  | _ ->
      Dream.redirect request "/signup?error=1"

(* ── Router ──────────────────────────────────────────────────────────────── *)

let routes = [
  Dream.get  "/static/**"           (Dream.static "static");
  Dream.get  "/"                    index_route;
  Dream.get  "/login"               login_page;
  Dream.post "/login"               login_form_route;
  Dream.get  "/signup"              signup_page;
  Dream.post "/signup"              signup_form_route;
  Dream.get  "/app"                 (Middleware.require_auth_html Home.app);
  Dream.get  "/app/api"             (Middleware.require_auth_html Home.app_api);
  Dream.get  "/app/overview"        (Middleware.require_auth_html Home.app_overview);
  Dream.post "/api/auth/register"   register_route;
  Dream.post "/api/auth/login"      login_route;
  Dream.post "/api/auth/logout"     logout_route;
  Dream.get  "/api/auth/me"         (Middleware.require_auth me_route);
]
