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

let channels_index (request : Dream.request) : Dream.response Lwt.t =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None ->
      Dream.json ~status:`Unauthorized {|{"error": "Not logged in"}|}
  | Some user_id ->
      let* result = Dream.sql request (fun db ->
        Fragment.Channel.list_by_user db ~user_id
      ) in
      (match result with
      | Error _ ->
          Dream.json ~status:`Internal_Server_Error {|{"error":"db"}|}
      | Ok channels ->
          let items =
            channels
            |> List.map (fun (c : Fragment.Channel.t) ->
                   Printf.sprintf
                     {|{"id":"%s","title":"%s","description":%s}|}
                     c.id
                     (String.escaped c.title)
                     (match c.description with
                      | None -> "null"
                      | Some d -> Printf.sprintf "\"%s\"" (String.escaped d)))
            |> String.concat ","
          in
          Dream.json (Printf.sprintf {|{"status":"ok","channels":[%s]}|} items))

let channels_create (request : Dream.request) : Dream.response Lwt.t =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None ->
      Dream.json ~status:`Unauthorized {|{"error": "Not logged in"}|}
  | Some user_id ->
      let* body = Dream.body request in
      let json = Yojson.Safe.from_string body in
      let title =
        match Yojson.Safe.Util.member "title" json with
        | `String s -> s
        | _ -> ""
      in
      let description =
        match Yojson.Safe.Util.member "description" json with
        | `String s -> Some s
        | _ -> None
      in
      if title = "" then
        Dream.json ~status:`Bad_Request {|{"error":"title required"}|}
      else
        let id = Dream.random 16 |> Dream.to_base64url in
        let* db_res = Dream.sql request (fun db ->
          Fragment.Channel.insert db ~id ~user_id ~title ~description
        ) in
        match db_res with
        | Ok () ->
            Dream.json (Printf.sprintf {|{"status":"ok","id":"%s"}|} id)
        | Error _ ->
            Dream.json ~status:`Internal_Server_Error {|{"error":"db"}|}

(* ── HTML handlers ───────────────────────────────────────────────────────── *)

let index_route request =
  match Dream.session_field request "user_id" with
  | Some _ -> Dream.redirect request "/app/channels"
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
  let* form = Dream.form request in
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
  let* form = Dream.form request in
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

let logout_page request =
  let open Lwt.Syntax in
  let* _ = Dream.form request in
  let* () = Dream.invalidate_session request in
  Dream.redirect request "/"

(* ── Channels HTML (HTMX) ─────────────────────────────────────────────────── *)

let channels_page request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
      let* result = Dream.sql request (fun db ->
        Fragment.Channel.list_by_user db ~user_id
      ) in
      (match result with
      | Error _ -> Dream.html ~status:`Internal_Server_Error "db"
      | Ok channels -> Channels.index request ~channels)

let channels_create_page request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
      let* form = Dream.form request in
      (match form with
      | `Ok fields ->
          let find name = List.assoc_opt name fields |> Option.value ~default:"" in
          let title = find "title" in
          let description =
            match find "description" with
            | "" -> None
            | s -> Some s
          in
          if title = "" then
            Dream.redirect request "/app/channels"
          else
            let id = Dream.random 16 |> Dream.to_base64url in
            let* db_res = Dream.sql request (fun db ->
              Fragment.Channel.insert db ~id ~user_id ~title ~description
            ) in
            (match db_res with
            | Error _ -> Dream.html ~status:`Internal_Server_Error "db"
            | Ok () -> channels_page request)
      | _ ->
          Dream.redirect request "/app/channels")

let channel_show request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
      let channel_id = Dream.param request "id" in
      let* ch_res = Dream.sql request (fun db ->
        Fragment.Channel.get_by_id db ~id:channel_id ~user_id
      ) in
      (match ch_res with
      | Error _ -> Dream.html ~status:`Internal_Server_Error "db"
      | Ok None -> Dream.not_found request
      | Ok (Some channel) ->
          let* conn_res = Dream.sql request (fun db ->
            Fragment.Connection.list_for_channel db ~parent_channel_id:channel_id ~user_id
          ) in
          (match conn_res with
          | Error _ -> Dream.html ~status:`Internal_Server_Error "db"
          | Ok connections ->
              let* connections_with_blocks =
                Lwt_list.map_s
                  (fun (c : Fragment.Connection.t) ->
                     match c.Fragment.Connection.child_type with
                     | "block" ->
                         let* block_res = Dream.sql request (fun db ->
                           Fragment.Block.get_by_id db ~id:c.child_id ~user_id
                         ) in
                         let block_opt =
                           match block_res with
                           | Ok (Some b) -> Some b
                           | _ -> None
                         in
                         Lwt.return (c, block_opt)
                     | _ ->
                         Lwt.return (c, None))
                  connections
              in
              Channels.channel request ~channel ~connections_with_blocks))

let channel_add_link_block request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
      let channel_id = Dream.param request "id" in
      let* form = Dream.form request in
      (match form with
      | `Ok fields ->
          let find name = List.assoc_opt name fields |> Option.value ~default:"" in
          let url = find "url" in
          if url = "" then
            Dream.redirect request (Printf.sprintf "/app/channels/%s" channel_id)
          else
            let title_input = find "title" in
            let desc_input  = find "description" in
            let block_id = Dream.random 16 |> Dream.to_base64url in
            let conn_id  = Dream.random 16 |> Dream.to_base64url in
            let* (og_title, og_desc, og_image, media_json) = Og.fetch url in
            let title =
              if title_input <> "" then Some title_input else og_title
            in
            let description =
              if desc_input <> "" then Some desc_input else og_desc
            in
            let image_url = og_image in
            let* pos_res = Dream.sql request (fun db ->
              Fragment.Connection.list_for_channel db ~parent_channel_id:channel_id ~user_id
            ) in
            let position =
              match pos_res with
              | Ok xs -> List.length xs
              | Error _ -> 0
            in
            let* _ = Dream.sql request (fun db ->
              Fragment.Block.insert db ~id:block_id ~user_id ~kind:"link" ~content:url
                ~title ~description ~image_url ~media_json
            ) in
            let* _ = Dream.sql request (fun db ->
              Fragment.Connection.insert db ~id:conn_id ~user_id ~parent_channel_id:channel_id
                ~child_id:block_id ~child_type:"block" ~position
            ) in
            Dream.redirect request (Printf.sprintf "/app/channels/%s" channel_id)
      | _ ->
          Dream.redirect request (Printf.sprintf "/app/channels/%s" channel_id))

let channel_add_image_block request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
      let channel_id = Dream.param request "id" in
      let* form = Dream.form request in
      (match form with
      | `Ok fields ->
          let find name = List.assoc_opt name fields |> Option.value ~default:"" in
          let url = find "image_url" in
          if url = "" then
            Dream.redirect request (Printf.sprintf "/app/channels/%s" channel_id)
          else
            let title_input = find "title" in
            let desc_input  = find "description" in
            let title =
              if title_input = "" then None else Some title_input
            in
            let description =
              if desc_input = "" then None else Some desc_input
            in
            let block_id = Dream.random 16 |> Dream.to_base64url in
            let conn_id  = Dream.random 16 |> Dream.to_base64url in
            let* pos_res = Dream.sql request (fun db ->
              Fragment.Connection.list_for_channel db ~parent_channel_id:channel_id ~user_id
            ) in
            let position =
              match pos_res with
              | Ok xs -> List.length xs
              | Error _ -> 0
            in
            let* _ = Dream.sql request (fun db ->
              Fragment.Block.insert db ~id:block_id ~user_id ~kind:"image" ~content:url
                ~title ~description ~image_url:(Some url) ~media_json:None
            ) in
            let* _ = Dream.sql request (fun db ->
              Fragment.Connection.insert db ~id:conn_id ~user_id ~parent_channel_id:channel_id
                ~child_id:block_id ~child_type:"block" ~position
            ) in
            Dream.redirect request (Printf.sprintf "/app/channels/%s" channel_id)
      | _ ->
          Dream.redirect request (Printf.sprintf "/app/channels/%s" channel_id))

let block_show request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
      let block_id = Dream.param request "id" in
      let* res = Dream.sql request (fun db ->
        Fragment.Block.get_by_id db ~id:block_id ~user_id
      ) in
      (match res with
      | Error _ -> Dream.html ~status:`Internal_Server_Error "db"
      | Ok None -> Dream.not_found request
      | Ok (Some block) ->
          if Middleware.is_htmx request then
            Block_detail.show_partial request ~block
          else
            Block_detail.show request ~block)

let block_update request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
      let block_id = Dream.param request "id" in
      let* form = Dream.form request in
      (match form with
      | `Ok fields ->
          let find name = List.assoc_opt name fields |> Option.value ~default:"" in
          let title = find "title" in
          let description = find "description" in
          let title_opt =
            if title = "" then None else Some title
          in
          let desc_opt =
            if description = "" then None else Some description
          in
          let* _ = Dream.sql request (fun db ->
            Fragment.Block.update_metadata db ~id:block_id ~user_id ~title:title_opt ~description:desc_opt
          ) in
          block_show request
      | _ ->
          block_show request)

let block_delete request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
      let block_id = Dream.param request "id" in
      let* _ = Dream.form request in
      let* _ = Dream.sql request (fun db ->
        Fragment.Connection.delete_for_block db ~child_id:block_id ~user_id
      ) in
      let* _ = Dream.sql request (fun db ->
        Fragment.Block.delete db ~id:block_id ~user_id
      ) in
      Dream.redirect request "/app/channels"

(* ── Router ──────────────────────────────────────────────────────────────── *)

let routes = [
  Dream.get  "/static/**"           (Dream.static "static");
  Dream.get  "/"                    index_route;
  Dream.get  "/login"               login_page;
  Dream.post "/login"               login_form_route;
  Dream.get  "/signup"              signup_page;
  Dream.post "/signup"              signup_form_route;
  Dream.post "/logout"              logout_page;
  Dream.get  "/app"                 (Middleware.require_auth_html Home.app);
  Dream.get  "/app/api"             (Middleware.require_auth_html Home.app_api);
  Dream.get  "/app/overview"        (Middleware.require_auth_html Home.app_overview);
  Dream.get  "/app/channels"        (Middleware.require_auth_html channels_page);
  Dream.post "/app/channels"        (Middleware.require_auth_html channels_create_page);
  Dream.get  "/app/channels/:id"    (Middleware.require_auth_html channel_show);
  Dream.post "/app/channels/:id/blocks/link" (Middleware.require_auth_html channel_add_link_block);
  Dream.post "/app/channels/:id/blocks/image" (Middleware.require_auth_html channel_add_image_block);
  Dream.get  "/app/blocks/:id"      (Middleware.require_auth_html block_show);
   Dream.post "/app/blocks/:id/update" (Middleware.require_auth_html block_update);
   Dream.post "/app/blocks/:id/delete" (Middleware.require_auth_html block_delete);
  (* Channels JSON API *)
   Dream.get  "/api/channels"        channels_index;
   Dream.post "/api/channels"        channels_create;
  Dream.post "/api/auth/register"   register_route;
  Dream.post "/api/auth/login"      login_route;
  Dream.post "/api/auth/logout"     logout_route;
  Dream.get  "/api/auth/me"         (Middleware.require_auth me_route);
]
