(** Dream middleware and session helpers. *)

open Lwt.Syntax

let set_user_session request ~id ~email =
  let* () = Dream.set_session_field request "user_id" id in
  Dream.set_session_field request "user_email" email

let is_htmx request =
  match Dream.header request "HX-Request" with
  | Some "true" -> true
  | _ -> false

(** Protect a JSON API route — responds 401 if the session has no user. *)
let require_auth handler request =
  match Dream.session_field request "user_id" with
  | None   -> Dream.json ~status:`Unauthorized {|{"error": "Not logged in"}|}
  | Some _ -> handler request

(** Protect an HTML route — redirects to /login if unauthenticated. *)
let require_auth_html handler request =
  match Dream.session_field request "user_id" with
  | None   -> Dream.redirect request "/login"
  | Some _ -> handler request
