(** Entry point.

    Runtime configuration is handled by dream-cli — pass --help to see all
    available flags (--port, --interface, --log-level, etc.). *)

let () =
  Dream_cli.run
  @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:database.sqlite"
  @@ Dream.cookie_sessions
  @@ Dream.router Fragment_web.Router.routes
