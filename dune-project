(lang dune 3.14)

(name fragment_api)

(generate_opam_files true)

(source (github username/reponame))
(authors "Author Name <author@example.com>")
(maintainers "Maintainer Name <maintainer@example.com>")
(license LICENSE)

(package
 (name fragment_api)
 (synopsis "Private bookmarking backend")
 (description "Dream-based bookmark manager with SQLite storage")
 (depends
  ocaml
  dream
  dream-html
  dream-livereload
  dream-cli
  lambdasoup
  caqti
  caqti-lwt
  caqti-driver-sqlite3
  yojson
  ppx_deriving_yojson
  ppx_rapper
  safepass
  alcotest)
 (tags ("bookmark" "dream" "ocaml")))
