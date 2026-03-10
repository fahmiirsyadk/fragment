# Fragment

Private Bookmarking app

> This project still on heavy development.

## Project structure

```
fragment/
├── bin/
│   ├── dune           ← depends on fragment_web + dream-cli
│   └── main.ml        ← Dream_cli.run + middleware stack
├── lib/
│   ├── fragment/      ← business logic & storage access
│   │   ├── user.ml    ← User type + ppx_rapper DB queries
│   │   └── auth.ml    ← auth payload + password hash/verify
│   ├── fragment_web/  ← API + HTML view layer
│   │   ├── middleware.ml  ← require_auth, require_auth_html, session helpers
│   │   ├── pages.ml       ← shared layout/components
│   │   ├── home.ml / login.ml / signup.ml
│   │   └── router.ml      ← handlers + `routes`
│   └── fragment_app/  ← (optional) client-side JS app layer (placeholder)
└── test/
    ├── dune
    └── test_fragment.ml   ← Alcotest unit tests
```

## Development

### Install JS deps (Tailwind)

```bash
pnpm install
```

### Run Tailwind (writes `static/app.css`)

```bash
pnpm run dev:css
```

### Run server

```bash
eval $(opam env)
dune exec fragment_api -- --help
```

Then run normally (defaults to Dream’s CLI defaults; override with flags):

```bash
dune exec fragment_api -- --port 8080 --interface 0.0.0.0
```

### Run tests

```bash
eval $(opam env)
dune test
```

