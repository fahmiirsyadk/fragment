(** Browser-side asset layer.

    CSS is compiled by the pnpm/Tailwind pipeline:
      assets/app.css  →  (tailwindcss CLI)  →  static/app.css

    A richer JS client (e.g. a ReScript or TypeScript SPA) would live here
    and be bundled into static/ for Dream to serve. *)
