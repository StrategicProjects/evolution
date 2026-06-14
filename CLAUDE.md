# evolution

R client for the **Evolution Cloud API** (<https://evoapicloud.com>) — send/receive
WhatsApp messages from R. CRAN package by Andre Leite et al.

## Layout

- `R/evolution.R` — all the code: `evo_client()` factory, internal `.evo_post()`
  (the single HTTP entry point), helpers (`jid()`, `.normalize_media()`,
  `.normalize_number()`, `.compact()`, `.evo_path()`, `.assert_scalar_string()`),
  and the `send_*()` / `check_is_whatsapp()` endpoints.
- `R/zzz.R` — `.onLoad`/`.onUnload` (the `evolution.timeout` option) and the
  internal `%||%` definition.
- `tests/testthat/` — offline unit tests (helpers + argument-validation guards).
  No network is hit; `send_*()` examples are all `\dontrun{}`.
- `man/` — roxygen-generated; never edit by hand.
- `README.Rmd` → `README.md` (knit, don't edit the `.md` directly).

## Conventions

- HTTP goes through **httr2**; every endpoint ends in `.evo_post(client, path, body)`.
  `.evo_post()` already drops top-level `NULL`s via `.compact()`, so endpoints
  generally don't need to compact again (the nested compact in `send_contact()`
  is the exception and is intentional).
- User-facing messages/errors use **cli** (`cli_abort`, `cli_warn`, `cli_alert_*`).
- `number` args are normalised with `.normalize_number()` (strips `+`, spaces,
  dashes, parens; passes `@`-JIDs like `...@g.us` through unchanged).
- The package targets `R (>= 4.2.0)`, so do **not** rely on base-R `%||%`
  (4.4.0+); use the internal one in `zzz.R`.
- `RoxygenNote: 7.3.3` in DESCRIPTION — keep regenerated docs consistent with
  that (a newer local roxygen2 will try to rewrite the field; restore it).

## Workflows

- Regenerate docs: `Rscript -e 'roxygen2::roxygenise(".")'`
- Run tests fast: `Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'`
- Full check: `R CMD build . && R CMD check --as-cran evolution_*.tar.gz`
  (locally on macOS expect 2 benign NOTEs: outdated HTML Tidy, and a
  `.DS_Store` the Finder drops inside `.Rcheck` — neither is in the tarball
  and neither appears on CI / win-builder).
- CI: `.github/workflows/R-CMD-check.yaml` runs the r-lib standard matrix
  (Linux devel/release/oldrel-1, macOS, Windows) on push/PR.
- Build artifacts (`evolution.Rcheck/`, `*.tar.gz`) and `.claude/`, `.github/`,
  `.Rhistory`, `.DS_Store` are git/Rbuild-ignored.
