# AGENTS.md

## What this is

`saferMake` is a small R package wrapping a Shiny app that converts a user-pasted R function into a "safer-r" function. The only export is `make()` (`R/make.R`), which just launches the app bundled under `inst/app/`.

Code layout after the R/ extraction (helpers are internal, accessed by the app via `saferMake:::`):
- `R/app_helpers.R`, `R/arg_check.R`, `R/rebuild.R` — pure code-generation logic (`build_rebuilt()`, `extract_aa_body()`, `arg_check_call_core()`, `run_arg_check_tests()`, …), unit tested in `tests/testthat/test-app-helpers.R`.
- `inst/app/server.R` — Shiny layer only: reactive state, screens, download handler. Aliases the package helpers at the top of `server()`.

## Commands

- Run the app in development: `devtools::load_all()` then `shiny::runApp("inst/app/")` — `load_all()` is now REQUIRED: the server resolves helpers through `saferMake:::`, so running the app without the package loaded fails at session start.
- `saferMake::make()` works on the *installed* package only (it uses `system.file("app", ...)`), so reinstall before testing it end-to-end.
- Tests: `devtools::test()` (75 tests; two run the generated function in a subprocess because the safer-r backbone reads `formals()` through `sys.parent(n = 2)`, which breaks inside testthat closures — see comments in the test file).
- After editing roxygen comments in `R/make.R`: `devtools::document()` (roxygen2 8.0.0 per DESCRIPTION). `NAMESPACE` and `man/` are generated — do not hand-edit.
- No CI workflows exist in the repo (the README rworkflows badge points to a workflow file that is not present).

## Gotchas

- **Backbone duplication**: the safer-r "backbone" injected into converted functions is hard-coded as string literals in `build_rebuilt()` (`R/rebuild.R`). `inst/backbone_v19.6.R` is only a reference copy — editing it changes nothing. Backbone updates (published as `backbone.R` in the `safer-r/.github` profile repo) must be applied in `build_rebuilt()` and mirrored in the reference copy plus version strings: the ui.R sidebar ("Backbone v19.6") and the README badge (stale, still says v19.3).
- **Known generator limitation** (pre-existing, kept for parity): `match_close()` does not track string literals, so a pasted function whose strings contain unbalanced `}` or `)` gets its body truncated — documented in `tests/testthat/test-app-helpers.R`.
- **Runtime dependencies**: `saferDev` (CRAN) is needed to run the `arg_check()` tests and to execute generated functions; the app degrades gracefully when it is absent. Declare any dependency you add (bslib was missing from `Imports` until 2026-10).
- **Reserved arguments**: `lib_path`, `safer_check`, `error_text` are appended to every converted function; input functions declaring these names are rejected.
- **Save flow**: on the result screen, the visible "Run" button runs the `saferDev::arg_check()` tests; the download button is hidden and triggered programmatically via a Shiny custom message only if all tests pass.
- **Error channels**: user-code errors and internal errors show different user-facing texts; real error details go to the console via `message()` and are never shown in the UI. Keep that separation.
- **Generated code style**: emitted functions use fully qualified namespaces (`base::paste0(...)`, `saferDev::arg_check`), explicit argument names, and `# nocov` markers — the output must stay CRAN-ready safer-r code. Preserve the exact emitted style in `build_rebuilt()`.
- `dev/` is scratch space, excluded from the R build via `.Rbuildignore` — not package code.
