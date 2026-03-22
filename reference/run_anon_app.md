# Launch the local `anon` Shiny app

The `anon` Shiny app provides a local, session-oriented workflow for
reviewing an environment, selecting objects, cleaning text, and
generating anonymized structural reports suitable for prompt building.

## Usage

``` r
run_anon_app(
  envir = globalenv(),
  launch.browser = interactive(),
  initial_selection = NULL
)
```

## Arguments

- envir:

  Environment or list to inspect. Defaults to
  [`globalenv()`](https://rdrr.io/r/base/environment.html).

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
  Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- initial_selection:

  Optional character vector of object names to preselect after the
  environment is loaded.

## Value

Invisibly returns the running Shiny app object.
