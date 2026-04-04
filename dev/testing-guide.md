# Testing Guide for blockr.dplyr

## Strategy

1. **Unit tests** for expression builders — pure R, no Shiny
2. **testServer** for block reactive logic — uses `blk$expr_server`
3. **AI discovery tests** — `dev/test-ai-discovery.R` (requires OPENAI_API_KEY)
4. **Playwright** for visual e2e — via `blockr-playwright` skill

## Unit tests for expression builders

Expression builders are pure functions in `R/expr-builders.R`. They return bquoted language objects with `.(data)` placeholder. Use this helper to evaluate them:

```r
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("make_filter_expr handles values condition", {
  conds <- list(list(type = "values", column = "cyl",
                     values = list("4", "6"), mode = "include"))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, mtcars)
  expect_true(all(result$cyl %in% c(4, 6)))
})
```

## testServer for blocks

Test the inner `expr_server` to verify expressions are built correctly from constructor state:

```r
test_that("filter block produces correct expression", {
  blk <- new_filter_block(
    state = list(
      conditions = list(list(type = "values", column = "cyl",
                             values = list("4"), mode = "include")),
      operator = "&"
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    expr_result <- session$returned$expr()
    evaluated <- eval_bquoted(expr_result, mtcars)
    expect_true(all(evaluated$cyl == 4))
  })
})
```

### Testing state changes

```r
state <- session$returned$state
state$state(list(conditions = list(...), operator = "&"))
session$flushReact()
expr_result2 <- session$returned$expr()
```

## AI discovery tests

Run `dev/test-ai-discovery.R` or `dev/test-ai-discovery-v2.R` to verify all blocks work with the LLM pipeline. Requires `OPENAI_API_KEY` (in `/workspace/.Renviron`).

## shinytest2 e2e

End-to-end tests in `tests/testthat/test-shinytest2.R` exercise the full round-trip: JS UI → Shiny input binding → R expression → evaluated result. These run headless Chromium via shinytest2 and verify that every block produces the expected output when given specific state.

**Test app:** `tests/testthat/apps/dplyr-e2e/app.R` — a board with all 14 blocks pre-wired to data sources (mtcars, iris, and a small pivot dataset).

**How it works:**

Since blockr's custom JS input bindings aren't detectable by `set_inputs()`, the tests use `run_js()` to call `setState()` + `_submit()` directly on the JS block instance:

```r
set_block_state <- function(app, block_id, input_suffix, state) {
  input_id <- paste0("board-block_", block_id, "-expr-", input_suffix)
  state_json <- jsonlite::toJSON(state, auto_unbox = TRUE, null = "null")
  app$run_js(sprintf(
    "var el = document.getElementById('%s');
     el._block.setState(%s);
     el._block._submit();",
    input_id, state_json
  ))
  app$wait_for_idle()
}
```

Results are read from `exportTestValues()` via `app$get_values()$export$result$<block_id>`.

**Key input ID pattern:** `board-block_<block_id>-expr-<type>_input`

**Running:**

```r
Sys.setenv(NOT_CRAN = "true")
testthat::test_file("tests/testthat/test-shinytest2.R")
```

**Adding a new test:**

1. Add the block + link to `apps/dplyr-e2e/app.R`
2. Add a `test_that()` block that calls `set_block_state()` with the desired JSON state
3. Assert on `get_block_result()` — row counts, column names, cell values

## Playwright e2e

For visual UI testing, use the `blockr-playwright` skill to launch an app and interact via browser automation. See `dev/preview-all-blocks.R` for a dock app with all blocks.
