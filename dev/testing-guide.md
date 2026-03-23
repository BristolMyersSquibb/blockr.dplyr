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

## Playwright e2e

For UI testing, use the `blockr-playwright` skill to launch an app and interact via browser automation. See `dev/preview-all-blocks.R` for a dock app with all blocks.
