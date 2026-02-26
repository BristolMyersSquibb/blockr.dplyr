# Testing Guide for blockr.dplyr

## Strategy

1. **Unit tests** for pure functions (expression builders, helpers, parsers) — no Shiny needed
2. **testServer with `block_server`** for all Shiny reactivity, state, and data transformation

That's it. Two layers, both fast.

---

## Unit Tests

Test pure R functions directly. Fast (~0.1s per test).

```r
test_that("parse_mutate handles single expression", {
  result <- parse_mutate("new_col = old_col * 2")
  expect_equal(result[[1]]$name, "new_col")
  expect_equal(result[[1]]$expr, "old_col * 2")
})
```

---

## testServer (block_server)

This is the main testing pattern. Tests the full block lifecycle: reactive state, expression generation, and data transformation — all through `block_server`, the same entry point the framework uses at runtime.

```r
test_that("filter block filters by selected values", {
  block <- new_filter_block(
    conditions = list(list(column = "cyl", values = c(4, 6), mode = "include"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(all(result$cyl %in% c(4, 6)))
      expect_false(any(result$cyl == 8))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
```

Key points:
- Use `blockr.core:::get_s3_method("block_server", block)` to get the method
- Args format: `list(x = block, data = list(data = function() df))`
- `session$returned$result()` gives the transformed data
- `session$returned$state` gives the reactive state values
- Always call `session$flushReact()` before reading results

### Testing state changes

To test that changing state propagates to the result:

```r
test_that("external state change propagates", {
  block <- new_filter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), 150)

      # Modify state directly (simulates AI ctrl or programmatic updates)
      state <- session$returned$state
      state$conditions(list(
        list(column = "Species", values = c("virginica"), mode = "include")
      ))
      session$flushReact()

      expect_equal(nrow(session$returned$result()), 50)
    },
    args = list(x = block, data = list(data = function() iris))
  )
})
```

---

## Browser Testing

For e2e package tests, shinytest2 is an option but we don't use it — `testServer` covers reactive logic faster and more reliably.

For interactive debugging of UI rendering or JS interactions, use Playwright via the `blockr-playwright` skill. It connects a real browser to a running Shiny app and lets you inspect, screenshot, and interact step by step.
