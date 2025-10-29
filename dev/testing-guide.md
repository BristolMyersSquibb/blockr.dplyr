# Testing Guide for blockr.dplyr

## Overview

This document outlines the **TWO-TIER testing strategy** for blockr.dplyr. Understanding WHEN to use each tool is critical for fast, maintainable tests.

This guide is adapted from blockr.io and applies the same principles to dplyr transform blocks.

## Testing Philosophy

1. **DEFAULT: Use testServer** - For all Shiny reactivity, UI interactions, and module logic
2. **Use unit tests for pure functions** - Expression builders, helpers, parsers
3. **Speed matters** - Fast tests = fast feedback = better development
4. **shinytest2 is almost never needed** - Only for pure visual UI checks with no server impact

---

## ⚠️ IMPORTANT: Two-Tier Strategy

**99% of your tests should be:**
- **Tier 1: Unit Tests** - Pure R functions (no Shiny)
- **Tier 2: testServer** - ALL Shiny reactivity and UI interactions

**shinytest2 is rarely needed** - See bottom of this guide for the exceptional cases.

---

## The Two Main Testing Layers

### Layer 1: Unit Tests (Pure Functions, No Shiny)

**Files**: `test-multi-kvexpr.R` (3 tests), portions of hybrid test files
**Speed**: ⚡⚡⚡ Very fast (~0.1-0.3s per test)

**What it CAN test**:
- ✅ Pure functions (`parse_mutate()`, helper functions, expression parsers)
- ✅ Expression generation correctness
- ✅ Expression evaluation (execute the generated dplyr code)
- ✅ Edge cases (empty data, invalid inputs, edge cases)
- ✅ Constructor validation
- ✅ Logic that doesn't need Shiny reactivity

**What it CANNOT test**:
- ❌ Reactive values and reactive contexts
- ❌ Shiny modules and moduleServer
- ❌ UI rendering or user interactions
- ❌ State management across reactive updates

**Example from blockr.dplyr**:
```r
test_that("parse_mutate handles single expression", {
  result <- parse_mutate("new_col = old_col * 2")
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$name, "new_col")
  expect_equal(result[[1]]$expr, "old_col * 2")
})

# Can also test by evaluating
test_that("filter block filters data correctly", {
  blk <- new_filter_block()
  expr <- quote(dplyr::filter(mtcars, mpg > 20))
  result <- eval(expr)
  expect_true(all(result$mpg > 20))
})
```

---

### Layer 2: testServer (ALL Shiny Interactions - USE THIS)

**Files**: `test-filter.R`, `test-mutate.R`, `test-select.R`, `test-join.R`, `test-rename.R`, `test-bind.R`, `test-summarize.R`, `test-value_filter.R`, and others
**Speed**: ⚡⚡ Fast (~0.2-0.5s per test)
**Total**: ~150 tests across multiple files

**What it CAN test**:
- ✅ Transform block reactive logic
- ✅ Reactive values and contexts
- ✅ State management
- ✅ Expression generation in reactive context
- ✅ Module server functions
- ✅ Data transformations (by evaluating expressions)
- ✅ **UI interactions** (button clicks, input changes via `session$setInputs()`)
- ✅ **Module add/remove operations**
- ✅ **Reactive auto-updates**
- ✅ **ALL edge cases and error handling**

**When to use**:
- Testing Shiny modules (moduleServer)
- Testing reactive behavior
- Testing UI input changes and their effects
- Testing add/remove operations in multi-* modules
- Testing state management
- **This should be your default for testing ANY Shiny logic**

**Two critical patterns for blockr.dplyr**:

**Pattern 1: Testing Expression Generation (expr_server)**
```r
test_that("filter block generates correct expression", {
  input_data <- reactive(mtcars)
  blk <- new_filter_block(expressions = list("mpg > 20"))

  testServer(
    blk$expr_server,           # Test expr_server directly
    args = list(data = input_data),  # Transform blocks need data argument
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test expression structure
      expr_result <- result$expr()
      expr_str <- deparse(expr_result)
      expect_true(grepl("dplyr::filter", expr_str))
      expect_true(grepl("mpg > 20", expr_str))

      # Can also evaluate to verify data transformation
      filtered_data <- eval(expr_result)
      expect_true(all(filtered_data$mpg > 20))

      # Test state
      expect_true(is.reactive(result$state$expressions))
    }
  )
})
```

**Pattern 2: Testing Data Transformation (block_server) - PREFERRED**
```r
test_that("filter block filters data correctly - testServer", {
  block <- new_filter_block(expressions = list("mpg > 20"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),  # Use block_server, not expr_server
    {
      session$flushReact()
      result <- session$returned$result()  # Get actual transformed data

      # Test the actual data transformation
      expect_true(all(result$mpg > 20))
      expect_true(nrow(result) < nrow(mtcars))
      expect_equal(ncol(result), ncol(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))  # Different args format!
  )
})
```

**When to use each pattern**:
- **Use expr_server** when testing expression generation, reactive state updates, or UI logic
- **Use block_server** when testing actual data transformation behavior (PREFERRED for data correctness tests)

**Key differences**:
- `expr_server`: Returns `list(expr = reactive(...), state = list(...))` - test expression structure
- `block_server`: Returns `list(result = reactive(...), ...)` - test actual data output
- `block_server` uses different args format: `list(x = block, data = list(data = function() df))`
- `block_server` is the same pattern used by the framework internally, so it's more reliable
- **DO NOT use manual `eval()`** - use `block_server` pattern instead

**Known issues**:
- `select_block` and `slice_block` currently return NULL with `block_server` pattern (initialization issue)
- For these blocks, continue using `expr_server` pattern until issue is resolved

---

## Note on shinytest2

**We don't use shinytest2.** All Shiny testing is done with testServer, which can simulate UI interactions via `session$setInputs()`.

### When would shinytest2 be useful? (Very rare cases)

ShinyTest2 performs **full browser-based testing** - it launches a real Shiny app in a headless browser (Chrome/Chromium) and simulates user clicks, typing, etc.

**Use shinytest2 ONLY for:**
- ✅ **Visual regression testing** - Catching CSS/layout changes via screenshots
- ✅ **Complex JavaScript interactions** - Custom JS widgets that testServer can't simulate
- ✅ **Browser-specific rendering bugs** - Issues that only appear in actual browser rendering
- ✅ **Third-party widget integration** - External JS libraries with complex DOM manipulation

**DO NOT use shinytest2 for:**
- ❌ Testing reactive logic (use testServer)
- ❌ Testing data transformations (use testServer with block_server)
- ❌ Testing module interactions (use testServer)
- ❌ Testing input changes and their effects (use testServer with session$setInputs)
- ❌ Testing button clicks, checkboxes, dropdowns (use testServer)

**Why we avoid shinytest2:**
- 🐌 **Very slow** (~2-5 seconds per test vs 0.2s with testServer)
- 🔧 **Brittle** - Breaks on minor UI changes, CSS updates, timing issues
- 🖥️ **Platform-dependent** - Requires Chrome/Chromium installation
- 🐛 **Harder to debug** - Failures are less clear than testServer errors

**Key insight:** If you can test it with `session$setInputs()` in testServer, that's always better than shinytest2.

### Historical note

We previously used shinytest2 but found that 100% of those tests could be replaced with faster, more reliable testServer tests. We removed all shinytest2 tests in October 2025.
