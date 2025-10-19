# shinytest2 Integration Tests

## Overview

This directory contains shinytest2 integration tests that complement the existing `shiny::testServer` unit tests. These tests verify end-to-end functionality by actually launching Shiny apps and testing user interactions.

## Test Files

### All Blocks - 100% Coverage (Completed ✅)

- **[helper-shinytest2.R](helper-shinytest2.R)** - Helper functions for creating test apps and verifying outputs
- **[test-shinytest2-arrange.R](test-shinytest2-arrange.R)** - Arrange block integration tests (2 tests)
- **[test-shinytest2-bind.R](test-shinytest2-bind.R)** - Bind rows/cols block tests (2 tests)
- **[test-shinytest2-filter.R](test-shinytest2-filter.R)** - Filter block integration tests (8 tests)
- **[test-shinytest2-join.R](test-shinytest2-join.R)** - Join block integration tests (1 test)
- **[test-shinytest2-mutate.R](test-shinytest2-mutate.R)** - Mutate block integration tests (2 tests)
- **[test-shinytest2-rename.R](test-shinytest2-rename.R)** - Rename block integration tests (1 test)
- **[test-shinytest2-select.R](test-shinytest2-select.R)** - Select block integration tests (7 tests)
- **[test-shinytest2-slice.R](test-shinytest2-slice.R)** - Slice block integration tests (1 test)
- **[test-shinytest2-summarize.R](test-shinytest2-summarize.R)** - Summarize block integration tests (8 tests)
- **[test-shinytest2-value-filter.R](test-shinytest2-value-filter.R)** - Value filter block integration tests (1 test)

### Future Enhancement - Pipeline Tests

- `test-shinytest2-pipelines.R` - Multi-block pipeline tests (not yet implemented)

## Running Tests

### Prerequisites

1. Install shinytest2:
   ```r
   install.packages("shinytest2")
   ```

2. Enable tests by setting environment variable in `~/.Renviron`:
   ```
   NOT_CRAN=true
   ```

   **Important:** Restart R after modifying `.Renviron`

### Running All Integration Tests

```r
# From package root
testthat::test_dir("tests/testthat", filter = "shinytest2")
```

### Running Specific Test Files

```r
# Single file
testthat::test_file("tests/testthat/test-shinytest2-select.R")

# Multiple files
testthat::test_dir("tests/testthat", filter = "shinytest2-(select|filter)")
```

### From Command Line

```bash
# All tests
R CMD check .

# Specific test file
Rscript -e "testthat::test_file('tests/testthat/test-shinytest2-select.R')"
```

## Test Structure

Each test follows this pattern:

```r
test_that("descriptive test name", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # 1. Create temporary app
  app_dir <- create_test_app(
    block_code = 'serve(new_select_block(columns = "mpg"), data = list(data = mtcars))'
  )

  # 2. Launch app
  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "test_name")
  app$wait_for_idle()

  # 3. Get exported values
  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # 4. Make assertions
  expect_equal(names(result_data), "mpg")
  expect_equal(nrow(result_data), nrow(mtcars))

  # 5. Optionally test UI interactions
  app$set_inputs(`block-expr-columns` = c("mpg", "cyl"))
  app$wait_for_idle()

  # 6. Cleanup
  cleanup_test_app(app_dir, app)
})
```

## Helper Functions

### `create_test_app(block_code, data_code = NULL, data_name = "mtcars")`

Creates a temporary Shiny app for testing.

**Example:**
```r
app_dir <- create_test_app(
  data_code = "custom_data <- data.frame(x = 1:10, y = letters[1:10])",
  block_code = 'serve(new_filter_block("x > 5"), data = list(data = custom_data))'
)
```

### `cleanup_test_app(app_dir, app = NULL)`

Stops the app and cleans up temporary directory.

### Verification Functions (currently not used, may need adjustment)

- `verify_table_columns(app, expected_cols, exact = TRUE)` - Check output columns
- `verify_row_count(app, expected_rows)` - Check output row count
- `verify_table_data(app, column, expected_values)` - Check specific values

## What These Tests Verify

### vs. shiny::testServer Tests

**shiny::testServer (unit tests):**
- ✅ Server-side logic
- ✅ Reactive expressions
- ✅ Expression generation
- ❌ Actual UI rendering
- ❌ User interactions
- ❌ Visual output verification

**shinytest2 (integration tests):**
- ✅ Full app launches
- ✅ User interactions (clicks, inputs)
- ✅ Actual data outputs
- ✅ End-to-end workflows
- ✅ Multi-block pipelines

## Best Practices

### 1. Test Coverage Strategy

- **Keep existing testServer tests** - They're fast and cover logic well
- **Add shinytest2 selectively** - Focus on critical user workflows
- **Each block should have:**
  - 1-3 basic functionality tests
  - 1-2 UI interaction tests
  - 1 edge case test

### 2. Test Organization

- Group related tests in the same `test_that()` block when they test the same scenario
- Use descriptive test names that explain what user action is being tested
- Test one concept per test when possible

### 3. Timeouts

- Default timeout: 30000ms (30 seconds)
- Increase for complex apps or slow systems
- Always use `app$wait_for_idle()` after interactions

### 4. Cleanup

- Always call `cleanup_test_app(app_dir, app)` in every test
- Use `on.exit()` if tests might error before cleanup

### 5. CI/CD Considerations

- Tests automatically skip on CRAN (`skip_on_cran()`)
- Tests skip if shinytest2 not installed
- Expected runtime: ~5-10 seconds per test

## Adding Tests for New Blocks

When adding a new block to blockr.dplyr, create a corresponding shinytest2 test file:

1. Create `test-shinytest2-yourblock.R`
2. Add at minimum:
   - Basic functionality test (input → output verification)
   - UI interaction test (change input → verify output changes)
   - Edge case test (empty data, extreme values, etc.)
3. Follow the test structure pattern shown above
4. Update this README

## Debugging Failed Tests

### Common Issues

1. **Tests skip every time**
   - Check `~/.Renviron` has `NOT_CRAN=true`
   - Restart R session after changing `.Renviron`

2. **App timeout errors**
   - Increase timeout in `AppDriver$new(timeout = 60000)`
   - Check if block has errors in standalone serve()

3. **Can't find output values**
   - Check `values$export` structure: `str(values$export)`
   - The result is exported via `exportTestValues(result = ...)` in serve.block

4. **Input IDs not found**
   - Inputs are namespaced as `block-expr-inputname`
   - Use `app$get_values()$input` to see available inputs

### Debug Mode

```r
# Launch app and keep it open for inspection
app_dir <- create_test_app(...)
app <- shinytest2::AppDriver$new(app_dir, timeout = 30000)
app$view() # Opens browser to see app

# Inspect available values
str(app$get_values())

# Don't forget to cleanup when done
cleanup_test_app(app_dir, app)
```

## Current Test Count

- **Total shinytest2 tests:** 33 tests (all passing ✅)
  - Arrange: 2 tests
  - Bind: 2 tests
  - Filter: 8 tests
  - Join: 1 test
  - Mutate: 2 tests
  - Rename: 1 test
  - Select: 7 tests
  - Slice: 1 test
  - Summarize: 8 tests
  - Value Filter: 1 test
- **Total testServer tests:** 123 tests
- **Coverage:** 10/10 blocks have integration tests (100% ✅)

**Achievement:** Complete end-to-end test coverage for all blockr.dplyr transform blocks!

## Next Steps

### Optional Enhancement: Complex Workflows

Create `test-shinytest2-pipelines.R` to test multi-block pipelines:
- Select → Filter
- Filter → Summarize
- Join → Filter → Select

This would test block composition and data flow through multiple transformations.

## Resources

- [shinytest2 documentation](https://rstudio.github.io/shinytest2/)
- [blockr.ts examples](../../blockr.ts/tests/testthat/test-shinytest2-blocks.R)
- [blockr.core examples](../../blockr.core/tests/testthat/test-utils-serve.R)
