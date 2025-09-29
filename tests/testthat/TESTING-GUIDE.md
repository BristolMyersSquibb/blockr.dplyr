# Testing Guide for blockr.dplyr Components

## Overview

This guide documents the testing approach for blockr.dplyr components, with a focus on using `shiny::testServer` for unit tests and `shinytest2` for integration testing of UI interactions.

## Testing Frameworks

### 1. shiny::testServer (Unit Testing)

Best for testing server logic without UI interaction.

**Example: Basic Module Testing**
```r
test_that("enhanced filter initializes correctly", {
  testServer(
    mod_enhanced_filter_server,
    args = list(
      get_value = function() "Petal.Length > 5",
      get_cols = function() colnames(iris),
      get_data = function() iris
    ),
    {
      # Check returned values
      result <- session$returned
      expect_type(result, "list")

      # Access reactive values
      conditions <- session$getReturned(r_conditions)
      expect_length(conditions, 1)

      # Simulate input changes
      session$setInputs(
        condition_1_column = "Sepal.Length"
      )
      session$flushReact()

      # Check inputs (may be NULL in testServer context)
      column_val <- input$condition_1_column
      range_val <- input$condition_1_range
    }
  )
})
```

**Limitations of testServer:**
- Input widgets may not initialize properly
- Dynamic UI elements often remain NULL
- No JavaScript execution
- No actual browser rendering

### 2. shinytest2 (Integration Testing)

Best for testing actual UI interactions and timing-dependent behavior.

## Setting Up shinytest2 Tests

### Step 1: Create Test App

Create a minimal Shiny app in `tests/testthat/app-{component-name}/app.R`:

```r
# app-enhanced-filter/app.R
library(blockr.core)
library(blockr.dplyr)

blockr.core::serve(
  new_enhanced_filter_block("Petal.Length > 5"),
  data = list(data = iris)
)
```

### Step 2: Write shinytest2 Test

```r
library(shinytest2)

test_that("Component updates correctly on interaction", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  # Launch app with proper timeouts
  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "test-name",
    height = 800,
    width = 1200,
    load_timeout = 20000,  # 20 seconds for app load
    timeout = 10000        # 10 seconds for operations
  )

  # Ensure cleanup
  withr::defer(app$stop())

  # Wait for app to stabilize
  app$wait_for_idle()

  # Get input values
  values <- app$get_values()

  # Find specific inputs (blockr uses namespaced IDs)
  input_names <- names(values$input)
  column_input <- input_names[grep("condition_1_column$", input_names)][1]
  range_input <- input_names[grep("condition_1_range$", input_names)][1]

  # Set inputs and wait
  app$set_inputs(
    !!column_input := "Sepal.Length",
    wait_ = TRUE,
    timeout_ = 5000
  )

  # Additional wait for UI updates
  app$wait_for_idle()
  Sys.sleep(1)  # Sometimes needed for complex UI

  # Capture screenshots for debugging
  app$screenshot("after-change.png")

  # Verify changes
  new_values <- app$get_values()
  expect_equal(new_values$input[[column_input]], "Sepal.Length")
})
```

## Best Practices

### Timing and Synchronization

1. **Use `wait_for_idle()`** after input changes
   ```r
   app$set_inputs(input_id = "value")
   app$wait_for_idle()
   ```

2. **Use `wait_for_value()`** for specific updates
   ```r
   app$wait_for_value(
     input = "range_slider",
     ignore = list(old_value),
     timeout = 10000,
     interval = 500
   )
   ```

3. **Add explicit delays when needed**
   ```r
   Sys.sleep(1)  # Give UI time to render
   ```

### Configuration

1. **Set appropriate timeouts**
   ```r
   options(shinytest2.load_timeout = 30000)  # Global setting

   app <- AppDriver$new(
     load_timeout = 30 * 1000,  # Per-app setting
     timeout = 10 * 1000
   )
   ```

2. **Handle CRAN checks**
   ```r
   # Run tests with: NOT_CRAN=true Rscript test-script.R
   skip_if_not_installed("shinytest2")
   ```

3. **Use wait_ parameter appropriately**
   ```r
   # Wait for reactive flush (default)
   app$set_inputs(input = "value", wait_ = TRUE)

   # Don't wait (when no output change expected)
   app$set_inputs(input = "value", wait_ = FALSE)

   # Custom timeout for slow operations
   app$set_inputs(input = "value", timeout_ = 30000)
   ```

## Common Issues and Solutions

### Issue: Input values are NULL in testServer

**Problem:** Dynamic UI elements don't initialize properly in `testServer` context.

**Solution:** Use `shinytest2` for testing UI interactions:
```r
# Instead of testServer, use AppDriver
app <- AppDriver$new(app_dir = "app-directory")
values <- app$get_values()
```

### Issue: Range slider doesn't update when switching columns

**Problem:** When switching from one numeric column to another, the range slider retains old values.

**Detection:**
```r
# Initial state: Petal.Length with range [5, 6.9]
initial_range <- app$get_values()$input[[range_input]]

# Switch column
app$set_inputs(!!column_input := "Sepal.Length")
app$wait_for_idle()

# Check if range updated
new_range <- app$get_values()$input[[range_input]]
# Bug: new_range may still be [5, 6.9] instead of [4.3, 7.9]
```

**Testing approach:**
1. Capture initial state
2. Change column selection
3. Wait for updates with `wait_for_idle()`
4. Verify new range matches new column's data range
5. Take screenshots for visual debugging

### Issue: Timing-dependent failures

**Problem:** Tests fail intermittently due to async operations.

**Solutions:**
1. Increase timeouts
2. Use explicit waits
3. Poll for specific conditions
4. Add retry logic

```r
# Retry pattern
for (i in 1:3) {
  tryCatch({
    app$set_inputs(input = "value")
    app$wait_for_idle()
    break
  }, error = function(e) {
    if (i == 3) stop(e)
    Sys.sleep(2)
  })
}
```

## Directory Structure

```
tests/testthat/
├── test-{component}.R           # testServer unit tests
├── test-{component}-shinytest2.R # shinytest2 integration tests
├── app-{component}/              # Test apps for shinytest2
│   └── app.R
├── run-shinytest2.R             # Direct test runners for debugging
└── TESTING-GUIDE.md             # This file
```

## Running Tests

### Run all tests
```bash
Rscript -e "testthat::test_local()"
```

### Run specific test file
```bash
Rscript -e "testthat::test_file('tests/testthat/test-enhanced-filter-shinytest2.R')"
```

### Run with debugging output
```bash
NOT_CRAN=true Rscript tests/testthat/run-shinytest2.R
```

## Debugging Tips

1. **Use screenshots liberally**
   ```r
   app$screenshot("step-1.png")
   # ... perform action ...
   app$screenshot("step-2.png")
   ```

2. **Print input values for debugging**
   ```r
   values <- app$get_values()
   cat("Inputs:\n")
   str(values$input)
   ```

3. **Check for namespaced IDs**
   ```r
   # blockr uses namespaced IDs like "block-1-module-input_id"
   input_names <- names(app$get_values()$input)
   relevant_inputs <- input_names[grep("pattern", input_names)]
   ```

4. **Test minimal examples first**
   ```r
   # Start with simplest case
   app <- AppDriver$new(app_dir = "minimal-app")
   # Then add complexity
   ```

## Continuous Improvement

- Document new testing patterns as they're discovered
- Share reusable test helpers
- Maintain example test apps for common scenarios
- Update this guide with new findings

## References

- [shinytest2 documentation](https://rstudio.github.io/shinytest2/)
- [testthat documentation](https://testthat.r-lib.org/)
- [Shiny testing guide](https://shiny.rstudio.com/articles/testing.html)