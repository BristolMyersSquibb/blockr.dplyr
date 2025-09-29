#!/usr/bin/env Rscript

# Direct script to test enhanced filter with shinytest2
# Run this to reproduce the column switching issue

library(shinytest2)
library(blockr.core)
library(blockr.dplyr)

cat("\n=== Testing Enhanced Filter Column Switching ===\n\n")

# Launch the app
cat("Launching app...\n")
app <- AppDriver$new(
  app_dir = "app-enhanced-filter",
  name = "enhanced-filter-test",
  height = 800,
  width = 1200,
  load_timeout = 30000
)

tryCatch({
  # Wait for app to load
  cat("Waiting for app to be ready...\n")
  app$wait_for_idle()

  # Take screenshot of initial state
  cat("Taking initial screenshot...\n")
  app$screenshot("initial-state.png")

  # Get all input values
  cat("\nGetting initial values...\n")
  values <- app$get_values()

  # Find the relevant inputs
  input_names <- names(values$input)
  cat("Found", length(input_names), "inputs\n")

  # Look for condition inputs
  condition_inputs <- input_names[grep("condition_1", input_names)]
  cat("\nCondition-related inputs:\n")
  for (name in condition_inputs) {
    val <- values$input[[name]]
    if (!is.null(val)) {
      if (length(val) > 1) {
        cat("  ", name, ":", paste(val, collapse = " to "), "\n")
      } else {
        cat("  ", name, ":", val, "\n")
      }
    }
  }

  # Find specific inputs
  column_input <- condition_inputs[grep("_column$", condition_inputs)][1]
  range_input <- condition_inputs[grep("_range$", condition_inputs)][1]

  cat("\nKey inputs identified:\n")
  cat("  Column input:", column_input, "\n")
  cat("  Range input:", range_input, "\n")

  if (!is.na(column_input)) {
    initial_column <- values$input[[column_input]]
    cat("\n*** Initial State ***\n")
    cat("  Column:", initial_column, "\n")

    if (!is.na(range_input)) {
      initial_range <- values$input[[range_input]]
      cat("  Range:", paste(initial_range, collapse = " to "), "\n")
    }

    # Switch to Sepal.Length
    cat("\n*** Switching to Sepal.Length ***\n")
    app$set_inputs(
      !!column_input := "Sepal.Length",
      wait_ = TRUE,
      timeout_ = 10000
    )

    # Wait and take screenshot
    app$wait_for_idle()
    Sys.sleep(2)  # Extra wait for UI update
    app$screenshot("after-switch.png")

    # Get new values
    new_values <- app$get_values()
    new_column <- new_values$input[[column_input]]
    cat("  New column:", new_column, "\n")

    if (!is.na(range_input)) {
      new_range <- new_values$input[[range_input]]
      cat("  New range:", paste(new_range, collapse = " to "), "\n")

      # Check if range updated properly
      if (!is.null(new_range) && length(new_range) == 2) {
        if (new_range[1] >= 5 && new_range[2] < 7) {
          cat("\n!!! BUG DETECTED !!!\n")
          cat("Range appears to be stuck at Petal.Length values (5 to ~6.9)\n")
          cat("Expected Sepal.Length range (~4.3 to ~7.9)\n")
        } else if (new_range[1] < 5 && new_range[2] > 7) {
          cat("\n✓ Range updated correctly to Sepal.Length values\n")
        }
      }
    }
  }

  cat("\nScreenshots saved in: tests/testthat/\n")

}, error = function(e) {
  cat("\nError occurred:", conditionMessage(e), "\n")
}, finally = {
  cat("\nStopping app...\n")
  app$stop()
})