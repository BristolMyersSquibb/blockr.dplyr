# Test enhanced filter column switching with shinytest2
# This test reproduces the issue where switching columns keeps old range values

library(shinytest2)

test_that("Enhanced filter updates range when switching columns", {
  # Skip if shinytest2 dependencies are not available
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  # Launch the app
  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "enhanced-filter-column-switch",
    height = 800,
    width = 1200,
    load_timeout = 20000,  # 20 seconds for app to load
    timeout = 10000         # 10 seconds for operations
  )

  # Ensure cleanup
  withr::defer(app$stop())

  # Wait for app to be ready
  app$wait_for_idle()

  # Take initial screenshot
  app$screenshot("01-initial-petal-length.png")

  # Get initial values
  initial_values <- app$get_values()

  # The input IDs in blockr have a specific pattern
  # We need to identify the correct namespace
  # Usually it's something like "block-1-module-condition_1_column"

  # First, let's explore what inputs are available
  cat("\n=== Initial Input Names ===\n")
  input_names <- names(initial_values$input)
  condition_inputs <- input_names[grep("condition_1", input_names)]
  for (name in condition_inputs) {
    val <- initial_values$input[[name]]
    if (!is.null(val)) {
      if (length(val) > 1) {
        cat(name, ":", paste(val, collapse = " to "), "\n")
      } else {
        cat(name, ":", val, "\n")
      }
    }
  }

  # Look for the column and range inputs
  column_input <- condition_inputs[grep("_column$", condition_inputs)][1]
  range_input <- condition_inputs[grep("_range$", condition_inputs)][1]

  cat("\nColumn input ID:", column_input, "\n")
  cat("Range input ID:", range_input, "\n")

  # Check initial column selection
  if (!is.null(column_input)) {
    initial_column <- initial_values$input[[column_input]]
    cat("Initial column selected:", initial_column, "\n")
    expect_equal(initial_column, "Petal.Length")
  }

  # Check initial range values
  if (!is.null(range_input)) {
    initial_range <- initial_values$input[[range_input]]
    cat("Initial range:", paste(initial_range, collapse = " to "), "\n")

    # Should be approximately 5 to 6.9 for Petal.Length > 5
    expect_equal(initial_range[1], 5)
    expect_true(initial_range[2] > 6.5 && initial_range[2] < 7)
  }

  # Now switch to Sepal.Length
  cat("\n=== Switching to Sepal.Length ===\n")

  if (!is.null(column_input)) {
    app$set_inputs(
      !!column_input := "Sepal.Length",
      wait_ = TRUE,  # Wait for reactive updates
      timeout_ = 5000
    )

    # Additional wait to ensure UI updates
    app$wait_for_idle()
    Sys.sleep(1)  # Give UI time to update

    # Take screenshot after switch
    app$screenshot("02-after-switch-sepal-length.png")

    # Get values after switch
    after_switch_values <- app$get_values()

    # Check column changed
    new_column <- after_switch_values$input[[column_input]]
    cat("Column after switch:", new_column, "\n")
    expect_equal(new_column, "Sepal.Length")

    # Check range updated
    if (!is.null(range_input)) {
      new_range <- after_switch_values$input[[range_input]]
      cat("Range after switch:", paste(new_range, collapse = " to "), "\n")

      # Sepal.Length range should be approximately 4.3 to 7.9
      # NOT the old Petal.Length range of 5 to 6.9

      # This is the key test - the range should update!
      if (!is.null(new_range) && length(new_range) == 2) {
        # Check it's not stuck at the old values
        if (new_range[1] == 5 && new_range[2] < 7) {
          cat("\n*** BUG CONFIRMED ***\n")
          cat("Range is stuck at Petal.Length values!\n")
          cat("Expected: ~4.3 to 7.9 (Sepal.Length range)\n")
          cat("Got: ", paste(new_range, collapse = " to "), " (Petal.Length range)\n")
        }

        # The test expectation - this should pass when fixed
        expect_true(
          new_range[1] < 5,  # Min should be less than 5 (around 4.3)
          info = "Minimum range should update to Sepal.Length min (~4.3)"
        )
        expect_true(
          new_range[2] > 7,  # Max should be greater than 7 (around 7.9)
          info = "Maximum range should update to Sepal.Length max (~7.9)"
        )
      } else {
        cat("Warning: Range is NULL or invalid after switch\n")
      }
    }
  }

  # Additional test: Switch to a character column
  cat("\n=== Switching to Species (character column) ===\n")

  if (!is.null(column_input)) {
    app$set_inputs(
      !!column_input := "Species",
      wait_ = TRUE,
      timeout_ = 5000
    )

    app$wait_for_idle()
    Sys.sleep(1)

    app$screenshot("03-species-column.png")

    species_values <- app$get_values()

    # Check for multi-select values input
    values_input <- condition_inputs[grep("_values$", condition_inputs)][1]
    if (!is.null(values_input)) {
      species_selections <- species_values$input[[values_input]]
      cat("Species values available:",
          if(is.null(species_selections)) "NULL" else paste(species_selections, collapse = ", "),
          "\n")
    }
  }

  # Final test: Switch back to numeric column
  cat("\n=== Switching back to Petal.Width ===\n")

  if (!is.null(column_input)) {
    app$set_inputs(
      !!column_input := "Petal.Width",
      wait_ = TRUE,
      timeout_ = 5000
    )

    app$wait_for_idle()
    Sys.sleep(1)

    app$screenshot("04-petal-width.png")

    final_values <- app$get_values()

    if (!is.null(range_input)) {
      final_range <- final_values$input[[range_input]]
      cat("Petal.Width range:", paste(final_range, collapse = " to "), "\n")

      # Petal.Width range should be approximately 0.1 to 2.5
      if (!is.null(final_range) && length(final_range) == 2) {
        expect_true(
          final_range[1] < 0.5,
          info = "Petal.Width min should be around 0.1"
        )
        expect_true(
          final_range[2] > 2,
          info = "Petal.Width max should be around 2.5"
        )
      }
    }
  }
})

test_that("Enhanced filter with wait_for_value approach", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "enhanced-filter-wait-value",
    height = 800,
    width = 1200,
    load_timeout = 20000
  )

  withr::defer(app$stop())

  app$wait_for_idle()

  # Get input IDs
  initial_values <- app$get_values()
  input_names <- names(initial_values$input)
  column_input <- input_names[grep("condition_1_column$", input_names)][1]
  range_input <- input_names[grep("condition_1_range$", input_names)][1]

  if (!is.null(column_input) && !is.null(range_input)) {
    # Set column and wait for range to update
    app$set_inputs(
      !!column_input := "Sepal.Length",
      wait_ = FALSE  # Don't wait for default behavior
    )

    # Wait specifically for the range input to change
    # This should detect when the range slider updates
    cat("\nWaiting for range slider to update...\n")

    tryCatch({
      app$wait_for_value(
        input = range_input,
        ignore = list(initial_values$input[[range_input]]),
        timeout = 10000,
        interval = 500
      )
      cat("Range slider updated!\n")
    }, error = function(e) {
      cat("Range slider did not update within timeout\n")
      cat("Error:", conditionMessage(e), "\n")
    })

    # Get final values
    final_values <- app$get_values()
    final_range <- final_values$input[[range_input]]

    cat("Final range values:", paste(final_range, collapse = " to "), "\n")

    # Verify the range updated correctly
    if (!is.null(final_range) && length(final_range) == 2) {
      expect_true(final_range[1] < 5, "Range min should update")
      expect_true(final_range[2] > 7, "Range max should update")
    }
  }
})