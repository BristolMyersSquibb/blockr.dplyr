# Test enhanced filter column switching behavior
# Focus on debugging the issue where switching columns shows single value instead of range

test_that("Enhanced filter correctly parses Petal.Length > 5", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "Petal.Length > 5",
      get_cols = function() colnames(iris),
      get_data = function() iris
    ),
    {
      # Initial state check
      result <- session$returned
      expect_type(result, "list")
      expect_true("string" %in% names(result))

      # The filter string should be set
      filter_str <- result$string()
      expect_true(grepl("Petal.Length", filter_str))

      # Check the stored conditions
      conditions <- session$getReturned(r_conditions)
      expect_length(conditions, 1)

      # Verify the condition is in simple mode
      expect_equal(conditions[[1]]$mode, "simple")

      # Wait for UI to initialize
      session$flushReact()

      # Check the column dropdown value
      column_val <- input$condition_1_column
      cat("Initial column selected:", column_val, "\n")

      # Check the range slider value
      range_val <- input$condition_1_range
      cat("Initial range values:", paste(range_val, collapse = " to "), "\n")

      # Verify column is Petal.Length
      expect_equal(column_val, "Petal.Length")

      # Verify range starts at 5
      if (!is.null(range_val)) {
        expect_equal(range_val[1], 5)
        expect_true(range_val[2] > 6.5)  # Max should be around 6.9
      }
    }
  )
})

test_that("Column switching updates range correctly", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "Petal.Length > 5",
      get_cols = function() colnames(iris),
      get_data = function() iris
    ),
    {
      # Wait for initialization
      session$flushReact()

      # Check initial state
      initial_column <- input$condition_1_column
      initial_range <- input$condition_1_range
      cat("\nInitial state:\n")
      cat("  Column:", initial_column, "\n")
      cat("  Range:", paste(initial_range, collapse = " to "), "\n")

      # Switch to Sepal.Length
      session$setInputs(condition_1_column = "Sepal.Length")
      session$flushReact()

      # Check updated values
      new_column <- input$condition_1_column
      new_range <- input$condition_1_range
      cat("\nAfter switching to Sepal.Length:\n")
      cat("  Column:", new_column, "\n")
      cat("  Range:", paste(new_range, collapse = " to "), "\n")

      # Verify column changed
      expect_equal(new_column, "Sepal.Length")

      # Verify range updated (Sepal.Length range is approximately 4.3 to 7.9)
      if (!is.null(new_range)) {
        expect_length(new_range, 2)  # Should have both min and max
        expect_true(new_range[1] < 5)  # Min should be around 4.3
        expect_true(new_range[2] > 7)   # Max should be around 7.9
        cat("  Range is a proper interval:",
            new_range[1], "to", new_range[2], "\n")
      } else {
        cat("  WARNING: Range is NULL after column switch!\n")
      }

      # Switch to Petal.Width
      session$setInputs(condition_1_column = "Petal.Width")
      session$flushReact()

      pw_column <- input$condition_1_column
      pw_range <- input$condition_1_range
      cat("\nAfter switching to Petal.Width:\n")
      cat("  Column:", pw_column, "\n")
      cat("  Range:", paste(pw_range, collapse = " to "), "\n")

      # Verify Petal.Width range
      if (!is.null(pw_range)) {
        expect_length(pw_range, 2)
        # Petal.Width range is approximately 0.1 to 2.5
        expect_true(pw_range[1] < 0.5)
        expect_true(pw_range[2] > 2)
      }
    }
  )
})

test_that("Switching between numeric and character columns works", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() colnames(iris),
      get_data = function() iris
    ),
    {
      session$flushReact()

      # Start with a numeric column
      session$setInputs(condition_1_column = "Sepal.Length")
      session$flushReact()

      # Check we have a range slider
      sl_range <- input$condition_1_range
      cat("\nSepal.Length range:", paste(sl_range, collapse = " to "), "\n")
      expect_length(sl_range, 2)

      # Switch to Species (character column)
      session$setInputs(condition_1_column = "Species")
      session$flushReact()

      # Check we have multi-select values
      species_vals <- input$condition_1_values
      cat("Species values available:",
          if(is.null(species_vals)) "NULL" else paste(species_vals, collapse = ", "),
          "\n")

      # The range should not be relevant for character columns
      # but let's check what happens to it
      range_after_species <- input$condition_1_range
      cat("Range input after switching to Species:",
          paste(range_after_species, collapse = " to "), "\n")

      # Switch back to numeric
      session$setInputs(condition_1_column = "Petal.Length")
      session$flushReact()

      # Check range is properly set again
      pl_range <- input$condition_1_range
      cat("\nPetal.Length range after switching back:",
          paste(pl_range, collapse = " to "), "\n")
      expect_length(pl_range, 2)
    }
  )
})

test_that("Debug: Track all inputs during column switch", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "Petal.Length > 5",
      get_cols = function() colnames(iris),
      get_data = function() iris
    ),
    {
      session$flushReact()

      # Capture all initial inputs
      cat("\n=== Initial Input State ===\n")
      all_inputs <- reactiveValuesToList(input)
      relevant_inputs <- all_inputs[grep("condition_1", names(all_inputs))]
      for (name in names(relevant_inputs)) {
        val <- relevant_inputs[[name]]
        if (is.null(val)) {
          cat(name, ": NULL\n")
        } else if (length(val) > 1) {
          cat(name, ":", paste(val, collapse = ", "), "\n")
        } else {
          cat(name, ":", val, "\n")
        }
      }

      # Switch column
      cat("\n=== Switching to Sepal.Width ===\n")
      session$setInputs(condition_1_column = "Sepal.Width")
      session$flushReact()

      # Capture inputs after switch
      all_inputs_after <- reactiveValuesToList(input)
      relevant_after <- all_inputs_after[grep("condition_1", names(all_inputs_after))]
      for (name in names(relevant_after)) {
        val <- relevant_after[[name]]
        if (is.null(val)) {
          cat(name, ": NULL\n")
        } else if (length(val) > 1) {
          cat(name, ":", paste(val, collapse = ", "), "\n")
        } else {
          cat(name, ":", val, "\n")
        }
      }

      # Check the specific range values
      range_val <- input$condition_1_range
      if (!is.null(range_val)) {
        if (length(range_val) == 1) {
          cat("\nWARNING: Range has only one value:", range_val, "\n")
          cat("This might be the issue - expecting two values [min, max]\n")
        } else if (length(range_val) == 2) {
          cat("\nRange properly has two values: [",
              range_val[1], ",", range_val[2], "]\n")
          if (range_val[1] == range_val[2]) {
            cat("WARNING: Min and max are the same value!\n")
          }
        }
      }
    }
  )
})

test_that("Enhanced filter expression updates correctly on column switch", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() colnames(iris),
      get_data = function() iris
    ),
    {
      session$flushReact()

      # Set initial column and range
      session$setInputs(
        condition_1_column = "Petal.Length",
        condition_1_range = c(2, 5)
      )
      session$flushReact()

      # Check the generated expression
      result1 <- session$returned
      expr1 <- result1$string()
      cat("\nExpression with Petal.Length [2,5]:", expr1, "\n")
      expect_true(grepl("Petal.Length", expr1))

      # Switch column
      session$setInputs(condition_1_column = "Sepal.Width")
      session$flushReact()

      # Check expression updated
      result2 <- session$returned
      expr2 <- result2$string()
      cat("Expression after switch to Sepal.Width:", expr2, "\n")

      # The expression should now reference Sepal.Width
      # or be TRUE if the range covers the full extent
      if (expr2 != "TRUE") {
        expect_true(grepl("Sepal.Width", expr2))
        expect_false(grepl("Petal.Length", expr2))
      }
    }
  )
})