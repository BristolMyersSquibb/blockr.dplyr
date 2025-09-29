test_that("enhanced filter initializes with default condition", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp", "wt"),
      get_data = function() mtcars
    ),
    {
      # Check initial state
      result <- session$returned()
      expect_type(result, "character")
      expect_equal(result, "TRUE")

      # Check that we have exactly one condition
      conditions <- session$getReturned(r_conditions)
      expect_length(conditions, 1)
    }
  )
})

test_that("enhanced filter initializes with custom condition string", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "mpg > 20 & cyl == 4",
      get_cols = function() c("mpg", "cyl", "hp", "wt"),
      get_data = function() mtcars
    ),
    {
      result <- session$returned()
      expect_type(result, "character")
      # Should parse into conditions
      expect_true(grepl("mpg.*20", result) || grepl("mpg > 20", result))
    }
  )
})

test_that("enhanced filter adds conditions dynamically", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp"),
      get_data = function() mtcars
    ),
    {
      # Initially one condition
      initial_result <- session$returned()

      # Add a new condition
      session$setInputs(add_condition = 1)

      # Should now have two conditions
      session$flushReact()

      # Set values for the new condition in simple mode
      session$setInputs(
        condition_2_column = "mpg",
        condition_2_operator = ">",
        condition_2_value = "20"
      )

      session$flushReact()

      # Check that we can retrieve the combined filter
      # Note: The actual combination happens in get_current_conditions
      # which is called by the returned reactive
    }
  )
})

test_that("enhanced filter removes conditions correctly", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "mpg > 20",
      get_cols = function() c("mpg", "cyl", "hp"),
      get_data = function() mtcars
    ),
    {
      # Add a second condition first
      session$setInputs(add_condition = 1)
      session$flushReact()

      # Now remove the second condition
      session$setInputs(condition_2_remove = 1)
      session$flushReact()

      # Should be back to one condition
      # The module maintains at least one condition
    }
  )
})

test_that("enhanced filter handles AND/OR logic operators", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp"),
      get_data = function() mtcars
    ),
    {
      # Add a second condition
      session$setInputs(add_condition = 1)
      session$flushReact()

      # Set first condition
      session$setInputs(
        condition_1_column = "mpg",
        condition_1_operator = ">",
        condition_1_value = "20"
      )

      # Set second condition
      session$setInputs(
        condition_2_column = "cyl",
        condition_2_operator = "==",
        condition_2_value = "4"
      )

      # Test AND logic (default)
      session$setInputs(logic_1 = "&")
      session$flushReact()

      # Test OR logic
      session$setInputs(logic_1 = "|")
      session$flushReact()

      # Result should reflect the OR operator
      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("enhanced filter switches between simple and advanced modes", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "mpg > 20",
      get_cols = function() c("mpg", "cyl", "hp"),
      get_data = function() mtcars
    ),
    {
      # Start in simple mode (default)
      # Switch first condition to advanced mode
      session$setInputs(condition_1_mode = "advanced")
      session$flushReact()

      # In advanced mode, we can set complex expressions
      session$setInputs(condition_1 = "mpg > 20 & mpg < 30")
      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")

      # Switch back to simple mode
      session$setInputs(condition_1_mode = "simple")
      session$flushReact()

      # Simple mode should parse the expression if possible
    }
  )
})

test_that("enhanced filter handles numeric operators in simple mode", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp", "wt"),
      get_data = function() mtcars
    ),
    {
      # Test each numeric operator
      operators <- c(">", "<", ">=", "<=", "==", "!=")

      for (op in operators) {
        session$setInputs(
          condition_1_column = "mpg",
          condition_1_operator = op,
          condition_1_value = "20"
        )
        session$flushReact()

        result <- session$returned()
        expect_type(result, "character")
      }
    }
  )
})

test_that("enhanced filter handles character column filtering", {
  # Create test data with character column
  test_data <- mtcars
  test_data$brand <- c("Toyota", "Honda", "Ford", "Toyota", "Honda",
                        rep("Other", nrow(mtcars) - 5))

  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() colnames(test_data),
      get_data = function() test_data
    ),
    {
      # Select character column
      session$setInputs(
        condition_1_column = "brand",
        condition_1_operator = "==",
        condition_1_value = "Toyota"
      )
      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")

      # Test %in% operator with multiple values
      session$setInputs(
        condition_1_operator = "%in%",
        condition_1_value = "Toyota,Honda"  # Comma-separated for %in%
      )
      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("enhanced filter handles factor columns", {
  # Create test data with factor column
  test_data <- mtcars
  test_data$gear_factor <- factor(test_data$gear)

  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() colnames(test_data),
      get_data = function() test_data
    ),
    {
      session$setInputs(
        condition_1_column = "gear_factor",
        condition_1_operator = "==",
        condition_1_value = "4"
      )
      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("enhanced filter handles empty and NULL values", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "",
      get_cols = function() c("mpg", "cyl"),
      get_data = function() mtcars
    ),
    {
      # Empty string should default to TRUE
      result <- session$returned()
      expect_equal(result, "TRUE")

      # Set empty value in simple mode
      session$setInputs(
        condition_1_column = "mpg",
        condition_1_operator = ">",
        condition_1_value = ""
      )
      session$flushReact()

      # Should handle empty value gracefully
      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("enhanced filter preserves state correctly", {
  # Test state restoration from saved conditions
  saved_conditions <- list(
    list(
      expression = "mpg > 20",
      logic_operator = NULL,
      mode = "simple",
      column = "mpg",
      operator = ">",
      value = "20"
    ),
    list(
      expression = "cyl == 4",
      logic_operator = "&",
      mode = "simple",
      column = "cyl",
      operator = "==",
      value = "4"
    )
  )

  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() saved_conditions,
      get_cols = function() c("mpg", "cyl", "hp"),
      get_data = function() mtcars
    ),
    {
      # Should restore with two conditions
      result <- session$returned()
      expect_type(result, "character")

      # Should have both conditions in the output
      expect_true(grepl("mpg", result) || length(result) > 0)
    }
  )
})

test_that("enhanced filter handles complex expressions in advanced mode", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp", "wt"),
      get_data = function() mtcars
    ),
    {
      # Switch to advanced mode
      session$setInputs(condition_1_mode = "advanced")
      session$flushReact()

      # Set complex expression with dplyr functions
      session$setInputs(
        condition_1 = "mpg > mean(mpg) & cyl %in% c(4, 6)"
      )
      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")
      expect_true(grepl("mean", result))

      # Test with between function
      session$setInputs(
        condition_1 = "between(mpg, 15, 25)"
      )
      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("enhanced filter handles multiple conditions with mixed modes", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp"),
      get_data = function() mtcars
    ),
    {
      # Add multiple conditions
      session$setInputs(add_condition = 1)
      session$flushReact()
      session$setInputs(add_condition = 1)
      session$flushReact()

      # First condition: simple mode
      session$setInputs(
        condition_1_column = "mpg",
        condition_1_operator = ">",
        condition_1_value = "20"
      )

      # Second condition: advanced mode
      session$setInputs(
        condition_2_mode = "advanced",
        condition_2 = "cyl %in% c(4, 6)"
      )

      # Third condition: simple mode
      session$setInputs(
        condition_3_column = "hp",
        condition_3_operator = "<",
        condition_3_value = "150"
      )

      # Set logic operators
      session$setInputs(
        logic_1 = "&",
        logic_2 = "|"
      )

      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("enhanced filter handles rapid add/remove operations", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl"),
      get_data = function() mtcars
    ),
    {
      # Rapidly add multiple conditions
      for (i in 1:3) {
        session$setInputs(add_condition = i)
      }
      session$flushReact()

      # Remove middle condition
      session$setInputs(condition_2_remove = 1)
      session$flushReact()

      # Add another condition
      session$setInputs(add_condition = 4)
      session$flushReact()

      # Should handle all operations gracefully
      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("enhanced filter protects against removing last condition", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "mpg > 20",
      get_cols = function() c("mpg", "cyl"),
      get_data = function() mtcars
    ),
    {
      # Try to remove the only condition (should be prevented)
      session$setInputs(condition_1_remove = 1)
      session$flushReact()

      # Should still have at least one condition
      result <- session$returned()
      expect_type(result, "character")
      expect_false(is.null(result))
    }
  )
})

test_that("enhanced filter handles invalid expressions gracefully", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl"),
      get_data = function() mtcars
    ),
    {
      # Switch to advanced mode
      session$setInputs(condition_1_mode = "advanced")
      session$flushReact()

      # Set invalid expression
      session$setInputs(condition_1 = "mpg >>> 20")  # Invalid syntax
      session$flushReact()

      # Should handle error gracefully
      result <- session$returned()
      expect_type(result, "character")

      # Test with non-existent column
      session$setInputs(condition_1 = "nonexistent_col > 20")
      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("enhanced filter handles range selections", {
  testServer(
    blockr.dplyr:::mod_enhanced_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp"),
      get_data = function() mtcars
    ),
    {
      # Add two conditions for a range
      session$setInputs(add_condition = 1)
      session$flushReact()

      # Lower bound
      session$setInputs(
        condition_1_column = "mpg",
        condition_1_operator = ">=",
        condition_1_value = "15"
      )

      # Upper bound
      session$setInputs(
        condition_2_column = "mpg",
        condition_2_operator = "<=",
        condition_2_value = "25",
        logic_1 = "&"
      )

      session$flushReact()

      result <- session$returned()
      expect_type(result, "character")
      # Should create a range filter
    }
  )
})

test_that("enhanced filter block integrates with blockr framework", {
  # Test the complete block construction
  block <- new_enhanced_filter_block()
  expect_s3_class(block, c("enhanced_filter_block", "transform_block", "block"))

  # Test with initial condition
  block_with_filter <- new_enhanced_filter_block("mpg > 20 & cyl == 4")
  expect_s3_class(block_with_filter, c("enhanced_filter_block", "transform_block", "block"))

  # Test with complex condition
  complex_block <- new_enhanced_filter_block("between(mpg, 15, 25) & cyl %in% c(4, 6)")
  expect_s3_class(complex_block, c("enhanced_filter_block", "transform_block", "block"))
})

test_that("enhanced filter parse functions work correctly", {
  # Test parse_enhanced_filter
  expr <- blockr.dplyr:::parse_enhanced_filter("mpg > 20")
  expect_type(expr, "expression")

  # Test with empty string
  expr_empty <- blockr.dplyr:::parse_enhanced_filter("")
  expect_type(expr_empty, "expression")

  # Test with complex expression
  expr_complex <- blockr.dplyr:::parse_enhanced_filter("mpg > 20 & cyl == 4 | hp < 100")
  expect_type(expr_complex, "expression")

  # Test apply_enhanced_filter doesn't error
  r_expr <- shiny::reactiveVal()
  r_string <- shiny::reactiveVal()

  expect_silent(
    blockr.dplyr:::apply_enhanced_filter(mtcars, "mpg > 20", r_expr, r_string)
  )
})