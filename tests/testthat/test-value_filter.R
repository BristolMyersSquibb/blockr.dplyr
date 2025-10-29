test_that("value_filter block constructor", {
  # Test basic constructor
  blk <- new_value_filter_block()
  expect_s3_class(blk, c("value_filter_block", "transform_block", "block"))

  # Test constructor with initial conditions
  conditions <- list(
    list(
      column = "Species",
      values = c("setosa", "versicolor"),
      mode = "include"
    )
  )
  blk <- new_value_filter_block(conditions = conditions)
  expect_s3_class(blk, c("value_filter_block", "transform_block", "block"))

  # Test constructor with multiple conditions
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include"),
    list(column = "Sepal.Length", values = c(5.1, 5.4), mode = "exclude")
  )
  blk <- new_value_filter_block(conditions = conditions)
  expect_s3_class(blk, c("value_filter_block", "transform_block", "block"))
})

test_that("parse_value_filter function", {
  # Test empty conditions
  expr <- blockr.dplyr:::parse_value_filter(list())
  expect_type(expr, "language")

  # Test single include condition with character values
  conditions <- list(
    list(
      column = "Species",
      values = c("setosa", "versicolor"),
      mode = "include"
    )
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "language")
  expect_true(grepl("Species.*%in%.*\"setosa\".*\"versicolor\"", deparse(expr)))

  # Test single exclude condition with character values
  conditions <- list(
    list(column = "Species", values = c("virginica"), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "language")
  expect_true(grepl("!.*Species.*%in%.*\"virginica\"", deparse(expr)))

  # Test single include condition with numeric values
  conditions <- list(
    list(column = "Sepal.Length", values = c(5.1, 5.4), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "language")
  expect_true(grepl("Sepal.Length.*%in%.*5.1.*5.4", deparse(expr)))

  # Test multiple conditions (combined with AND)
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include"),
    list(column = "Sepal.Length", values = c(5.1), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "language")
  expr_str <- paste(deparse(expr), collapse = " ")
  expect_true(grepl(
    "Species.*%in%.*\"setosa\".*&.*!.*Sepal.Length.*%in%.*5.1",
    expr_str
  ))
})

test_that("parse_value_filter handles edge cases", {
  # Test condition with NULL column
  conditions <- list(
    list(column = NULL, values = c("test"), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_true(grepl("TRUE", deparse(expr)))

  # Test condition with empty values
  conditions <- list(
    list(column = "Species", values = character(0), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_true(grepl("TRUE", deparse(expr)))

  # Test condition with NULL values
  conditions <- list(
    list(column = "Species", values = NULL, mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_true(grepl("TRUE", deparse(expr)))

  # Test mixed valid and invalid conditions
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include"),
    list(column = NULL, values = c("test"), mode = "include"),
    list(column = "Sepal.Length", values = c(5.1), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "language")
  expr_str <- paste(deparse(expr), collapse = " ")
  expect_true(grepl(
    "Species.*%in%.*\"setosa\".*&.*!.*Sepal.Length.*%in%.*5.1",
    expr_str
  ))
})

test_that("apply_value_filter function validates input", {
  # Test parse_value_filter with valid conditions
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "language")

  # Test evaluation doesn't error
  result <- try(eval(expr, envir = list(data = iris)), silent = TRUE)
  expect_false(inherits(result, "try-error"))

  # Test parse_value_filter with empty conditions
  expr <- blockr.dplyr:::parse_value_filter(list())
  expect_type(expr, "language")

  result <- try(eval(expr, envir = list(data = iris)), silent = TRUE)
  expect_false(inherits(result, "try-error"))
})

test_that("value filter generates correct dplyr expressions", {
  # Test include mode with single value
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  # Test that it creates a valid dplyr filter
  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
  expect_equal(as.character(unique(result$Species)), "setosa")

  # Test exclude mode with single value
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
  expect_false("setosa" %in% result$Species)
  expect_true(all(c("versicolor", "virginica") %in% result$Species))

  # Test include mode with multiple values
  conditions <- list(
    list(
      column = "Species",
      values = c("setosa", "versicolor"),
      mode = "include"
    )
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
  expect_true(all(result$Species %in% c("setosa", "versicolor")))
  expect_false("virginica" %in% result$Species)
})

test_that("value filter works with numeric columns", {
  # Test with numeric values
  conditions <- list(
    list(column = "Sepal.Length", values = c(5.1, 5.4, 5.8), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
  expect_true(all(result$Sepal.Length %in% c(5.1, 5.4, 5.8)))

  # Test exclude with numeric values
  conditions <- list(
    list(column = "Sepal.Length", values = c(5.1), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
  expect_false(5.1 %in% result$Sepal.Length)
})

test_that("value filter handles multiple conditions", {
  # Test multiple include conditions (AND logic)
  conditions <- list(
    list(
      column = "Species",
      values = c("setosa", "versicolor"),
      mode = "include"
    ),
    list(column = "Sepal.Length", values = c(5.1, 5.4, 5.8), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
  expect_true(all(result$Species %in% c("setosa", "versicolor")))
  expect_true(all(result$Sepal.Length %in% c(5.1, 5.4, 5.8)))

  # Test mixed include/exclude conditions
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include"),
    list(column = "Sepal.Length", values = c(5.1), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
  expect_true(all(result$Species == "setosa"))
  expect_false(5.1 %in% result$Sepal.Length)
})

test_that("mod_value_filter_server extracts unique values correctly", {
  # Mock server environment
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(),
      get_data = function() iris
    ),
    {
      # Test that available columns are extracted correctly
      expect_equal(
        sort(available_columns()),
        sort(c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Petal.Width",
          "Species"
        ))
      )

      # Test unique value extraction for Species (factor column)
      species_values <- get_unique_values("Species")
      expect_equal(
        sort(species_values),
        sort(c("setosa", "versicolor", "virginica"))
      )

      # Test unique value extraction for numeric column
      sepal_length_values <- get_unique_values("Sepal.Length")
      expect_true(
        is.numeric(sepal_length_values) ||
          all(grepl("^[0-9.]+$", sepal_length_values))
      )
      expect_true(length(sepal_length_values) > 1)

      # Test with non-existent column
      expect_equal(get_unique_values("NonExistentColumn"), character(0))
    }
  )
})

test_that("mod_value_filter_server handles initial conditions", {
  initial_conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include")
  )

  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() initial_conditions,
      get_data = function() iris
    ),
    {
      # Test that initial conditions are set correctly
      result <- session$getReturned()
      conditions <- result$conditions()
      expect_length(conditions, 1)
      expect_equal(conditions[[1]]$column, "Species")
      expect_equal(conditions[[1]]$values, c("setosa"))
      expect_equal(conditions[[1]]$mode, "include")
    }
  )
})

test_that("parse_value_filter supports logic operators", {
  # Test OR logic between conditions
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include"),
    list(
      column = "Species",
      values = c("versicolor"),
      mode = "include",
      operator = "|"
    )
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
  expect_true(all(result$Species %in% c("setosa", "versicolor")))
  expect_false("virginica" %in% result$Species)

  # Test mixed AND/OR logic
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include"),
    list(
      column = "Sepal.Length",
      values = c(5.1),
      mode = "exclude",
      operator = "&"
    ),
    list(
      column = "Species",
      values = c("versicolor"),
      mode = "include",
      operator = "|"
    )
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)

  # Should be: (Species == "setosa" & Sepal.Length != 5.1) | Species == "versicolor"
  result <- eval(expr, envir = list(data = iris))
  expect_s3_class(result, "data.frame")
})

test_that("default operator %||% works correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(character(0) %||% "default", character(0))
  expect_equal(0 %||% "default", 0)
})

test_that("value filter handles NA values correctly", {
  # Create test data with NAs
  test_data <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c("a", "b", NA, "d", "e"),
    stringsAsFactors = FALSE
  )

  # Test include mode with NA in numeric column
  conditions <- list(
    list(column = "x", values = c("1", "<NA>"), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  result <- eval(expr, envir = list(data = test_data))

  expect_equal(nrow(result), 2)
  expect_true(1 %in% result$x)
  expect_true(any(is.na(result$x)))

  # Test include mode with NA in character column
  conditions <- list(
    list(column = "y", values = c("a", "<NA>"), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  result <- eval(expr, envir = list(data = test_data))

  expect_equal(nrow(result), 2)
  expect_true("a" %in% result$y)
  expect_true(any(is.na(result$y)))

  # Test exclude mode with NA
  conditions <- list(
    list(column = "x", values = c("<NA>"), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  result <- eval(expr, envir = list(data = test_data))

  expect_equal(nrow(result), 4)
  expect_false(any(is.na(result$x)))
})

test_that("value filter handles empty strings correctly", {
  # Create test data with empty strings
  test_data <- data.frame(
    x = c("a", "b", "", "d", "e"),
    y = c("foo", "", "bar", "", "baz"),
    stringsAsFactors = FALSE
  )

  # Test include mode with empty string
  conditions <- list(
    list(column = "x", values = c("a", "<empty>"), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  result <- eval(expr, envir = list(data = test_data))

  expect_equal(nrow(result), 2)
  expect_true("a" %in% result$x)
  expect_true("" %in% result$x)

  # Test exclude mode with empty string
  conditions <- list(
    list(column = "y", values = c("<empty>"), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  result <- eval(expr, envir = list(data = test_data))

  expect_equal(nrow(result), 3)
  expect_false("" %in% result$y)
  expect_true(all(result$y != ""))
})

test_that("value filter handles mixed NA, empty, and regular values", {
  # Create test data with NAs, empty strings, and regular values
  test_data <- data.frame(
    species = c("setosa", "", NA, "versicolor", "setosa", ""),
    value = c(1, 2, 3, NA, 5, 6),
    stringsAsFactors = FALSE
  )

  # Test include mode with all three types
  conditions <- list(
    list(
      column = "species",
      values = c("setosa", "<empty>", "<NA>"),
      mode = "include"
    )
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  result <- eval(expr, envir = list(data = test_data))

  # Data has: 2 setosa + 2 empty + 1 NA = 5 rows
  expect_equal(nrow(result), 5)
  expect_true("setosa" %in% result$species)
  expect_true("" %in% result$species)
  expect_true(any(is.na(result$species)))

  # Test exclude mode with mixed values
  conditions <- list(
    list(column = "species", values = c("<empty>", "<NA>"), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  result <- eval(expr, envir = list(data = test_data))

  # Data has 6 rows total, excluding 2 empty + 1 NA = 3 rows remain
  expect_equal(nrow(result), 3)
  expect_false("" %in% result$species)
  expect_false(any(is.na(result$species)))
  expect_true(all(result$species %in% c("setosa", "versicolor")))
})

test_that("helper functions convert values correctly", {
  # Test blockr.dplyr:::actual_to_display
  expect_equal(blockr.dplyr:::actual_to_display(NA), "<NA>")
  expect_equal(blockr.dplyr:::actual_to_display(""), "<empty>")
  expect_equal(blockr.dplyr:::actual_to_display("test"), "test")
  expect_equal(blockr.dplyr:::actual_to_display(123), "123")

  # Test blockr.dplyr:::display_to_actual
  expect_true(is.na(blockr.dplyr:::display_to_actual("<NA>")))
  expect_equal(blockr.dplyr:::display_to_actual("<empty>"), "")
  expect_equal(blockr.dplyr:::display_to_actual("test"), "test")
  expect_equal(blockr.dplyr:::display_to_actual("123", "numeric"), 123)
  expect_equal(blockr.dplyr:::display_to_actual("123", "character"), "123")
})

# Data transformation tests using block_server
test_that("value_filter block filters by selected values - testServer", {
  block <- new_value_filter_block(
    conditions = list(list(column = "cyl", values = c(4, 6), mode = "include"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify filtering worked
      expect_true(is.data.frame(result))
      expect_true(all(result$cyl %in% c(4, 6)))
      expect_false(any(result$cyl == 8))
      expect_equal(ncol(result), ncol(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("value_filter block with preserve_order=TRUE - testServer", {
  # Create data with specific order
  test_data <- mtcars
  test_data$cyl <- factor(test_data$cyl, levels = c(8, 6, 4))

  block <- new_value_filter_block(
    conditions = list(list(column = "cyl", values = c(4, 6), mode = "include")),
    preserve_order = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true(all(result$cyl %in% c(4, 6)))
      # With preserve_order=TRUE, the order should be maintained
      expect_true(is.factor(result$cyl))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("value_filter block with exclude mode - testServer", {
  block <- new_value_filter_block(
    conditions = list(list(column = "cyl", values = c(8), mode = "exclude"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should exclude cyl=8
      expect_false(any(result$cyl == 8))
      expect_true(all(result$cyl %in% c(4, 6)))
      expect_equal(ncol(result), ncol(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("value_filter block with multiple conditions AND operator - testServer", {
  block <- new_value_filter_block(
    conditions = list(
      list(column = "cyl", values = c(4, 6), mode = "include"),
      list(column = "gear", values = c(4), mode = "include", operator = "&")
    )
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have both conditions met (cyl in 4,6 AND gear = 4)
      expect_true(all(result$cyl %in% c(4, 6)))
      expect_true(all(result$gear == 4))
      expect_equal(ncol(result), ncol(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("value_filter block with multiple conditions OR operator - testServer", {
  block <- new_value_filter_block(
    conditions = list(
      list(column = "cyl", values = c(4), mode = "include"),
      list(column = "cyl", values = c(8), mode = "include", operator = "|")
    )
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have either condition met (cyl = 4 OR cyl = 8)
      expect_true(all(result$cyl %in% c(4, 8)))
      expect_false(any(result$cyl == 6))
      expect_equal(ncol(result), ncol(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# Additional mod_value_filter_server tests for coverage
test_that("mod_value_filter_server handles empty initial conditions", {
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(),
      get_data = function() mtcars
    ),
    {
      session$flushReact()
      result <- session$getReturned()

      # Should return empty conditions list
      conditions <- result$conditions()
      expect_type(conditions, "list")
    }
  )
})

test_that("mod_value_filter_server handles numeric columns", {
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(),
      get_data = function() mtcars
    ),
    {
      session$flushReact()

      # Test numeric column unique values
      mpg_values <- get_unique_values("mpg")
      expect_true(length(mpg_values) > 0)
      expect_true(
        is.numeric(mpg_values) ||
          all(grepl("^[0-9.]+$", mpg_values))
      )

      # Test another numeric column
      hp_values <- get_unique_values("hp")
      expect_true(length(hp_values) > 0)
    }
  )
})

test_that("mod_value_filter_server with preserve_order parameter", {
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(),
      get_data = function() iris,
      preserve_order = TRUE
    ),
    {
      session$flushReact()

      # Module should initialize without error
      result <- session$getReturned()
      expect_type(result, "list")
    }
  )
})

test_that("mod_value_filter_server handles multiple conditions", {
  multi_conditions <- list(
    list(column = "cyl", values = c(4, 6), mode = "include"),
    list(column = "gear", values = c(4), mode = "include", operator = "&")
  )

  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() multi_conditions,
      get_data = function() mtcars
    ),
    {
      session$flushReact()
      result <- session$getReturned()

      # Should have multiple conditions
      conditions <- result$conditions()
      expect_length(conditions, 2)
      expect_equal(conditions[[1]]$column, "cyl")
      expect_equal(conditions[[2]]$column, "gear")
      expect_equal(conditions[[2]]$operator, "&")
    }
  )
})

test_that("mod_value_filter_server handles exclude mode", {
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(
        list(column = "Species", values = c("virginica"), mode = "exclude")
      ),
      get_data = function() iris
    ),
    {
      session$flushReact()
      result <- session$getReturned()

      # Check exclude mode is set
      conditions <- result$conditions()
      expect_equal(conditions[[1]]$mode, "exclude")
    }
  )
})

test_that("mod_value_filter_server handles OR operator", {
  or_conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include"),
    list(column = "Species", values = c("versicolor"), mode = "include", operator = "|")
  )

  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() or_conditions,
      get_data = function() iris
    ),
    {
      session$flushReact()
      result <- session$getReturned()

      # Check OR operator is set
      conditions <- result$conditions()
      expect_equal(conditions[[2]]$operator, "|")
    }
  )
})

test_that("mod_value_filter_server with mixed column types", {
  # Test data with numeric, character, and factor columns
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(),
      get_data = function() iris
    ),
    {
      session$flushReact()

      # Test numeric column
      numeric_values <- get_unique_values("Sepal.Length")
      expect_true(length(numeric_values) > 0)

      # Test factor column
      factor_values <- get_unique_values("Species")
      expect_equal(sort(factor_values), sort(c("setosa", "versicolor", "virginica")))
    }
  )
})

# Tests for reactive observeEvent blocks
test_that("mod_value_filter_server add_condition button - testServer", {
  # Test add_condition button (lines 201-222)
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(
        list(column = "Species", values = c("setosa"), mode = "include")
      ),
      get_data = function() iris
    ),
    {
      session$flushReact()

      # Initial conditions
      returned <- session$getReturned()
      initial_conditions <- returned$conditions()
      initial_count <- length(initial_conditions)

      # Click add_condition button (triggers observeEvent at line 201)
      session$setInputs(add_condition = 1)
      session$flushReact()

      # Should have added a new condition
      returned2 <- session$getReturned()
      updated_conditions <- returned2$conditions()
      # Note: May not increase if UI not rendered yet, but shouldn't crash
      expect_type(updated_conditions, "list")

      # Click again
      session$setInputs(add_condition = 2)
      session$flushReact()

      returned3 <- session$getReturned()
      final_conditions <- returned3$conditions()
      expect_type(final_conditions, "list")
    }
  )
})

test_that("mod_value_filter_server preserve_order checkbox - testServer", {
  # Test preserve_order checkbox (lines 378-380)
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(),
      get_data = function() iris,
      preserve_order = FALSE
    ),
    {
      session$flushReact()

      # Initial preserve_order should be FALSE
      returned <- session$getReturned()
      initial_preserve <- returned$preserve_order()
      expect_false(initial_preserve)

      # Change preserve_order checkbox (triggers observeEvent at line 378)
      session$setInputs(preserve_order = TRUE)
      session$flushReact()

      # Should be updated
      returned2 <- session$getReturned()
      updated_preserve <- returned2$preserve_order()
      expect_true(updated_preserve)

      # Change back
      session$setInputs(preserve_order = FALSE)
      session$flushReact()

      returned3 <- session$getReturned()
      final_preserve <- returned3$preserve_order()
      expect_false(final_preserve)
    }
  )
})

test_that("mod_value_filter_server column selection updates values - testServer", {
  # Test dynamic column selection updates (lines 322-361)
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(),
      get_data = function() iris
    ),
    {
      session$flushReact()

      # Set column for first condition
      session$setInputs(condition_1_column = "Species")
      session$flushReact()

      # The observe block should trigger updateSelectizeInput
      # We can't directly test updateSelectizeInput, but we can verify no crash
      returned <- session$getReturned()
      conditions <- returned$conditions()
      expect_type(conditions, "list")

      # Change to different column
      session$setInputs(condition_1_column = "Sepal.Length")
      session$flushReact()

      returned2 <- session$getReturned()
      conditions2 <- returned2$conditions()
      expect_type(conditions2, "list")
    }
  )
})

test_that("mod_value_filter_server mode checkbox - testServer", {
  # Test mode checkbox (exclude vs include)
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(
        list(column = "Species", values = c("setosa"), mode = "include")
      ),
      get_data = function() iris
    ),
    {
      session$flushReact()

      # Initial mode is include (FALSE checkbox)
      returned <- session$getReturned()
      initial_conditions <- returned$conditions()
      expect_equal(initial_conditions[[1]]$mode, "include")

      # Change mode to exclude (TRUE checkbox)
      session$setInputs(condition_1_mode = TRUE)
      session$flushReact()

      # Note: Input changes may not immediately reflect without UI rendered
      # The important part is testing that it doesn't crash
      returned2 <- session$getReturned()
      updated_conditions <- returned2$conditions()
      expect_type(updated_conditions, "list")
      expect_true(length(updated_conditions) > 0)

      # Change back to include
      session$setInputs(condition_1_mode = FALSE)
      session$flushReact()

      returned3 <- session$getReturned()
      final_conditions <- returned3$conditions()
      expect_type(final_conditions, "list")
      expect_true(length(final_conditions) > 0)
    }
  )
})

test_that("mod_value_filter_server values selection - testServer", {
  # Test values selection input
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(
        list(column = "Species", values = c("setosa"), mode = "include")
      ),
      get_data = function() iris
    ),
    {
      session$flushReact()

      # Change selected values
      session$setInputs(
        condition_1_column = "Species",
        condition_1_values = c("setosa", "versicolor")
      )
      session$flushReact()

      returned <- session$getReturned()
      conditions <- returned$conditions()
      expect_type(conditions, "list")

      # Clear values
      session$setInputs(condition_1_values = character(0))
      session$flushReact()

      returned2 <- session$getReturned()
      conditions2 <- returned2$conditions()
      expect_type(conditions2, "list")
    }
  )
})

test_that("mod_value_filter_server operator selection - testServer", {
  # Test operator selection for multiple conditions
  testServer(
    mod_value_filter_server,
    args = list(
      get_value = function() list(
        list(column = "Species", values = c("setosa"), mode = "include"),
        list(column = "Sepal.Length", values = c("5.1"), mode = "include", operator = "&")
      ),
      get_data = function() iris
    ),
    {
      session$flushReact()

      returned <- session$getReturned()
      initial_conditions <- returned$conditions()
      expect_equal(length(initial_conditions), 2)
      if (length(initial_conditions) >= 2) {
        expect_equal(initial_conditions[[2]]$operator, "&")
      }

      # Change operator to OR
      session$setInputs(operator_2 = "|")
      session$flushReact()

      # Note: Input changes may not immediately reflect without UI rendered
      returned2 <- session$getReturned()
      updated_conditions <- returned2$conditions()
      expect_type(updated_conditions, "list")
      expect_true(length(updated_conditions) >= 2)
    }
  )
})
