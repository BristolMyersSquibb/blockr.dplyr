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
  expect_type(expr, "expression")

  # Test single include condition with character values
  conditions <- list(
    list(
      column = "Species",
      values = c("setosa", "versicolor"),
      mode = "include"
    )
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "expression")
  expect_true(grepl("Species.*%in%.*\"setosa\".*\"versicolor\"", deparse(expr)))

  # Test single exclude condition with character values
  conditions <- list(
    list(column = "Species", values = c("virginica"), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "expression")
  expect_true(grepl("!.*Species.*%in%.*\"virginica\"", deparse(expr)))

  # Test single include condition with numeric values
  conditions <- list(
    list(column = "Sepal.Length", values = c(5.1, 5.4), mode = "include")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "expression")
  expect_true(grepl("Sepal.Length.*%in%.*5.1.*5.4", deparse(expr)))

  # Test multiple conditions (combined with AND)
  conditions <- list(
    list(column = "Species", values = c("setosa"), mode = "include"),
    list(column = "Sepal.Length", values = c(5.1), mode = "exclude")
  )
  expr <- blockr.dplyr:::parse_value_filter(conditions)
  expect_type(expr, "expression")
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
  expect_type(expr, "expression")
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
  expect_type(expr, "expression")

  # Test evaluation doesn't error
  result <- try(eval(expr, envir = list(data = iris)), silent = TRUE)
  expect_false(inherits(result, "try-error"))

  # Test parse_value_filter with empty conditions
  expr <- blockr.dplyr:::parse_value_filter(list())
  expect_type(expr, "expression")

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
