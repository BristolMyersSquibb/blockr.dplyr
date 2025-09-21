test_that("parse_simple correctly parses numeric filter expressions", {
  data <- mtcars
  cols <- colnames(data)

  # Test range pattern
  result <- parse_simple("mpg >= 20 & mpg <= 30", cols, data)
  expect_equal(result$column, "mpg")
  expect_equal(result$range, c(20, 30))

  # Test equality pattern
  result <- parse_simple("cyl == 4", cols, data)
  expect_equal(result$column, "cyl")
  expect_equal(result$range, c(4, 4))

  # Test greater than
  result <- parse_simple("hp > 100", cols, data)
  expect_equal(result$column, "hp")
  expect_true(result$range[1] > 100)
  expect_equal(result$range[2], max(data$hp))

  # Test less than or equal
  result <- parse_simple("disp <= 200", cols, data)
  expect_equal(result$column, "disp")
  expect_equal(result$range[1], min(data$disp))
  expect_equal(result$range[2], 200)
})

test_that("parse_simple correctly parses character filter expressions", {
  data <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    category = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )
  cols <- colnames(data)

  # Test %in% pattern
  result <- parse_simple('name %in% c("Alice", "Bob")', cols, data)
  expect_equal(result$column, "name")
  expect_equal(result$values, c("Alice", "Bob"))
  expect_true(result$include)

  # Test negated %in%
  result <- parse_simple('!category %in% c("A")', cols, data)
  expect_equal(result$column, "category")
  expect_equal(result$values, "A")
  expect_false(result$include)

  # Test single equality
  result <- parse_simple('name == "Charlie"', cols, data)
  expect_equal(result$column, "name")
  expect_equal(result$values, "Charlie")
  expect_true(result$include)

  # Test inequality
  result <- parse_simple('category != "B"', cols, data)
  expect_equal(result$column, "category")
  expect_equal(result$values, "B")
  expect_false(result$include)
})

test_that("parse_simple returns NULL for invalid expressions", {
  data <- mtcars
  cols <- colnames(data)

  expect_null(parse_simple(NULL, cols, data))
  expect_null(parse_simple("", cols, data))
  expect_null(parse_simple("TRUE", cols, data))
  expect_null(parse_simple("unknown_column > 5", cols, data))
})

test_that("build_simple creates correct numeric filter expressions", {
  data <- mtcars

  # Test range filter
  result <- build_simple("mpg", data, range_val = c(20, 30))
  expect_equal(result, "mpg >= 20 & mpg <= 30")

  # Test single value
  result <- build_simple("cyl", data, range_val = c(4, 4))
  expect_equal(result, "cyl == 4")

  # Test upper bound only (at min)
  col_range <- range(data$mpg, na.rm = TRUE)
  result <- build_simple("mpg", data, range_val = c(col_range[1], 25))
  expect_equal(result, "mpg <= 25")

  # Test lower bound only (at max)
  result <- build_simple("mpg", data, range_val = c(15, col_range[2]))
  expect_equal(result, "mpg >= 15")

  # Test full range (returns TRUE)
  result <- build_simple("mpg", data, range_val = col_range)
  expect_equal(result, "TRUE")
})

test_that("build_simple creates correct character filter expressions", {
  data <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    category = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )

  # Test single value inclusion
  result <- build_simple("name", data, selected_vals = "Alice", include_mode = "include")
  expect_equal(result, 'name == "Alice"')

  # Test multiple values inclusion
  result <- build_simple("name", data, selected_vals = c("Alice", "Bob"), include_mode = "include")
  expect_equal(result, 'name %in% c("Alice", "Bob")')

  # Test single value exclusion
  result <- build_simple("category", data, selected_vals = "B", include_mode = "exclude")
  expect_equal(result, 'category != "B"')

  # Test multiple values exclusion
  result <- build_simple("category", data, selected_vals = c("A", "B"), include_mode = "exclude")
  expect_equal(result, '!category %in% c("A", "B")')
})

test_that("build_simple handles edge cases", {
  data <- mtcars

  # NULL column
  expect_equal(build_simple(NULL, data), "TRUE")

  # Empty column
  expect_equal(build_simple("", data), "TRUE")

  # No data provided
  expect_equal(build_simple("mpg", NULL, range_val = c(20, 30)), "TRUE")

  # Column not in data
  expect_equal(build_simple("unknown", data, range_val = c(20, 30)), "TRUE")

  # No values provided
  expect_equal(build_simple("mpg", data), "TRUE")
})

test_that("format_range creates correct range expressions", {
  # Single value
  result <- format_range("mpg", c(20, 20), c(10, 35))
  expect_equal(result, "mpg == 20")

  # Full range
  result <- format_range("mpg", c(10, 35), c(10, 35))
  expect_equal(result, "TRUE")

  # Upper bound only
  result <- format_range("mpg", c(10, 25), c(10, 35))
  expect_equal(result, "mpg <= 25")

  # Lower bound only
  result <- format_range("mpg", c(20, 35), c(10, 35))
  expect_equal(result, "mpg >= 20")

  # Both bounds
  result <- format_range("mpg", c(20, 30), c(10, 35))
  expect_equal(result, "mpg >= 20 & mpg <= 30")
})

test_that("format_values creates correct value expressions", {
  # Single value inclusion
  result <- format_values("name", "Alice", "include")
  expect_equal(result, 'name == "Alice"')

  # Multiple values inclusion
  result <- format_values("name", c("Alice", "Bob"), "include")
  expect_equal(result, 'name %in% c("Alice", "Bob")')

  # Single value exclusion
  result <- format_values("category", "A", "exclude")
  expect_equal(result, 'category != "A"')

  # Multiple values exclusion
  result <- format_values("category", c("A", "B"), "exclude")
  expect_equal(result, '!category %in% c("A", "B")')
})

test_that("combine_filters correctly combines conditions", {
  # Empty conditions
  expect_equal(combine_filters(list()), "TRUE")

  # Single condition
  expect_equal(combine_filters(list("mpg > 20")), "mpg > 20")

  # Multiple conditions with default AND
  result <- combine_filters(list("mpg > 20", "cyl == 4"))
  expect_equal(result, "mpg > 20 & cyl == 4")

  # Multiple conditions with explicit operators
  result <- combine_filters(
    list("mpg > 20", "cyl == 4", "hp > 100"),
    operators = c("&", "|")
  )
  expect_equal(result, "mpg > 20 & cyl == 4 | hp > 100")

  # All OR operators
  result <- combine_filters(
    list("mpg > 30", "cyl == 6", "hp > 200"),
    operators = c("|", "|")
  )
  expect_equal(result, "mpg > 30 | cyl == 6 | hp > 200")
})

test_that("parse_numeric_filter handles all comparison operators", {
  col_range <- c(10, 35)

  # Greater than or equal
  result <- parse_numeric_filter("mpg >= 20", "mpg", col_range)
  expect_equal(result$column, "mpg")
  expect_equal(result$range, c(20, 35))

  # Greater than
  result <- parse_numeric_filter("mpg > 20", "mpg", col_range)
  expect_equal(result$column, "mpg")
  expect_equal(result$range[1], 20.01)
  expect_equal(result$range[2], 35)

  # Less than or equal
  result <- parse_numeric_filter("mpg <= 25", "mpg", col_range)
  expect_equal(result$column, "mpg")
  expect_equal(result$range, c(10, 25))

  # Less than
  result <- parse_numeric_filter("mpg < 25", "mpg", col_range)
  expect_equal(result$column, "mpg")
  expect_equal(result$range[1], 10)
  expect_equal(result$range[2], 24.99)

  # Equality
  result <- parse_numeric_filter("mpg == 22.5", "mpg", col_range)
  expect_equal(result$column, "mpg")
  expect_equal(result$range, c(22.5, 22.5))

  # Range pattern
  result <- parse_numeric_filter("mpg >= 20 & mpg <= 30", "mpg", col_range)
  expect_equal(result$column, "mpg")
  expect_equal(result$range, c(20, 30))
})

test_that("parse_character_filter handles quoted values correctly", {
  # Single quotes not currently supported, but double quotes work
  result <- parse_character_filter('name == "Alice"', "name")
  expect_equal(result$column, "name")
  expect_equal(result$values, "Alice")
  expect_true(result$include)

  # Multiple values with spaces
  result <- parse_character_filter('city %in% c("New York", "Los Angeles")', "city")
  expect_equal(result$column, "city")
  expect_equal(result$values, c("New York", "Los Angeles"))
  expect_true(result$include)

  # Negated with special characters
  result <- parse_character_filter('!code %in% c("A-1", "B-2")', "code")
  expect_equal(result$column, "code")
  expect_equal(result$values, c("A-1", "B-2"))
  expect_false(result$include)
})