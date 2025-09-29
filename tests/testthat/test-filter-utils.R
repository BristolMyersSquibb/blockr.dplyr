test_that("parse_simple correctly identifies column names with dots", {
  # Test with iris dataset column names
  cols <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

  # Test 1: Petal.Length should be correctly identified
  result <- parse_simple("Petal.Length > 5", cols, iris)
  expect_equal(result$column, "Petal.Length")
  expect_equal(result$range, c(5, max(iris$Petal.Length)))

  # Test 2: Sepal.Length should be correctly identified
  result <- parse_simple("Sepal.Length < 6", cols, iris)
  expect_equal(result$column, "Sepal.Length")
  expect_equal(result$range, c(min(iris$Sepal.Length), 6))

  # Test 3: Should not confuse Petal.Length with Sepal.Length
  result <- parse_simple("Petal.Length >= 4", cols, iris)
  expect_equal(result$column, "Petal.Length")
  expect_false(result$column == "Sepal.Length")
})

test_that("parse_simple handles overlapping column names correctly", {
  # Create test data with overlapping column names
  test_data <- data.frame(
    Length = 1:10,
    Sepal.Length = 11:20,
    Petal.Length = 21:30,
    Width = 31:40,
    Sepal.Width = 41:50
  )
  cols <- names(test_data)

  # Test 1: Should match exact column "Length" not "Sepal.Length"
  result <- parse_simple("Length > 5", cols, test_data)
  expect_equal(result$column, "Length")

  # Test 2: Should match "Sepal.Length" when specified
  result <- parse_simple("Sepal.Length == 15", cols, test_data)
  expect_equal(result$column, "Sepal.Length")
  expect_equal(result$range, c(15, 15))

  # Test 3: Should match "Petal.Length" when specified
  result <- parse_simple("Petal.Length < 25", cols, test_data)
  expect_equal(result$column, "Petal.Length")
  expect_equal(result$range, c(21, 25))
})

test_that("parse_simple handles various operators correctly", {
  cols <- names(iris)

  # Test equality
  result <- parse_simple("Sepal.Length == 5.1", cols, iris)
  expect_equal(result$column, "Sepal.Length")
  expect_equal(result$range, c(5.1, 5.1))

  # Test greater than
  result <- parse_simple("Petal.Width > 2", cols, iris)
  expect_equal(result$column, "Petal.Width")
  expect_equal(result$range, c(2, max(iris$Petal.Width)))

  # Test less than or equal
  result <- parse_simple("Sepal.Width <= 3", cols, iris)
  expect_equal(result$column, "Sepal.Width")
  expect_equal(result$range, c(min(iris$Sepal.Width), 3))

  # Test greater than or equal
  result <- parse_simple("Petal.Length >= 1.5", cols, iris)
  expect_equal(result$column, "Petal.Length")
  expect_equal(result$range, c(1.5, max(iris$Petal.Length)))
})

test_that("parse_simple handles character columns correctly", {
  cols <- names(iris)

  # Test %in% operator
  result <- parse_simple('Species %in% c("setosa", "versicolor")', cols, iris)
  expect_equal(result$column, "Species")
  expect_equal(result$values, c("setosa", "versicolor"))
  expect_true(result$include)

  # Test negated %in%
  result <- parse_simple('!Species %in% c("virginica")', cols, iris)
  expect_equal(result$column, "Species")
  expect_equal(result$values, "virginica")
  expect_false(result$include)

  # Test equality with character column
  result <- parse_simple('Species == "setosa"', cols, iris)
  expect_equal(result$column, "Species")
  expect_equal(result$values, "setosa")
  expect_true(result$include)

  # Test inequality with character column
  result <- parse_simple('Species != "setosa"', cols, iris)
  expect_equal(result$column, "Species")
  expect_equal(result$values, "setosa")
  expect_false(result$include)
})

test_that("parse_simple handles all valid R column names", {
  # Test various valid R column name formats
  test_data <- data.frame(
    # Standard names
    col_1 = 1:10,
    col_2a = 11:20,
    my_column = 21:30,
    another_col_123 = 31:40,
    # Names with dots
    my.column = 41:50,
    data.table = 51:60,
    a.b.c.d = 61:70,
    # Names with special characters (backticks required in R)
    `column-name` = 71:80,
    `column name` = 81:90,
    `123column` = 91:100,
    `column@test` = 101:110,
    `column$test` = 111:120,
    # Edge cases
    `.hidden` = 121:130,
    `_underscore` = 131:140,
    X = 141:150,
    x = 151:160,
    # Unicode names (if supported)
    `année` = 161:170,
    # Really long name
    this_is_a_really_long_column_name_that_should_still_work = 171:180
  )
  cols <- names(test_data)

  # Test standard names with numbers and underscores
  result <- parse_simple("col_1 > 5", cols, test_data)
  expect_equal(result$column, "col_1")

  result <- parse_simple("col_2a <= 15", cols, test_data)
  expect_equal(result$column, "col_2a")

  result <- parse_simple("my_column == 25", cols, test_data)
  expect_equal(result$column, "my_column")

  result <- parse_simple("another_col_123 >= 35", cols, test_data)
  expect_equal(result$column, "another_col_123")

  # Test names with dots
  result <- parse_simple("my.column > 45", cols, test_data)
  expect_equal(result$column, "my.column")

  result <- parse_simple("data.table < 55", cols, test_data)
  expect_equal(result$column, "data.table")

  result <- parse_simple("a.b.c.d == 65", cols, test_data)
  expect_equal(result$column, "a.b.c.d")

  # Test names that require backticks in R
  result <- parse_simple("`column-name` > 75", cols, test_data)
  expect_equal(result$column, "column-name")

  result <- parse_simple("`column name` <= 85", cols, test_data)
  expect_equal(result$column, "column name")

  result <- parse_simple("`123column` == 95", cols, test_data)
  expect_equal(result$column, "123column")

  # Test edge cases
  result <- parse_simple("X > 145", cols, test_data)
  expect_equal(result$column, "X")

  result <- parse_simple("x < 155", cols, test_data)
  expect_equal(result$column, "x")

  # Test long column name
  result <- parse_simple("this_is_a_really_long_column_name_that_should_still_work >= 175", cols, test_data)
  expect_equal(result$column, "this_is_a_really_long_column_name_that_should_still_work")
})

test_that("parse_simple handles ambiguous column names correctly", {
  # Test with columns that could be ambiguous
  test_data <- data.frame(
    col = 1:10,
    column = 11:20,
    column_name = 21:30,
    `column name` = 31:40,
    data = 41:50,
    data.frame = 51:60,
    data.table = 61:70,
    my_data = 71:80,
    my_data_frame = 81:90
  )
  cols <- names(test_data)

  # Should match the most specific (longest) column name first
  result <- parse_simple("column_name > 25", cols, test_data)
  expect_equal(result$column, "column_name")
  expect_false(result$column == "column")

  result <- parse_simple("column > 15", cols, test_data)
  expect_equal(result$column, "column")
  expect_false(result$column == "col")

  result <- parse_simple("data.table < 65", cols, test_data)
  expect_equal(result$column, "data.table")
  expect_false(result$column == "data")

  result <- parse_simple("my_data_frame == 85", cols, test_data)
  expect_equal(result$column, "my_data_frame")
  expect_false(result$column == "my_data")
})

test_that("parse_simple returns NULL for invalid expressions", {
  cols <- names(iris)

  # Test with empty string
  expect_null(parse_simple("", cols, iris))

  # Test with NULL
  expect_null(parse_simple(NULL, cols, iris))

  # Test with "TRUE"
  expect_null(parse_simple("TRUE", cols, iris))

  # Test with non-existent column
  expect_null(parse_simple("NonExistent > 5", cols, iris))

  # Test with invalid expression
  expect_null(parse_simple("random text", cols, iris))
})

test_that("parse_simple handles range expressions correctly", {
  cols <- names(iris)

  # Test compound range expression
  result <- parse_simple("Sepal.Length >= 5 & Sepal.Length <= 7", cols, iris)
  expect_equal(result$column, "Sepal.Length")
  expect_equal(result$range, c(5, 7))
})

test_that("split_filter_conditions handles complex expressions", {
  # Test simple AND
  result <- split_filter_conditions("mpg > 20 & cyl == 4")
  expect_equal(result, c("mpg > 20", "cyl == 4"))

  # Test simple OR
  result <- split_filter_conditions("mpg > 20 | cyl == 4")
  expect_equal(result, c("mpg > 20", "cyl == 4"))

  # Test mixed operators
  result <- split_filter_conditions("mpg > 20 & cyl == 4 | hp < 100")
  expect_equal(result, c("mpg > 20", "cyl == 4", "hp < 100"))

  # Test with parentheses
  result <- split_filter_conditions("(mpg > 20) & (cyl == 4)")
  expect_equal(result, c("(mpg > 20)", "(cyl == 4)"))

  # Test with quoted strings
  result <- split_filter_conditions('name == "John & Mary" | age > 30')
  expect_equal(result, c('name == "John & Mary"', "age > 30"))
})

test_that("extract_logical_operators works correctly", {
  # Test AND operators
  result <- extract_logical_operators("a > 1 & b < 2 & c == 3")
  expect_equal(result, c("&", "&"))

  # Test OR operators
  result <- extract_logical_operators("a > 1 | b < 2 | c == 3")
  expect_equal(result, c("|", "|"))

  # Test mixed operators
  result <- extract_logical_operators("a > 1 & b < 2 | c == 3")
  expect_equal(result, c("&", "|"))

  # Test with quoted strings containing operators
  result <- extract_logical_operators('name == "A & B" | age > 30')
  expect_equal(result, "|")

  # Test empty/NULL cases
  expect_equal(extract_logical_operators(""), character(0))
  expect_equal(extract_logical_operators(NULL), character(0))
  expect_equal(extract_logical_operators("TRUE"), character(0))
})

test_that("can_parse_simple identifies simple expressions correctly", {
  # Should be parseable as simple
  expect_true(can_parse_simple("column > 5"))
  expect_true(can_parse_simple("column == 10"))
  expect_true(can_parse_simple("column %in% c(1, 2, 3)"))
  expect_true(can_parse_simple("between(column, 1, 10)"))

  # Should NOT be parseable as simple
  expect_false(can_parse_simple(""))
  expect_false(can_parse_simple("TRUE"))
  expect_false(can_parse_simple(NULL))
  expect_false(can_parse_simple("complex_function(column)"))
  # Note: can_parse_simple actually returns TRUE for compound expressions
  # because it only checks if the expression starts with a simple pattern
  # This is by design - it checks if ANY part can be parsed as simple
  expect_true(can_parse_simple("column > 5 & other < 10"))
})

test_that("format_range generates correct filter expressions", {
  # Test single value
  result <- format_range("column", c(5, 5), c(0, 10))
  expect_equal(result, "column == 5")

  # Test full range (should return TRUE)
  result <- format_range("column", c(0, 10), c(0, 10))
  expect_equal(result, "TRUE")

  # Test lower bound only
  result <- format_range("column", c(5, 10), c(0, 10))
  expect_equal(result, "column >= 5")

  # Test upper bound only
  result <- format_range("column", c(0, 7), c(0, 10))
  expect_equal(result, "column <= 7")

  # Test both bounds
  result <- format_range("column", c(3, 7), c(0, 10))
  expect_equal(result, "column >= 3 & column <= 7")
})

test_that("format_values generates correct filter expressions", {
  # Test single value include
  result <- format_values("column", "A", "include")
  expect_equal(result, 'column == "A"')

  # Test single value exclude
  result <- format_values("column", "A", "exclude")
  expect_equal(result, 'column != "A"')

  # Test multiple values include
  result <- format_values("column", c("A", "B", "C"), "include")
  expect_equal(result, 'column %in% c("A", "B", "C")')

  # Test multiple values exclude
  result <- format_values("column", c("A", "B"), "exclude")
  expect_equal(result, '!column %in% c("A", "B")')
})

test_that("combine_filters creates proper compound expressions", {
  # Test single condition
  result <- combine_filters(list("a > 1"))
  expect_equal(result, "a > 1")

  # Test two conditions with AND
  result <- combine_filters(list("a > 1", "b < 2"), "&")
  expect_equal(result, "a > 1 & b < 2")

  # Test multiple conditions with OR
  result <- combine_filters(list("a > 1", "b < 2", "c == 3"), c("|", "|"))
  expect_equal(result, "a > 1 | b < 2 | c == 3")

  # Test mixed operators
  result <- combine_filters(list("a > 1", "b < 2", "c == 3"), c("&", "|"))
  expect_equal(result, "a > 1 & b < 2 | c == 3")

  # Test empty list
  result <- combine_filters(list())
  expect_equal(result, "TRUE")
})

test_that("new_filter_condition creates valid condition objects", {
  # Test basic condition
  cond <- new_filter_condition("mpg > 20", "&", "simple")
  expect_equal(cond$expression, "mpg > 20")
  expect_equal(cond$logical_op, "&")
  expect_equal(cond$mode, "simple")
  expect_s3_class(cond, "filter_condition")

  # Test with NULL logical operator (first condition)
  cond <- new_filter_condition("mpg > 20", NULL, "advanced")
  expect_equal(cond$expression, "mpg > 20")
  expect_null(cond$logical_op)
  expect_equal(cond$mode, "advanced")

  # Test validation errors
  expect_error(new_filter_condition(123, "&", "simple"), "Expression must be a single character string")
  expect_error(new_filter_condition("expr", "NOT", "simple"), "Logical operator must be")
  expect_error(new_filter_condition("expr", "&", "invalid"), "Mode must be")
})

test_that("parse_filter_string converts strings to condition objects", {
  # Test single condition
  result <- parse_filter_string("mpg > 20")
  expect_length(result, 1)
  expect_equal(result[[1]]$expression, "mpg > 20")
  expect_null(result[[1]]$logical_op)

  # Test multiple conditions with AND
  result <- parse_filter_string("mpg > 20 & cyl == 4")
  expect_length(result, 2)
  expect_equal(result[[1]]$expression, "mpg > 20")
  expect_null(result[[1]]$logical_op)
  expect_equal(result[[2]]$expression, "cyl == 4")
  expect_equal(result[[2]]$logical_op, "&")

  # Test empty/NULL cases
  result <- parse_filter_string("")
  expect_length(result, 1)
  expect_equal(result[[1]]$expression, "TRUE")

  result <- parse_filter_string(NULL)
  expect_length(result, 1)
  expect_equal(result[[1]]$expression, "TRUE")
})

test_that("conditions_to_expression converts conditions back to string", {
  # Test single condition
  conds <- list(new_filter_condition("mpg > 20", NULL, "simple"))
  result <- conditions_to_expression(conds)
  expect_equal(result, "mpg > 20")

  # Test multiple conditions
  conds <- list(
    new_filter_condition("mpg > 20", NULL, "simple"),
    new_filter_condition("cyl == 4", "&", "simple"),
    new_filter_condition("hp < 100", "|", "advanced")
  )
  result <- conditions_to_expression(conds)
  expect_equal(result, "mpg > 20 & cyl == 4 | hp < 100")

  # Test empty list
  result <- conditions_to_expression(list())
  expect_equal(result, "TRUE")
})

test_that("build_simple creates correct expressions from UI inputs", {
  # Test numeric column with range
  result <- build_simple("Sepal.Length", iris, range_val = c(5, 6))
  expect_equal(result, "Sepal.Length >= 5 & Sepal.Length <= 6")

  # Test character column with selected values
  result <- build_simple("Species", iris, selected_vals = c("setosa", "versicolor"),
                         include_mode = "include")
  expect_equal(result, 'Species %in% c("setosa", "versicolor")')

  # Test character column with exclude mode
  result <- build_simple("Species", iris, selected_vals = "virginica",
                         include_mode = "exclude")
  expect_equal(result, 'Species != "virginica"')

  # Test empty/NULL cases
  expect_equal(build_simple(NULL), "TRUE")
  expect_equal(build_simple(""), "TRUE")
  expect_equal(build_simple("NonExistent", iris), "TRUE")
})