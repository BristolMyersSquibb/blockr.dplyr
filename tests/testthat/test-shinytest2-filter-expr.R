test_that("filter expr block filters rows correctly with simple condition", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create app with filter condition "mpg > 20"
  app_dir <- create_test_app(
    block_code = 'serve(new_filter_expr_block("mpg > 20"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "filter_simple"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got a data frame
  expect_true(is.data.frame(result_data))

  # Calculate expected row count
  expected_rows <- nrow(mtcars[mtcars$mpg > 20, ])

  # Verify correct number of rows
  expect_equal(nrow(result_data), expected_rows)

  # Verify all rows meet the condition
  expect_true(all(result_data$mpg > 20))

  # Verify all columns are still present
  expect_equal(ncol(result_data), ncol(mtcars))
  expect_equal(names(result_data), names(mtcars))

  cleanup_test_app(app_dir, app)
})

test_that("filter expr block handles multiple conditions with AND logic", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Filter with multiple conditions
  app_dir <- create_test_app(
    block_code = 'serve(new_filter_expr_block("mpg > 20 & cyl == 4"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "filter_multiple"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Calculate expected result
  expected_data <- mtcars[mtcars$mpg > 20 & mtcars$cyl == 4, ]

  # Verify row count
  expect_equal(nrow(result_data), nrow(expected_data))

  # Verify all rows meet both conditions
  expect_true(all(result_data$mpg > 20))
  expect_true(all(result_data$cyl == 4))

  cleanup_test_app(app_dir, app)
})

test_that("filter expr block with empty/TRUE condition returns all rows", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Default filter expr block (should use "TRUE" condition)
  app_dir <- create_test_app(
    block_code = 'serve(new_filter_expr_block(), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "filter_empty"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should return all rows
  expect_equal(nrow(result_data), nrow(mtcars))
  expect_equal(ncol(result_data), ncol(mtcars))

  cleanup_test_app(app_dir, app)
})

test_that("filter expr block handles complex conditions", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Complex filter with OR and grouping
  app_dir <- create_test_app(
    block_code = 'serve(new_filter_expr_block("(mpg > 25 | hp > 200) & cyl != 6"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "filter_complex"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Calculate expected result
  expected_data <- mtcars[
    (mtcars$mpg > 25 | mtcars$hp > 200) & mtcars$cyl != 6,
  ]

  # Verify row count
  expect_equal(nrow(result_data), nrow(expected_data))

  # Verify conditions are met
  expect_true(all(result_data$cyl != 6))
  expect_true(all(result_data$mpg > 25 | result_data$hp > 200))

  cleanup_test_app(app_dir, app)
})

test_that("filter expr block returns empty data frame when no rows match", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Filter that matches no rows
  app_dir <- create_test_app(
    block_code = 'serve(new_filter_expr_block("mpg > 100"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "filter_no_match"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should return empty data frame
  expect_equal(nrow(result_data), 0)

  # But should still have all columns
  expect_equal(ncol(result_data), ncol(mtcars))
  expect_equal(names(result_data), names(mtcars))

  cleanup_test_app(app_dir, app)
})

test_that("filter expr block with character column conditions", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test data with character column
  test_data_code <- "test_data <- data.frame(
    name = c('Alice', 'Bob', 'Charlie', 'David'),
    age = c(25, 30, 35, 40),
    city = c('NYC', 'LA', 'NYC', 'Chicago'),
    stringsAsFactors = FALSE
  )"

  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_filter_expr_block("city == \'NYC\'"), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "filter_character"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 2 rows (Alice and Charlie)
  expect_equal(nrow(result_data), 2)

  # All rows should have city == 'NYC'
  expect_true(all(result_data$city == "NYC"))

  cleanup_test_app(app_dir, app)
})

test_that("filter expr block handles numeric comparisons correctly", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test different comparison operators
  test_cases <- list(
    list(condition = "mpg >= 20", verify = function(d) all(d$mpg >= 20)),
    list(condition = "cyl <= 4", verify = function(d) all(d$cyl <= 4)),
    list(condition = "hp != 110", verify = function(d) all(d$hp != 110))
  )

  for (test_case in test_cases) {
    app_dir <- create_test_app(
      block_code = sprintf(
        'serve(new_filter_expr_block("%s"), data = list(data = mtcars))',
        test_case$condition
      )
    )

    app <- shinytest2::AppDriver$new(
      app_dir,
      timeout = 30000,
      name = "filter_numeric"
    )
    app$wait_for_idle()

    values <- app$get_values(export = TRUE)
    result_data <- values$export$result

    # Verify the condition is met
    expect_true(
      test_case$verify(result_data),
      info = sprintf("Condition '%s' verification failed", test_case$condition)
    )

    cleanup_test_app(app_dir, app)
  }
})

test_that("filter expr block with dplyr helper functions", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test using between() helper
  app_dir <- create_test_app(
    block_code = 'serve(new_filter_expr_block("dplyr::between(mpg, 15, 25)"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "filter_between"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # All rows should have mpg between 15 and 25
  expect_true(all(result_data$mpg >= 15 & result_data$mpg <= 25))

  cleanup_test_app(app_dir, app)
})
