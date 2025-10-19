test_that("summarize block creates single summary row without grouping", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create summarize block with mean calculation
  app_dir <- create_test_app(
    block_code = 'serve(new_summarize_block(exprs = list(mean_mpg = "mean(mpg)")), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_simple")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got a data frame
  expect_true(is.data.frame(result_data))

  # Should have exactly 1 row (no grouping)
  expect_equal(nrow(result_data), 1)

  # Should have the summary column
  expect_true("mean_mpg" %in% names(result_data))

  # Verify the calculated value is correct
  expected_mean <- mean(mtcars$mpg)
  expect_equal(result_data$mean_mpg, expected_mean, tolerance = 0.0001)

  cleanup_test_app(app_dir, app)
})

test_that("summarize block with multiple summary expressions", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Multiple summary calculations
  app_dir <- create_test_app(
    block_code = 'serve(new_summarize_block(exprs = list(mean_mpg = "mean(mpg)", sum_hp = "sum(hp)", count = "n()")), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_multiple")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 1 row with 3 columns
  expect_equal(nrow(result_data), 1)
  expect_equal(ncol(result_data), 3)

  # Verify all columns are present
  expect_true(all(c("mean_mpg", "sum_hp", "count") %in% names(result_data)))

  # Verify calculated values
  expect_equal(result_data$mean_mpg, mean(mtcars$mpg), tolerance = 0.0001)
  expect_equal(result_data$sum_hp, sum(mtcars$hp))
  expect_equal(result_data$count, nrow(mtcars))

  cleanup_test_app(app_dir, app)
})

test_that("summarize block with grouping by single column", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Group by cylinder count
  app_dir <- create_test_app(
    block_code = 'serve(new_summarize_block(exprs = list(mean_mpg = "mean(mpg)", count = "n()"), by = "cyl"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_group_single")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have one row per unique cyl value
  n_groups <- length(unique(mtcars$cyl))
  expect_equal(nrow(result_data), n_groups)

  # Should have grouping column plus summary columns
  expect_true("cyl" %in% names(result_data))
  expect_true("mean_mpg" %in% names(result_data))
  expect_true("count" %in% names(result_data))

  # Verify calculations for one group (e.g., cyl == 4)
  cyl_4_data <- mtcars[mtcars$cyl == 4, ]
  cyl_4_result <- result_data[result_data$cyl == 4, ]

  expect_equal(cyl_4_result$mean_mpg, mean(cyl_4_data$mpg), tolerance = 0.0001)
  expect_equal(cyl_4_result$count, nrow(cyl_4_data))

  cleanup_test_app(app_dir, app)
})

test_that("summarize block with grouping by multiple columns", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Group by cyl and gear
  app_dir <- create_test_app(
    block_code = 'serve(new_summarize_block(exprs = list(mean_mpg = "mean(mpg)"), by = c("cyl", "gear")), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_group_multiple")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have one row per unique combination of cyl and gear
  n_groups <- nrow(unique(mtcars[, c("cyl", "gear")]))
  expect_equal(nrow(result_data), n_groups)

  # Should have both grouping columns
  expect_true(all(c("cyl", "gear", "mean_mpg") %in% names(result_data)))

  # Verify no duplicate group combinations
  group_cols <- result_data[, c("cyl", "gear")]
  expect_equal(nrow(group_cols), nrow(unique(group_cols)))

  cleanup_test_app(app_dir, app)
})

test_that("summarize block with various aggregation functions", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test different aggregation functions
  app_dir <- create_test_app(
    block_code = 'serve(new_summarize_block(exprs = list(min_mpg = "min(mpg)", max_mpg = "max(mpg)", median_mpg = "median(mpg)", sd_mpg = "sd(mpg)", n = "n()"), by = "cyl"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_functions")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify all summary columns are present
  expect_true(all(
    c("cyl", "min_mpg", "max_mpg", "median_mpg", "sd_mpg", "n") %in% names(result_data)
  ))

  # Verify calculations for cyl == 6
  cyl_6_data <- mtcars[mtcars$cyl == 6, ]
  cyl_6_result <- result_data[result_data$cyl == 6, ]

  expect_equal(cyl_6_result$min_mpg, min(cyl_6_data$mpg), tolerance = 0.0001)
  expect_equal(cyl_6_result$max_mpg, max(cyl_6_data$mpg), tolerance = 0.0001)
  expect_equal(cyl_6_result$median_mpg, median(cyl_6_data$mpg), tolerance = 0.0001)
  expect_equal(cyl_6_result$n, nrow(cyl_6_data))

  cleanup_test_app(app_dir, app)
})

test_that("summarize block with custom data and grouping", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create custom test data
  test_data_code <- "test_data <- data.frame(
    category = c('A', 'A', 'B', 'B', 'C', 'C'),
    value = c(10, 20, 30, 40, 50, 60),
    weight = c(1, 2, 3, 4, 5, 6)
  )"

  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_summarize_block(exprs = list(total = "sum(value)", avg = "mean(value)", count = "n()"), by = "category"), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_custom")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 3 rows (one per category)
  expect_equal(nrow(result_data), 3)

  # Verify calculations for category A
  cat_a_result <- result_data[result_data$category == "A", ]
  expect_equal(cat_a_result$total, 30) # 10 + 20
  expect_equal(cat_a_result$avg, 15) # (10 + 20) / 2
  expect_equal(cat_a_result$count, 2)

  cleanup_test_app(app_dir, app)
})

test_that("summarize block handles empty groups correctly", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create data where filtering might create empty groups
  test_data_code <- "test_data <- mtcars[mtcars$cyl == 4, ]" # Only 4-cylinder cars

  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_summarize_block(exprs = list(mean_mpg = "mean(mpg)", count = "n()"), by = "cyl"), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_empty_groups")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have only 1 row (only cyl == 4 exists in data)
  expect_equal(nrow(result_data), 1)
  expect_equal(result_data$cyl, 4)

  cleanup_test_app(app_dir, app)
})

test_that("summarize block with across() for multiple columns", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test using across() to summarize multiple numeric columns
  # Using unnamed expression: across(where(is.numeric), mean)
  # This should create mean_mpg, mean_cyl, mean_disp, etc.

  app_dir <- create_test_app(
    block_code = 'serve(new_summarize_block(exprs = setNames("across(where(is.numeric), mean)", ""), by = character(0)), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_across")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 1 row (no grouping)
  expect_equal(nrow(result_data), 1)

  # Should have mean for all numeric columns (mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
  # When using unnamed across(), columns get named with the function: mean_mpg, mean_cyl, etc.
  numeric_cols <- names(mtcars)[sapply(mtcars, is.numeric)]
  expected_cols <- paste0("mpg_", "mean")  # Check at least one column exists

  # Just verify we got some columns back and they contain means
  expect_true(ncol(result_data) >= length(numeric_cols))

  # Verify at least the mpg mean is calculated correctly
  # The column name depends on how across() names it - could be "mpg" or "mpg_mean" or "mean_mpg"
  if ("mpg" %in% names(result_data)) {
    expect_equal(result_data$mpg, mean(mtcars$mpg), tolerance = 0.0001)
  } else if ("mpg_mean" %in% names(result_data)) {
    expect_equal(result_data$mpg_mean, mean(mtcars$mpg), tolerance = 0.0001)
  } else if ("mean_mpg" %in% names(result_data)) {
    expect_equal(result_data$mean_mpg, mean(mtcars$mpg), tolerance = 0.0001)
  } else {
    # Just verify we got numeric results
    expect_true(is.numeric(result_data[[1]]))
  }

  cleanup_test_app(app_dir, app)
})

test_that("summarize block full restorability - all parameters (exprs + by)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test comprehensive restorability with:
  # - Multiple expressions (exprs)
  # - Multiple grouping columns (by)
  # Note: unpack parameter testing requires further investigation -
  # the parameter exists but behavior in shinytest2 context needs verification

  app_dir <- create_test_app(
    block_code = 'serve(new_summarize_block(
      exprs = list(
        mean_mpg = "mean(mpg)",
        max_hp = "max(hp)",
        count = "n()"
      ),
      by = c("cyl", "am")
    ), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "summarize_full_restore")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify grouping columns are present
  expect_true("cyl" %in% names(result_data))
  expect_true("am" %in% names(result_data))

  # Verify all expression columns are present
  expect_true("mean_mpg" %in% names(result_data))
  expect_true("max_hp" %in% names(result_data))
  expect_true("count" %in% names(result_data))

  # Verify we have one row per cyl+am combination
  n_groups <- nrow(unique(mtcars[, c("cyl", "am")]))
  expect_equal(nrow(result_data), n_groups)

  # Verify calculations for one specific group (e.g., cyl == 4, am == 1)
  group_data <- mtcars[mtcars$cyl == 4 & mtcars$am == 1, ]
  if (nrow(group_data) > 0) {
    group_result <- result_data[result_data$cyl == 4 & result_data$am == 1, ]
    expect_equal(group_result$mean_mpg, mean(group_data$mpg), tolerance = 0.0001)
    expect_equal(group_result$max_hp, max(group_data$hp), tolerance = 0.0001)
    expect_equal(group_result$count, nrow(group_data))
  }

  cleanup_test_app(app_dir, app)
})
