test_that("mutate block adds new column", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test mutate adds a new column
  app_dir <- create_test_app(
    block_code = 'serve(new_mutate_block(exprs = list(mpg_squared = "mpg * mpg")), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "mutate_add")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))

  # Verify new column exists
  expect_true("mpg_squared" %in% names(result_data))

  # Verify calculation is correct
  expect_equal(result_data$mpg_squared, mtcars$mpg * mtcars$mpg)

  # Verify all original columns still present
  expect_true(all(names(mtcars) %in% names(result_data)))

  cleanup_test_app(app_dir, app)
})

test_that("mutate block with grouping (single group)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test mutate with .by parameter
  app_dir <- create_test_app(
    block_code = 'serve(new_mutate_block(exprs = list(mean_mpg = "mean(mpg)"), by = "cyl"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "mutate_grouped")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify new column exists
  expect_true("mean_mpg" %in% names(result_data))

  # Verify grouping worked - mean_mpg should be same for all rows with same cyl
  for (cyl_val in unique(result_data$cyl)) {
    cyl_rows <- result_data[result_data$cyl == cyl_val, ]
    expect_true(all(cyl_rows$mean_mpg == cyl_rows$mean_mpg[1]))
  }

  cleanup_test_app(app_dir, app)
})

test_that("mutate block with multiple expressions and multiple grouping columns", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test mutate with multiple expressions AND multiple grouping columns
  # This tests full restorability of both exprs and by parameters
  app_dir <- create_test_app(
    block_code = 'serve(new_mutate_block(
      exprs = list(
        avg_mpg = "mean(mpg)",
        max_hp = "max(hp)",
        efficiency = "mpg / wt"
      ),
      by = c("cyl", "am")
    ), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "mutate_multi_group")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify all new columns exist
  expect_true("avg_mpg" %in% names(result_data))
  expect_true("max_hp" %in% names(result_data))
  expect_true("efficiency" %in% names(result_data))

  # Verify grouping worked for avg_mpg - should be same for all rows with same cyl+am
  for (cyl_val in unique(result_data$cyl)) {
    for (am_val in unique(result_data$am)) {
      group_rows <- result_data[result_data$cyl == cyl_val & result_data$am == am_val, ]
      if (nrow(group_rows) > 0) {
        # avg_mpg should be constant within each cyl+am group
        expect_true(all(group_rows$avg_mpg == group_rows$avg_mpg[1]))
        # max_hp should be constant within each cyl+am group
        expect_true(all(group_rows$max_hp == group_rows$max_hp[1]))
      }
    }
  }

  # Verify efficiency is calculated per-row (not grouped)
  expect_equal(result_data$efficiency, result_data$mpg / result_data$wt)

  cleanup_test_app(app_dir, app)
})
