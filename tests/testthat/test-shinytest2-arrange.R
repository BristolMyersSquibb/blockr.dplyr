test_that("arrange block sorts data correctly", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test arrange by mpg ascending
  app_dir <- create_test_app(
    block_code = 'serve(new_arrange_block(columns = list(list(column = "mpg", direction = "asc"))), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "arrange_asc"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))
  expect_equal(nrow(result_data), nrow(mtcars))

  # Verify data is sorted by mpg ascending
  expect_true(all(diff(result_data$mpg) >= 0))

  cleanup_test_app(app_dir, app)
})

test_that("arrange block sorts descending", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test arrange by hp descending
  app_dir <- create_test_app(
    block_code = 'serve(new_arrange_block(columns = list(list(column = "hp", direction = "desc"))), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "arrange_desc"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify data is sorted by hp descending
  expect_true(all(diff(result_data$hp) <= 0))

  cleanup_test_app(app_dir, app)
})

test_that("arrange block full restorability - multiple columns with mixed directions", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test full restorability with multiple sort columns and different directions
  # Sort by cyl (ascending), then by mpg (descending), then by hp (ascending)
  app_dir <- create_test_app(
    block_code = 'serve(new_arrange_block(columns = list(
      list(column = "cyl", direction = "asc"),
      list(column = "mpg", direction = "desc"),
      list(column = "hp", direction = "asc")
    )), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "arrange_multi"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got all rows
  expect_equal(nrow(result_data), nrow(mtcars))

  # Verify primary sort: cyl ascending
  # All cyl values should be in ascending order when looking at unique cyl values
  cyl_order <- unique(result_data$cyl)
  expect_true(all(diff(cyl_order) >= 0))

  # Verify secondary sort within each cyl group: mpg descending
  for (cyl_val in unique(result_data$cyl)) {
    cyl_group <- result_data[result_data$cyl == cyl_val, ]
    # Within this cyl group, mpg should be descending
    if (nrow(cyl_group) > 1) {
      expect_true(all(diff(cyl_group$mpg) <= 0))
    }
  }

  # Verify tertiary sort within each cyl+mpg combination: hp ascending
  for (cyl_val in unique(result_data$cyl)) {
    cyl_group <- result_data[result_data$cyl == cyl_val, ]
    for (mpg_val in unique(cyl_group$mpg)) {
      sub_group <- cyl_group[cyl_group$mpg == mpg_val, ]
      if (nrow(sub_group) > 1) {
        expect_true(all(diff(sub_group$hp) >= 0))
      }
    }
  }

  cleanup_test_app(app_dir, app)
})
