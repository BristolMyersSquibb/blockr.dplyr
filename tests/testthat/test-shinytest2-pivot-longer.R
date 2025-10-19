test_that("pivot_longer block basic functionality", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create wide format test data
  test_data_code <- "test_data <- data.frame(
    id = 1:3,
    measurement_a = c(10, 20, 30),
    measurement_b = c(15, 25, 35),
    measurement_c = c(12, 22, 32)
  )"

  # Test basic pivot longer
  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_pivot_longer_block(
      cols = c("measurement_a", "measurement_b", "measurement_c"),
      names_to = "metric",
      values_to = "value"
    ), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "pivot_longer_basic")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))

  # Should have 9 rows (3 original rows × 3 pivoted columns)
  expect_equal(nrow(result_data), 9)

  # Should have id, metric, and value columns
  expect_true("id" %in% names(result_data))
  expect_true("metric" %in% names(result_data))
  expect_true("value" %in% names(result_data))

  # Original pivoted columns should not be present
  expect_false("measurement_a" %in% names(result_data))
  expect_false("measurement_b" %in% names(result_data))
  expect_false("measurement_c" %in% names(result_data))

  # Verify metric column contains the original column names
  expect_true(all(c("measurement_a", "measurement_b", "measurement_c") %in% result_data$metric))

  cleanup_test_app(app_dir, app)
})

test_that("pivot_longer block with names_prefix parameter", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test data with prefix in column names
  test_data_code <- "test_data <- data.frame(
    id = 1:2,
    col_x = c(10, 20),
    col_y = c(15, 25),
    col_z = c(12, 22)
  )"

  # Test pivot longer with names_prefix to remove "col_"
  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_pivot_longer_block(
      cols = c("col_x", "col_y", "col_z"),
      names_to = "type",
      values_to = "measurement",
      names_prefix = "col_"
    ), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "pivot_longer_prefix")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify prefix was removed from metric names
  expect_true(all(c("x", "y", "z") %in% result_data$type))
  expect_false(any(grepl("^col_", result_data$type)))

  # Verify column names
  expect_true("type" %in% names(result_data))
  expect_true("measurement" %in% names(result_data))

  cleanup_test_app(app_dir, app)
})

test_that("pivot_longer block with values_drop_na parameter", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test data with NA values
  test_data_code <- "test_data <- data.frame(
    id = 1:3,
    a = c(10, NA, 30),
    b = c(15, 25, NA)
  )"

  # Test pivot longer with values_drop_na = TRUE
  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_pivot_longer_block(
      cols = c("a", "b"),
      names_to = "metric",
      values_to = "value",
      values_drop_na = TRUE
    ), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "pivot_longer_dropna")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 4 rows instead of 6 (2 NA rows dropped)
  expect_equal(nrow(result_data), 4)

  # No NA values should be present
  expect_false(any(is.na(result_data$value)))

  cleanup_test_app(app_dir, app)
})

test_that("pivot_longer block with mtcars (real-world example)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Use mtcars and pivot performance metrics
  app_dir <- create_test_app(
    block_code = 'serve(new_pivot_longer_block(
      cols = c("mpg", "hp", "wt"),
      names_to = "metric",
      values_to = "value"
    ), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "pivot_longer_mtcars")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 96 rows (32 cars × 3 metrics)
  expect_equal(nrow(result_data), 96)

  # Verify correct columns
  expect_true(all(c("cyl", "disp", "drat", "qsec", "vs", "am", "gear", "carb") %in% names(result_data)))
  expect_true("metric" %in% names(result_data))
  expect_true("value" %in% names(result_data))

  # Verify metrics
  expect_setequal(unique(result_data$metric), c("mpg", "hp", "wt"))

  cleanup_test_app(app_dir, app)
})

test_that("pivot_longer block full restorability - all parameters", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test all restorable parameters together:
  # - Multiple columns (cols)
  # - Custom names_to column name
  # - Custom values_to column name
  # - names_prefix to clean column names
  # - values_drop_na to remove NAs
  # This is the comprehensive restorability test

  test_data_code <- "test_data <- data.frame(
    patient_id = 1:4,
    measure_temp = c(98.6, NA, 99.1, 97.8),
    measure_bp = c(120, 130, NA, 115),
    measure_hr = c(72, 80, 75, 68)
  )"

  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_pivot_longer_block(
      cols = c("measure_temp", "measure_bp", "measure_hr"),
      names_to = "vital_sign",
      values_to = "reading",
      names_prefix = "measure_",
      values_drop_na = TRUE
    ), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "pivot_longer_full_restore")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify cols parameter worked - original columns should be gone
  expect_false("measure_temp" %in% names(result_data))
  expect_false("measure_bp" %in% names(result_data))
  expect_false("measure_hr" %in% names(result_data))

  # Verify names_to parameter worked
  expect_true("vital_sign" %in% names(result_data))
  expect_false("metric" %in% names(result_data)) # Should not use default name

  # Verify values_to parameter worked
  expect_true("reading" %in% names(result_data))
  expect_false("value" %in% names(result_data)) # Should not use default name

  # Verify names_prefix parameter worked - prefix removed
  expect_setequal(unique(result_data$vital_sign), c("temp", "bp", "hr"))
  expect_false(any(grepl("^measure_", result_data$vital_sign)))

  # Verify values_drop_na parameter worked
  # Should have 10 rows (4 patients × 3 metrics - 2 NAs)
  expect_equal(nrow(result_data), 10)
  expect_false(any(is.na(result_data$reading)))

  # Verify patient_id column is preserved
  expect_true("patient_id" %in% names(result_data))

  cleanup_test_app(app_dir, app)
})
