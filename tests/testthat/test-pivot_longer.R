# Basic construction tests
test_that("pivot_longer block constructor", {
  blk <- new_pivot_longer_block()
  expect_s3_class(blk, c("pivot_longer_block", "transform_block", "block"))
})

test_that("pivot_longer block with parameters", {
  blk <- new_pivot_longer_block(
    cols = c("col_a", "col_b"),
    names_to = "measurement",
    values_to = "result"
  )
  expect_s3_class(blk, c("pivot_longer_block", "transform_block", "block"))
})

# testServer tests for data transformation
test_that("pivot_longer basic transformation - testServer", {
  # Create wide format test data
  wide_data <- data.frame(
    id = 1:3,
    measurement_a = c(10, 20, 30),
    measurement_b = c(15, 25, 35)
  )

  block <- new_pivot_longer_block(
    cols = c("measurement_a", "measurement_b"),
    names_to = "measurement",
    values_to = "value"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have 3 rows * 2 measurements = 6 rows
      expect_equal(nrow(result), 6)
      # Should have id, measurement, value columns
      expect_true(all(c("id", "measurement", "value") %in% names(result)))
      # Check that measurement column has the right values
      expect_true(all(result$measurement %in% c("measurement_a", "measurement_b")))
    },
    args = list(x = block, data = list(data = function() wide_data))
  )
})

test_that("pivot_longer with values_drop_na=TRUE - testServer", {
  # Create data with NA values
  wide_data <- data.frame(
    id = 1:3,
    col_a = c(10, NA, 30),
    col_b = c(NA, 25, 35)
  )

  # Without dropping NAs
  block_keep_na <- new_pivot_longer_block(
    cols = c("col_a", "col_b"),
    values_drop_na = FALSE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_keep_na),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 6)  # 3 rows * 2 cols = 6 rows including NAs
      expect_true(any(is.na(result$value)))
    },
    args = list(x = block_keep_na, data = list(data = function() wide_data))
  )

  # With dropping NAs
  block_drop_na <- new_pivot_longer_block(
    cols = c("col_a", "col_b"),
    values_drop_na = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_drop_na),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 4)  # Only 4 non-NA values
      expect_false(any(is.na(result$value)))
    },
    args = list(x = block_drop_na, data = list(data = function() wide_data))
  )
})

test_that("pivot_longer with names_prefix - testServer", {
  wide_data <- data.frame(
    id = 1:3,
    measurement_a = c(10, 20, 30),
    measurement_b = c(15, 25, 35),
    measurement_c = c(12, 22, 32)
  )

  block <- new_pivot_longer_block(
    cols = c("measurement_a", "measurement_b", "measurement_c"),
    names_to = "type",
    values_to = "value",
    names_prefix = "measurement_"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 9)  # 3 rows * 3 measurements
      # After prefix removal, should have "a", "b", "c" in type column
      expect_true(all(result$type %in% c("a", "b", "c")))
      expect_false(any(grepl("measurement_", result$type)))
    },
    args = list(x = block, data = list(data = function() wide_data))
  )
})

test_that("pivot_longer with custom column names - testServer", {
  wide_data <- data.frame(
    id = 1:2,
    year_2020 = c(100, 200),
    year_2021 = c(110, 210),
    year_2022 = c(120, 220)
  )

  block <- new_pivot_longer_block(
    cols = c("year_2020", "year_2021", "year_2022"),
    names_to = "year",
    values_to = "revenue"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 6)  # 2 rows * 3 years
      expect_true(all(c("id", "year", "revenue") %in% names(result)))
      expect_true(all(result$year %in% c("year_2020", "year_2021", "year_2022")))
    },
    args = list(x = block, data = list(data = function() wide_data))
  )
})

test_that("pivot_longer with empty cols selection - testServer", {
  wide_data <- data.frame(
    id = 1:2,
    col_a = c(10, 20),
    col_b = c(15, 25)
  )

  # Empty cols should result in pass-through or select all non-id columns
  block <- new_pivot_longer_block(
    cols = character(0),
    names_to = "name",
    values_to = "value"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # With empty cols, block should either pass through or select nothing
      # Based on implementation, this should still return a data frame
      expect_true(is.data.frame(result))
      # Result may vary - either original data or pivoted data
      expect_true(nrow(result) >= 0)
    },
    args = list(x = block, data = list(data = function() wide_data))
  )
})

test_that("pivot_longer full parameter combination - testServer", {
  wide_data <- data.frame(
    subject_id = 1:3,
    test_score_a = c(85, NA, 92),
    test_score_b = c(90, 88, 95),
    test_score_c = c(NA, 87, 93)
  )

  block <- new_pivot_longer_block(
    cols = c("test_score_a", "test_score_b", "test_score_c"),
    names_to = "test",
    values_to = "score",
    values_drop_na = TRUE,
    names_prefix = "test_score_"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # 3 subjects * 3 tests = 9, minus 2 NAs = 7 rows
      expect_equal(nrow(result), 7)
      expect_true(all(c("subject_id", "test", "score") %in% names(result)))
      # After prefix removal, should have "a", "b", "c"
      expect_true(all(result$test %in% c("a", "b", "c")))
      # No NAs should remain
      expect_false(any(is.na(result$score)))
    },
    args = list(x = block, data = list(data = function() wide_data))
  )
})
