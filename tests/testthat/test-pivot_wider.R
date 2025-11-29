# Basic construction tests
test_that("pivot_wider block constructor", {
  blk <- new_pivot_wider_block()
  expect_s3_class(blk, c("pivot_wider_block", "transform_block", "block"))
})

test_that("pivot_wider block with parameters", {
  blk <- new_pivot_wider_block(
    names_from = "type",
    values_from = "value",
    names_prefix = "col_"
  )
  expect_s3_class(blk, c("pivot_wider_block", "transform_block", "block"))
})

# testServer tests for data transformation
test_that("pivot_wider basic transformation with names_from and values_from - testServer", {
  # Create long format test data
  long_data <- data.frame(
    id = rep(1:3, each = 2),
    measurement_type = rep(c("a", "b"), 3),
    value = c(10, 15, 20, 25, 30, 35)
  )

  block <- new_pivot_wider_block(
    names_from = "measurement_type",
    values_from = "value"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have 3 rows (one per id)
      expect_equal(nrow(result), 3)
      # Should have id, a, b columns
      expect_true(all(c("id", "a", "b") %in% names(result)))
      # Check values
      expect_equal(result$a, c(10, 20, 30))
      expect_equal(result$b, c(15, 25, 35))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

test_that("pivot_wider with id_cols specified - testServer", {
  long_data <- data.frame(
    subject = rep(1:2, each = 3),
    time = rep(c("t1", "t2", "t3"), 2),
    measurement = c("height", "weight", "age", "height", "weight", "age"),
    value = c(170, 70, 25, 165, 65, 30)
  )

  block <- new_pivot_wider_block(
    names_from = "measurement",
    values_from = "value",
    id_cols = c("subject", "time")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have 6 rows (2 subjects * 3 time points)
      expect_equal(nrow(result), 6)
      # Should have subject, time, height, weight, age columns
      expect_true(all(c("subject", "time", "height", "weight", "age") %in% names(result)))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

test_that("pivot_wider with values_fill for missing combinations - testServer", {
  # Data with missing combinations
  long_data <- data.frame(
    id = c(1, 1, 2),
    type = c("a", "b", "a"),
    value = c(10, 15, 20)
  )

  block <- new_pivot_wider_block(
    names_from = "type",
    values_from = "value",
    values_fill = "0"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have 2 rows (one per id)
      expect_equal(nrow(result), 2)
      # Missing combination (id=2, type=b) should be filled with 0
      expect_equal(result$a, c(10, 20))
      expect_equal(result$b, c(15, 0))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

test_that("pivot_wider with names_prefix - testServer", {
  long_data <- data.frame(
    id = rep(1:2, each = 2),
    measurement = rep(c("temp", "pressure"), 2),
    value = c(98.6, 120, 99.1, 115)
  )

  block <- new_pivot_wider_block(
    names_from = "measurement",
    values_from = "value",
    names_prefix = "measure_"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 2)
      # Column names should have prefix
      expect_true(all(c("id", "measure_temp", "measure_pressure") %in% names(result)))
      expect_false("temp" %in% names(result))
      expect_false("pressure" %in% names(result))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

test_that("pivot_wider with names_sep for multiple names_from columns - testServer", {
  long_data <- data.frame(
    id = rep(1:2, each = 4),
    category = rep(c("A", "A", "B", "B"), 2),
    type = rep(c("x", "y"), 4),
    value = 1:8
  )

  block <- new_pivot_wider_block(
    names_from = c("category", "type"),
    values_from = "value",
    names_sep = "."
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 2)
      # Should have columns with separator: A.x, A.y, B.x, B.y
      expect_true(all(c("id", "A.x", "A.y", "B.x", "B.y") %in% names(result)))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

test_that("pivot_wider with all parameters combined - testServer", {
  long_data <- data.frame(
    subject_id = rep(1:2, each = 3),
    test = rep(c("math", "science", "english"), 2),
    score = c(85, 90, 88, 92, 87, 95)
  )

  block <- new_pivot_wider_block(
    names_from = "test",
    values_from = "score",
    id_cols = "subject_id",
    values_fill = "0",
    names_prefix = "score_",
    names_sep = "_"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 2)
      # Should have prefixed column names
      expect_true(all(c("subject_id", "score_math", "score_science", "score_english") %in% names(result)))
      # Check values
      expect_equal(result$score_math, c(85, 92))
      expect_equal(result$score_science, c(90, 87))
      expect_equal(result$score_english, c(88, 95))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

test_that("pivot_wider with empty names_from - testServer", {
  long_data <- data.frame(
    id = 1:3,
    value = c(10, 20, 30)
  )

  block <- new_pivot_wider_block(
    names_from = character(),
    values_from = "value"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # With empty names_from, should return original data unchanged
      # or show a notification and return data
      expect_true(is.data.frame(result))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("pivot_wider - input values_fill changes missing value handling - testServer", {
  # Data with missing combinations
  long_data <- data.frame(
    id = c(1, 1, 2),
    type = c("a", "b", "a"),
    value = c(10, 15, 20)
  )

  block <- new_pivot_wider_block(
    names_from = "type",
    values_from = "value",
    values_fill = ""
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result - NA for missing (id=2, type=b)
      result <- session$returned$result()
      expect_true(is.data.frame(result))
      expect_true(is.na(result$b[result$id == 2]))

      # Change values_fill to 0
      expr$setInputs(values_fill = "0")
      session$flushReact()
      result <- session$returned$result()
      # Missing should now be 0
      expect_equal(result$b[result$id == 2], 0)
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

test_that("pivot_wider - input names_prefix changes column names - testServer", {
  long_data <- data.frame(
    id = rep(1:2, each = 2),
    type = rep(c("a", "b"), 2),
    value = c(10, 15, 20, 25)
  )

  block <- new_pivot_wider_block(
    names_from = "type",
    values_from = "value",
    names_prefix = ""
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result - no prefix
      result <- session$returned$result()
      expect_true(all(c("id", "a", "b") %in% names(result)))

      # Add names_prefix
      expr$setInputs(names_prefix = "col_")
      session$flushReact()
      result <- session$returned$result()
      # Columns should have prefix
      expect_true(all(c("id", "col_a", "col_b") %in% names(result)))
      expect_false("a" %in% names(result))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})

test_that("pivot_wider - input names_sep changes column name separator - testServer", {
  long_data <- data.frame(
    id = rep(1:2, each = 4),
    category = rep(c("A", "A", "B", "B"), 2),
    type = rep(c("x", "y"), 4),
    value = 1:8
  )

  block <- new_pivot_wider_block(
    names_from = c("category", "type"),
    values_from = "value",
    names_sep = "_"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result with "_" separator
      result <- session$returned$result()
      expect_true(all(c("A_x", "A_y", "B_x", "B_y") %in% names(result)))

      # Change separator to "."
      expr$setInputs(names_sep = ".")
      session$flushReact()
      result <- session$returned$result()
      expect_true(all(c("A.x", "A.y", "B.x", "B.y") %in% names(result)))
      expect_false("A_x" %in% names(result))
    },
    args = list(x = block, data = list(data = function() long_data))
  )
})
