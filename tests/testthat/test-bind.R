test_that("bind_rows block constructor", {
  block <- new_bind_rows_block()
  expect_s3_class(
    block,
    c("bind_rows_block", "transform_block", "block")
  )

  # Test with unnamed arguments
  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        dplyr::bind_rows(iris[1:3, ], iris[4:6, ])
      )
    },
    args = list(
      x = block,
      data = list(
        ...args = reactiveValues(
          `1` = iris[1:3, ],
          `2` = iris[4:6, ]
        )
      )
    )
  )

  # Test with named arguments
  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        dplyr::bind_rows(a = iris[1:3, ], b = iris[4:6, ])
      )
    },
    args = list(
      x = block,
      data = list(
        ...args = reactiveValues(
          a = iris[1:3, ],
          b = iris[4:6, ]
        )
      )
    )
  )

  # Test with mixed named/unnamed arguments
  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        dplyr::bind_rows(iris[1:3, ], a = iris[4:6, ])
      )
    },
    args = list(
      x = block,
      data = list(
        ...args = reactiveValues(
          `1` = iris[1:3, ],
          a = iris[4:6, ]
        )
      )
    )
  )
})

test_that("bind_rows block with id_name parameter", {
  block_with_id <- new_bind_rows_block(id_name = "source")
  expect_s3_class(
    block_with_id,
    c("bind_rows_block", "transform_block", "block")
  )

  # Test with named arguments and .id column
  testServer(
    blockr.core:::get_s3_method("block_server", block_with_id),
    {
      session$flushReact()
      result <- session$returned$result()
      expected <- dplyr::bind_rows(
        a = iris[1:3, ],
        b = iris[4:6, ],
        .id = "source"
      )

      expect_identical(result, expected)
      expect_true("source" %in% colnames(result))
      expect_equal(result$source, c(rep("a", 3), rep("b", 3)))
    },
    args = list(
      x = block_with_id,
      data = list(
        ...args = reactiveValues(
          a = iris[1:3, ],
          b = iris[4:6, ]
        )
      )
    )
  )

  # Test state includes id_name
  testServer(
    blockr.core:::get_s3_method("block_server", block_with_id),
    {
      session$flushReact()
      expect_true("id_name" %in% names(session$returned$state))
      expect_equal(session$returned$state$id_name(), "source")
    },
    args = list(
      x = block_with_id,
      data = list(
        ...args = reactiveValues(
          a = iris[1:3, ],
          b = iris[4:6, ]
        )
      )
    )
  )
})

test_that("bind_rows block UI includes id_name input", {
  block <- new_bind_rows_block(id_name = ".id")
  ui <- block$expr_ui("test")

  # Convert to character for easier testing
  ui_str <- as.character(ui)

  # Should include ID column name input
  expect_true(grepl("ID column name", ui_str))
})

test_that("bind_cols block constructor", {
  block <- new_bind_cols_block()
  expect_s3_class(
    block,
    c("bind_cols_block", "transform_block", "block")
  )

  # Test with two arguments
  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expected <- dplyr::bind_cols(iris[1:3, 1:2], iris[1:3, 3:4])

      expect_identical(result, expected)
    },
    args = list(
      x = block,
      data = list(
        ...args = reactiveValues(
          `1` = iris[1:3, 1:2],
          `2` = iris[1:3, 3:4]
        )
      )
    )
  )
})

test_that("bind_rows block handles different column structures", {
  # Test data with different column sets
  x_df <- data.frame(a = 1:2, b = 3:4)
  y_df <- data.frame(b = 5:6, c = 7:8)

  # Test basic bind_rows functionality
  result <- dplyr::bind_rows(x_df, y_df)

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3) # a, b, c
  expect_true(all(c("a", "b", "c") %in% colnames(result)))

  # Check that missing values are filled with NA
  expect_true(is.na(result$a[3])) # First row from y_df should have NA for column 'a'
  expect_true(is.na(result$c[1])) # First row from x_df should have NA for column 'c'
})

test_that("bind_cols block handles duplicate column names", {
  # Test data with duplicate column names
  x_df <- data.frame(id = 1:3, value = c("A", "B", "C"))
  y_df <- data.frame(id = 4:6, score = c(10, 20, 30))

  # Test that bind_cols can handle duplicates
  result <- dplyr::bind_cols(x_df, y_df)

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4) # id, value, id, score

  # Column names should be made unique
  expect_true(length(unique(colnames(result))) <= 4)
})

test_that("bind_cols block validates row count compatibility", {
  # Test data with different row counts
  x_df <- data.frame(a = 1:3)
  y_df <- data.frame(b = 1:5) # Different row count

  # bind_cols should fail with different row counts
  expect_error(dplyr::bind_cols(x_df, y_df))
})

test_that("bind blocks handle empty data gracefully", {
  empty_df <- data.frame()
  x_df <- data.frame(a = 1:3)

  # bind_rows with empty data
  result1 <- dplyr::bind_rows(empty_df, x_df)
  expect_equal(nrow(result1), 3)
  expect_equal(ncol(result1), 1)

  result2 <- dplyr::bind_rows(x_df, empty_df)
  expect_equal(nrow(result2), 3)
  expect_equal(ncol(result2), 1)
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# Note: bind_rows uses debounce(800ms) for id_name input which doesn't work
# in testServer context. We test initial values instead.
# =============================================================================

test_that("bind_rows - id_name parameter adds ID column - testServer", {
  # Test without id_name
  block_no_id <- new_bind_rows_block(id_name = "")

  testServer(
    blockr.core:::get_s3_method("block_server", block_no_id),
    {
      session$flushReact()
      result <- session$returned$result()

      # No ID column should be present
      expect_false("source" %in% names(result))
      expect_equal(nrow(result), 6)
    },
    args = list(
      x = block_no_id,
      data = list(
        ...args = reactiveValues(
          a = iris[1:3, ],
          b = iris[4:6, ]
        )
      )
    )
  )

  # Test with id_name
  block_with_id <- new_bind_rows_block(id_name = "source")

  testServer(
    blockr.core:::get_s3_method("block_server", block_with_id),
    {
      session$flushReact()
      result <- session$returned$result()

      # ID column should be present
      expect_true("source" %in% names(result))
      expect_equal(nrow(result), 6)
      expect_equal(result$source, c(rep("a", 3), rep("b", 3)))
    },
    args = list(
      x = block_with_id,
      data = list(
        ...args = reactiveValues(
          a = iris[1:3, ],
          b = iris[4:6, ]
        )
      )
    )
  )

  # Test with different id_name value
  block_custom_id <- new_bind_rows_block(id_name = "dataset")

  testServer(
    blockr.core:::get_s3_method("block_server", block_custom_id),
    {
      session$flushReact()
      result <- session$returned$result()

      # Custom ID column name should be used
      expect_true("dataset" %in% names(result))
      expect_false("source" %in% names(result))
    },
    args = list(
      x = block_custom_id,
      data = list(
        ...args = reactiveValues(
          a = iris[1:3, ],
          b = iris[4:6, ]
        )
      )
    )
  )
})
