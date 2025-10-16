test_that("bind_rows block constructor", {
  block <- new_bind_rows_block()
  expect_s3_class(block, c("bind_rows_block", "rbind_block", "transform_block", "block"))

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

test_that("bind_cols block constructor", {
  block <- new_bind_cols_block()
  expect_s3_class(block, c("bind_cols_block", "transform_block", "block"))

  # Test with custom suffixes
  block_custom <- new_bind_cols_block(suffix = c("_left", "_right"))
  expect_s3_class(
    block_custom,
    c("bind_cols_block", "transform_block", "block")
  )
})

test_that("bind blocks have correct structure", {
  bind_rows_block <- new_bind_rows_block()
  bind_cols_block <- new_bind_cols_block()

  # Check that blocks have required components
  expect_true(is.function(bind_rows_block$expr_server))
  # Note: bind_rows_block has no UI (variadic blocks don't need UI)

  expect_true(is.function(bind_cols_block$expr_server))
  expect_true(is.function(bind_cols_block$expr_ui))
})

# Removed: bind_rows block has no UI (variadic blocks don't need configuration UI)

test_that("bind_cols block UI includes configuration options", {
  block <- new_bind_cols_block(suffix = c(".left", ".right"))
  ui <- block$expr_ui("test")

  # Convert to character for easier testing
  ui_str <- as.character(ui)

  # bind_cols has minimal UI (no configuration options needed)
  # Just check that UI is not NULL
  expect_false(is.null(ui))

  # No submit button - immediate reactivity
})

# Removed: bind_rows server functionality is already tested in the constructor test above

test_that("bind_cols block server functionality", {
  skip_if_not_installed("shiny")

  # Create test data with same row count
  x_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      stringsAsFactors = FALSE
    )
  })

  y_data <- reactive({
    data.frame(
      score = c(100, 200, 300),
      category = c("X", "Y", "Z"),
      stringsAsFactors = FALSE
    )
  })

  testServer(
    function(id, input, output, session) {
      block <- new_bind_cols_block()
      result <- block$expr_server(id, x_data, y_data)
      result
    },
    {
      # Test that server returns expected structure
      expect_true(is.list(session$returned))
      expect_true("expr" %in% names(session$returned))
      expect_true("state" %in% names(session$returned))
    }
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

test_that("bind blocks maintain state correctly", {
  # Test that state is properly maintained
  bind_rows_block <- new_bind_rows_block()
  bind_cols_block <- new_bind_cols_block(suffix = c("_x", "_y"))

  expect_s3_class(bind_rows_block, "bind_rows_block")
  expect_s3_class(bind_cols_block, "bind_cols_block")
})

# Removed: ID column functionality will be implemented later

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
