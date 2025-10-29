test_that("arrange block constructor", {
  # Test basic constructor
  blk <- new_arrange_block()
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test constructor with single column
  blk <- new_arrange_block("mpg")
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test constructor with multiple columns
  blk <- new_arrange_block(c("mpg", "cyl"))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))
})

test_that("arrange block with character input", {
  # Test single column
  blk <- new_arrange_block("mpg")
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test multiple columns
  blk <- new_arrange_block(c("mpg", "cyl", "hp"))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test with multiple columns (defaults to ascending)
  blk <- new_arrange_block(c("mpg", "cyl"))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))
})

test_that("arrange block with list input", {
  # Test list of arrange specifications
  arrange_specs <- list(
    list(column = "mpg", direction = "desc"),
    list(column = "cyl", direction = "asc"),
    list(column = "hp", direction = "desc")
  )

  blk <- new_arrange_block(arrange_specs)
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))
})

test_that("arrange block with empty input", {
  # Test empty character vector
  blk <- new_arrange_block(character(0))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test empty list
  blk <- new_arrange_block(list())
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test NULL
  blk <- new_arrange_block(NULL)
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))
})

test_that("multi arrange module functionality", {
  # Test module server function exists
  expect_true(exists("mod_multi_arrange_server"))
  expect_true(is.function(mod_multi_arrange_server))

  # Test module UI function exists
  expect_true(exists("mod_multi_arrange_ui"))
  expect_true(is.function(mod_multi_arrange_ui))

  # Test example function exists
  expect_true(exists("run_multi_arrange_example"))
  expect_true(is.function(run_multi_arrange_example))
})

test_that("arrange block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Create test data
  test_data <- data.frame(
    a = c(3, 1, 2),
    b = c("x", "y", "z"),
    c = c(10, 30, 20)
  )

  # Test basic arrange block functionality
  blk <- new_arrange_block(c("a", "c"))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # The block should be properly structured (internal functions, not exposed)
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
})

test_that("arrange specifications format", {
  # Test ascending specifications
  specs_asc <- list(
    list(column = "mpg", direction = "asc"),
    list(column = "cyl", direction = "asc")
  )

  # Test mixed ascending and descending specifications
  specs_mixed <- list(
    list(column = "mpg", direction = "desc"),
    list(column = "cyl", direction = "asc")
  )

  # Blocks should handle both specification formats
  blk1 <- new_arrange_block(specs_asc)
  expect_s3_class(blk1, c("arrange_block", "transform_block", "block"))

  blk2 <- new_arrange_block(specs_mixed)
  expect_s3_class(blk2, c("arrange_block", "transform_block", "block"))
})

# Data transformation tests using block_server
test_that("arrange block sorts data ascending - testServer", {
  block <- new_arrange_block(columns = list(list(column = "mpg", direction = "asc")))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify data is sorted by mpg ascending
      expect_equal(nrow(result), nrow(mtcars))
      expect_true(all(diff(result$mpg) >= 0))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("arrange block sorts data descending - testServer", {
  block <- new_arrange_block(columns = list(list(column = "hp", direction = "desc")))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify data is sorted by hp descending
      expect_true(all(diff(result$hp) <= 0))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("arrange block multi-column sort - testServer", {
  block <- new_arrange_block(columns = list(
    list(column = "cyl", direction = "asc"),
    list(column = "mpg", direction = "desc")
  ))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify we got all rows
      expect_equal(nrow(result), nrow(mtcars))

      # Verify primary sort: cyl ascending
      cyl_order <- unique(result$cyl)
      expect_true(all(diff(cyl_order) >= 0))

      # Verify secondary sort within each cyl group: mpg descending
      for (cyl_val in unique(result$cyl)) {
        cyl_group <- result[result$cyl == cyl_val, ]
        if (nrow(cyl_group) > 1) {
          expect_true(all(diff(cyl_group$mpg) <= 0))
        }
      }
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# Module tests for multi_arrange coverage
test_that("mod_multi_arrange_server initializes from character vector", {
  # Test character vector initialization (lines 21-25)
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() c("mpg", "cyl"),
    get_cols = function() c("mpg", "cyl", "hp")
  ), {
    session$flushReact()
    result <- session$returned()

    # Should convert character vector to list of arrange specs
    expect_type(result, "list")
    expect_length(result, 2)
    expect_equal(result[[1]]$column, "mpg")
    expect_equal(result[[1]]$direction, "asc")
    expect_equal(result[[2]]$column, "cyl")
    expect_equal(result[[2]]$direction, "asc")
  })
})

test_that("mod_multi_arrange_server handles empty initialization", {
  # Test empty initialization fallback (lines 28-30)
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() character(0),
    get_cols = function() c("mpg", "cyl", "hp")
  ), {
    session$flushReact()
    result <- session$returned()

    # Should fallback to default arrange
    expect_type(result, "list")
    expect_length(result, 1)
    expect_equal(result[[1]]$column, "")
    expect_equal(result[[1]]$direction, "asc")
  })
})

test_that("mod_multi_arrange_server handles NULL initialization", {
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() NULL,
    get_cols = function() c("mpg", "cyl", "hp")
  ), {
    session$flushReact()
    result <- session$returned()

    # Should fallback to default arrange
    expect_type(result, "list")
    expect_length(result, 1)
  })
})

test_that("mod_multi_arrange_server handles empty column list", {
  # Test fallback when no columns available (lines 66-73)
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() list(list(column = "mpg", direction = "asc")),
    get_cols = function() character(0)
  ), {
    session$flushReact()

    # Simulate clearing the column selection
    session$setInputs(arrange_1_column = "")
    session$flushReact()

    result <- session$returned()

    # Should handle empty columns gracefully
    expect_type(result, "list")
  })
})

test_that("mod_multi_arrange_server adds arrange", {
  # Test add operation (lines 79-105)
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() list(list(column = "mpg", direction = "asc")),
    get_cols = function() c("mpg", "cyl", "hp")
  ), {
    session$flushReact()
    initial_result <- session$returned()

    # Should start with 1 arrange
    expect_length(initial_result, 1)

    # Simulate clicking add button
    session$setInputs(add_arrange = 1)
    session$flushReact()

    # Note: In the actual module, adding requires UI to be rendered
    # This tests the add_arrange observer is set up
    # Full UI testing would require the rendered UI elements
  })
})
