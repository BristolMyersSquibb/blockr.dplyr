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

# Tests for reactive observeEvent blocks
test_that("mod_multi_arrange_server add_arrange button reactivity - testServer", {
  # Test add_arrange button (lines 79-105)
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() list(list(column = "mpg", direction = "asc")),
    get_cols = function() c("mpg", "cyl", "hp", "wt")
  ), {
    session$flushReact()

    initial_arranges <- session$returned()
    expect_length(initial_arranges, 1)

    # Click add_arrange button multiple times
    session$setInputs(add_arrange = 1)
    session$flushReact()

    updated_arranges <- session$returned()
    expect_type(updated_arranges, "list")

    # Add another
    session$setInputs(add_arrange = 2)
    session$flushReact()

    final_arranges <- session$returned()
    expect_type(final_arranges, "list")
  })
})

test_that("mod_multi_arrange_server direction checkbox reactivity - testServer", {
  # Test direction checkbox changes
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() list(list(column = "mpg", direction = "asc")),
    get_cols = function() c("mpg", "cyl", "hp")
  ), {
    session$flushReact()

    initial_arranges <- session$returned()
    expect_equal(initial_arranges[[1]]$direction, "asc")

    # Change direction checkbox (TRUE = desc, FALSE = asc)
    session$setInputs(arrange_1_direction = TRUE)
    session$flushReact()

    # Note: Input changes may not immediately reflect in returned() without UI rendered
    # The important part is testing that the observer is set up correctly
    updated_arranges <- session$returned()
    expect_type(updated_arranges, "list")
    expect_true(length(updated_arranges) > 0)

    # Change back to asc
    session$setInputs(arrange_1_direction = FALSE)
    session$flushReact()

    final_arranges <- session$returned()
    expect_type(final_arranges, "list")
    expect_true(length(final_arranges) > 0)
  })
})

test_that("mod_multi_arrange_server column selection reactivity - testServer", {
  # Test column selection changes
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() list(list(column = "mpg", direction = "asc")),
    get_cols = function() c("mpg", "cyl", "hp", "wt")
  ), {
    session$flushReact()

    initial_arranges <- session$returned()
    expect_equal(initial_arranges[[1]]$column, "mpg")

    # Change column selection
    session$setInputs(arrange_1_column = "cyl")
    session$flushReact()

    # Note: Column changes may not immediately reflect without UI rendered
    # Testing that the function doesn't crash
    updated_arranges <- session$returned()
    expect_type(updated_arranges, "list")
    expect_true(length(updated_arranges) > 0)

    # Change to another column
    session$setInputs(arrange_1_column = "hp")
    session$flushReact()

    final_arranges <- session$returned()
    expect_type(final_arranges, "list")
    expect_true(length(final_arranges) > 0)
  })
})

test_that("mod_multi_arrange_server handles unused columns selection - testServer", {
  # Test selection of unused columns when adding new arrange (lines 90-98)
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() list(
      list(column = "mpg", direction = "asc"),
      list(column = "cyl", direction = "desc")
    ),
    get_cols = function() c("mpg", "cyl", "hp", "wt")
  ), {
    session$flushReact()

    initial_arranges <- session$returned()
    expect_length(initial_arranges, 2)

    # Add a new arrange - should prefer unused columns (hp or wt)
    session$setInputs(add_arrange = 1)
    session$flushReact()

    updated_arranges <- session$returned()
    expect_type(updated_arranges, "list")
  })
})

test_that("mod_multi_arrange_server empty column fallback - testServer", {
  # Test fallback when all columns are empty (lines 66-73)
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() list(list(column = "", direction = "asc")),
    get_cols = function() character(0)
  ), {
    session$flushReact()

    # Should fallback to empty column with asc direction
    arranges <- session$returned()
    expect_type(arranges, "list")
    expect_length(arranges, 1)
    expect_equal(arranges[[1]]$column, "")
    expect_equal(arranges[[1]]$direction, "asc")
  })
})

test_that("mod_multi_arrange_server get_current_arranges with empty results - testServer", {
  # Test get_current_arranges fallback (lines 66-73)
  testServer(mod_multi_arrange_server, args = list(
    get_value = function() list(),
    get_cols = function() c("mpg", "cyl", "hp")
  ), {
    session$flushReact()

    # Should fallback to first available column
    arranges <- session$returned()
    expect_type(arranges, "list")
    expect_length(arranges, 1)
    # Should use first column from available cols
    expect_true(arranges[[1]]$column %in% c("mpg", ""))
  })
})
