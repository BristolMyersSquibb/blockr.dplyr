# Tests for summarize_nocode_block
# This file tests the no-code interface for summarization

# Basic block creation tests ----

test_that("summarize_nocode_block creates successfully", {
  # Test default creation
  block1 <- new_summarize_nocode_block()
  expect_s3_class(block1, "summarize_nocode_block")
  expect_s3_class(block1, "transform_block")
  expect_s3_class(block1, "block")

  # Test with custom summaries
  block2 <- new_summarize_nocode_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg"))
  )
  expect_s3_class(block2, "summarize_nocode_block")

  # Test with grouping
  block3 <- new_summarize_nocode_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by = "cyl"
  )
  expect_s3_class(block3, "summarize_nocode_block")
})

# Restorability tests - Custom labels ----

test_that("summarize_nocode_block restores single custom label", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Create block with custom label
  blk <- new_summarize_nocode_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg"))
  )

  # Verify the block works and preserves the label
  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "cyl")])),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))
      expect_true(is.reactive(result$state$summaries))

      # Verify the label is preserved in state
      summaries <- result$state$summaries()
      expect_true("avg_mpg" %in% names(summaries))
      expect_equal(summaries$avg_mpg$func, "mean")
      expect_equal(summaries$avg_mpg$col, "mpg")
    }
  )
})

test_that("summarize_nocode_block restores multiple custom labels", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Create block with multiple custom labels (like in the bug report)
  blk <- new_summarize_nocode_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      max_hp = list(func = "max", col = "hp"),
      min_wt = list(func = "min", col = "wt")
    )
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "hp", "wt", "cyl")])),
    {
      session$flushReact()

      result <- session$returned
      summaries <- result$state$summaries()

      # Verify all three labels are preserved in the correct order
      expect_equal(names(summaries), c("avg_mpg", "max_hp", "min_wt"))

      # Verify each summary specification
      expect_equal(summaries$avg_mpg$func, "mean")
      expect_equal(summaries$avg_mpg$col, "mpg")

      expect_equal(summaries$max_hp$func, "max")
      expect_equal(summaries$max_hp$col, "hp")

      expect_equal(summaries$min_wt$func, "min")
      expect_equal(summaries$min_wt$col, "wt")
    }
  )
})

test_that("summarize_nocode_block restores labels with special characters", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Test labels with spaces, dots, and parentheses (common in clinical data)
  blk <- new_summarize_nocode_block(
    summaries = list(
      ".variable_2" = list(func = "mean", col = "mpg"),
      "DEUC 6 mg N = 336" = list(func = "max", col = "hp"),
      "PBO N = 334" = list(func = "min", col = "wt")
    )
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "hp", "wt")])),
    {
      session$flushReact()

      result <- session$returned
      summaries <- result$state$summaries()

      # Verify labels with special characters are preserved exactly
      expect_equal(
        names(summaries),
        c(".variable_2", "DEUC 6 mg N = 336", "PBO N = 334")
      )

      # Verify the specifications are correct
      expect_equal(summaries[[".variable_2"]]$func, "mean")
      expect_equal(summaries[["DEUC 6 mg N = 336"]]$func, "max")
      expect_equal(summaries[["PBO N = 334"]]$func, "min")
    }
  )
})

# Custom function tests ----

test_that("summarize_nocode_block works with namespaced functions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Test with dplyr namespaced functions
  blk <- new_summarize_nocode_block(
    summaries = list(
      first_val = list(func = "dplyr::first", col = "mpg"),
      last_val = list(func = "dplyr::last", col = "hp")
    )
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "hp")])),
    {
      session$flushReact()

      result <- session$returned
      summaries <- result$state$summaries()

      # Verify namespaced functions are preserved
      expect_equal(summaries$first_val$func, "dplyr::first")
      expect_equal(summaries$last_val$func, "dplyr::last")
    }
  )
})

test_that("summarize_nocode_block works with custom package functions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Test with custom namespaced functions (like blockr.topline::paste_paren)
  blk <- new_summarize_nocode_block(
    summaries = list(
      custom_stat = list(func = "blockr.topline::paste_paren", col = "mpg")
    )
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "hp")])),
    {
      session$flushReact()

      result <- session$returned
      summaries <- result$state$summaries()

      # Verify custom function is preserved with full namespace
      expect_equal(summaries$custom_stat$func, "blockr.topline::paste_paren")
      expect_equal(summaries$custom_stat$col, "mpg")
    }
  )
})

test_that("summarize_nocode_block works with dplyr::n function", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Test with dplyr::n which doesn't need a column
  blk <- new_summarize_nocode_block(
    summaries = list(
      count = list(func = "dplyr::n", col = "")
    )
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "hp")])),
    {
      session$flushReact()

      result <- session$returned
      summaries <- result$state$summaries()

      # Verify n() function works with empty column
      expect_equal(summaries$count$func, "dplyr::n")
      expect_equal(summaries$count$col, "")
    }
  )
})

# Grouping tests ----

test_that("summarize_nocode_block restores grouping with custom labels", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Test with both custom labels and grouping
  blk <- new_summarize_nocode_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      count = list(func = "dplyr::n", col = "")
    ),
    by = "cyl"
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "cyl")])),
    {
      session$flushReact()

      result <- session$returned

      # Verify summaries are preserved
      summaries <- result$state$summaries()
      expect_equal(names(summaries), c("avg_mpg", "count"))

      # Verify grouping is preserved
      by_selection <- result$state$by()
      expect_equal(by_selection, "cyl")
    }
  )
})

test_that("summarize_nocode_block restores multiple grouping columns", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Test with multiple grouping columns
  blk <- new_summarize_nocode_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by = c("cyl", "gear")
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "cyl", "gear")])),
    {
      session$flushReact()

      result <- session$returned
      by_selection <- result$state$by()

      # Verify multiple grouping columns are preserved
      expect_equal(by_selection, c("cyl", "gear"))
    }
  )
})

# Data transformation tests ----

test_that("summarize_nocode_block produces correct output with custom labels", {
  skip_if_not_installed("dplyr")

  block <- new_summarize_nocode_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      max_hp = list(func = "max", col = "hp")
    )
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify output has custom column names
      expect_equal(nrow(result), 1)
      expect_true("avg_mpg" %in% names(result))
      expect_true("max_hp" %in% names(result))

      # Verify calculations are correct
      expect_equal(result$avg_mpg, mean(mtcars$mpg), tolerance = 0.0001)
      expect_equal(result$max_hp, max(mtcars$hp))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize_nocode_block produces correct grouped output", {
  skip_if_not_installed("dplyr")

  block <- new_summarize_nocode_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      count = list(func = "dplyr::n", col = "")
    ),
    by = "cyl"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify grouped output
      expect_equal(nrow(result), length(unique(mtcars$cyl)))
      expect_true("cyl" %in% names(result))
      expect_true("avg_mpg" %in% names(result))
      expect_true("count" %in% names(result))

      # Verify one group's calculation
      cyl_6_data <- mtcars[mtcars$cyl == 6, ]
      cyl_6_result <- result[result$cyl == 6, ]
      if (nrow(cyl_6_result) > 0) {
        expect_equal(
          cyl_6_result$avg_mpg,
          mean(cyl_6_data$mpg),
          tolerance = 0.0001
        )
        expect_equal(cyl_6_result$count, nrow(cyl_6_data))
      }
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# Edge case tests ----

test_that("summarize_nocode_block handles empty summaries", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Default empty summaries should use count
  blk <- new_summarize_nocode_block(
    summaries = list()
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg")])),
    {
      session$flushReact()

      # Should not error
      expect_true(is.reactive(session$returned$expr))
    }
  )
})

test_that("summarize_nocode_block handles NULL grouping", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  blk <- new_summarize_nocode_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by = NULL
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg")])),
    {
      session$flushReact()

      result <- session$returned
      by_selection <- result$state$by()

      # NULL should be handled (likely converted to character(0))
      expect_true(is.null(by_selection) || length(by_selection) == 0)
    }
  )
})

test_that("summarize_nocode_block handles empty grouping", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  blk <- new_summarize_nocode_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by = character(0)
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg")])),
    {
      session$flushReact()

      result <- session$returned
      by_selection <- result$state$by()

      # Empty character vector should be preserved
      expect_equal(length(by_selection), 0)
    }
  )
})

# Parse function tests ----

test_that("parse_summarize_nocode generates correct expression", {
  # Test basic summary
  expr <- parse_summarize_nocode(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by_selection = character()
  )

  expect_type(expr, "language")
  code <- deparse(expr)
  expect_true(any(grepl("avg_mpg", code)))
  expect_true(any(grepl("mean\\(mpg\\)", code)))
})

test_that("parse_summarize_nocode handles grouping", {
  expr <- parse_summarize_nocode(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by_selection = "cyl"
  )

  expect_type(expr, "language")
  code <- paste(deparse(expr), collapse = " ")
  expect_true(grepl('\\.by', code))
})

test_that("parse_summarize_nocode handles multiple summaries", {
  expr <- parse_summarize_nocode(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      max_hp = list(func = "max", col = "hp"),
      count = list(func = "dplyr::n", col = "")
    ),
    by_selection = character()
  )

  expect_type(expr, "language")
  code <- paste(deparse(expr), collapse = " ")
  expect_true(grepl("avg_mpg", code))
  expect_true(grepl("max_hp", code))
  expect_true(grepl("count", code))
})

test_that("parse_summarize_nocode handles backticks in names", {
  # Test with names that need backticks
  expr <- parse_summarize_nocode(
    summaries = list(
      "DEUC 6 mg N = 336" = list(func = "mean", col = "mpg")
    ),
    by_selection = character()
  )

  expect_type(expr, "language")
  code <- paste(deparse(expr), collapse = " ")
  # Should contain the name (possibly with backticks if needed)
  expect_true(grepl("DEUC", code))
})

test_that("parse_summarize_nocode handles empty summaries", {
  expr <- parse_summarize_nocode(
    summaries = list(),
    by_selection = character()
  )

  expect_type(expr, "language")
  code <- paste(deparse(expr), collapse = " ")
  expect_true(grepl("dplyr::summarize", code))
})
