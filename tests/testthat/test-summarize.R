# Basic construction tests
test_that("summarize block constructor", {
  blk <- new_summarize_block()
  expect_s3_class(blk, c("summarize_block", "transform_block", "block"))
})

test_that("summarize block with parameters", {
  blk <- new_summarize_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      max_hp = list(func = "max", col = "hp")
    ),
    by = "cyl"
  )
  expect_s3_class(blk, c("summarize_block", "transform_block", "block"))
})

# testServer tests for data transformation
test_that("summarize block default count - testServer", {
  # Default is count = n()
  block <- new_summarize_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("count" %in% names(result))
      expect_equal(result$count, nrow(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block mean function - testServer", {
  block <- new_summarize_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("avg_mpg" %in% names(result))
      expect_equal(result$avg_mpg, mean(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block median function - testServer", {
  block <- new_summarize_block(
    summaries = list(med_mpg = list(func = "stats::median", col = "mpg"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("med_mpg" %in% names(result))
      expect_equal(result$med_mpg, median(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block sum function - testServer", {
  block <- new_summarize_block(
    summaries = list(total_hp = list(func = "sum", col = "hp"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("total_hp" %in% names(result))
      expect_equal(result$total_hp, sum(mtcars$hp))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block min/max functions - testServer", {
  block <- new_summarize_block(
    summaries = list(
      min_mpg = list(func = "min", col = "mpg"),
      max_mpg = list(func = "max", col = "mpg")
    )
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("min_mpg" %in% names(result))
      expect_true("max_mpg" %in% names(result))
      expect_equal(result$min_mpg, min(mtcars$mpg))
      expect_equal(result$max_mpg, max(mtcars$mpg))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block sd function - testServer", {
  block <- new_summarize_block(
    summaries = list(sd_mpg = list(func = "stats::sd", col = "mpg"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("sd_mpg" %in% names(result))
      expect_equal(result$sd_mpg, sd(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block n_distinct function - testServer", {
  block <- new_summarize_block(
    summaries = list(unique_cyl = list(func = "dplyr::n_distinct", col = "cyl"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("unique_cyl" %in% names(result))
      expect_equal(result$unique_cyl, dplyr::n_distinct(mtcars$cyl))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block first/last functions - testServer", {
  block <- new_summarize_block(
    summaries = list(
      first_mpg = list(func = "dplyr::first", col = "mpg"),
      last_mpg = list(func = "dplyr::last", col = "mpg")
    )
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("first_mpg" %in% names(result))
      expect_true("last_mpg" %in% names(result))
      expect_equal(result$first_mpg, dplyr::first(mtcars$mpg))
      expect_equal(result$last_mpg, dplyr::last(mtcars$mpg))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block multiple summaries - testServer", {
  block <- new_summarize_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      total_hp = list(func = "sum", col = "hp"),
      count = list(func = "dplyr::n", col = "")
    )
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_equal(ncol(result), 3)
      expect_true(all(c("avg_mpg", "total_hp", "count") %in% names(result)))
      expect_equal(result$avg_mpg, mean(mtcars$mpg), tolerance = 0.0001)
      expect_equal(result$total_hp, sum(mtcars$hp))
      expect_equal(result$count, nrow(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block with single grouping column - testServer", {
  block <- new_summarize_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      count = list(func = "dplyr::n", col = "")
    ),
    by = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have one row per unique cyl value
      expect_equal(nrow(result), length(unique(mtcars$cyl)))
      expect_true("cyl" %in% names(result))
      expect_true("avg_mpg" %in% names(result))
      expect_true("count" %in% names(result))

      # Verify calculations for one group
      cyl_4_data <- mtcars[mtcars$cyl == 4, ]
      cyl_4_result <- result[result$cyl == 4, ]
      expect_equal(cyl_4_result$avg_mpg, mean(cyl_4_data$mpg), tolerance = 0.0001)
      expect_equal(cyl_4_result$count, nrow(cyl_4_data))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block with multiple grouping columns - testServer", {
  block <- new_summarize_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      count = list(func = "dplyr::n", col = "")
    ),
    by = c("cyl", "am")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have one row per unique cyl+am combination
      expected_groups <- nrow(unique(mtcars[, c("cyl", "am")]))
      expect_equal(nrow(result), expected_groups)
      expect_true(all(c("cyl", "am", "avg_mpg", "count") %in% names(result)))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# Parse function tests
test_that("parse_summarize_nocode handles single summary", {
  summaries <- list(avg_mpg = list(func = "mean", col = "mpg"))
  result <- parse_summarize_nocode(summaries)

  expect_type(result, "language")
  code <- deparse(result)
  expect_true(grepl("dplyr::summarize", code))
  expect_true(grepl("avg_mpg = mean\\(mpg\\)", code))
})

test_that("parse_summarize_nocode handles multiple summaries", {
  summaries <- list(
    avg_mpg = list(func = "mean", col = "mpg"),
    total_hp = list(func = "sum", col = "hp")
  )
  result <- parse_summarize_nocode(summaries)

  expect_type(result, "language")
  code <- paste(deparse(result), collapse = " ")
  expect_true(grepl("dplyr::summarize", code))
  expect_true(grepl("avg_mpg = mean\\(mpg\\)", code))
  expect_true(grepl("total_hp = sum\\(hp\\)", code))
})

test_that("parse_summarize_nocode handles n() function without column", {
  summaries <- list(count = list(func = "dplyr::n", col = ""))
  result <- parse_summarize_nocode(summaries)

  expect_type(result, "language")
  code <- deparse(result)
  expect_true(grepl("count = dplyr::n\\(\\)", code))
})

test_that("parse_summarize_nocode handles grouping", {
  summaries <- list(avg_mpg = list(func = "mean", col = "mpg"))
  result <- parse_summarize_nocode(summaries, by_selection = c("cyl", "am"))

  expect_type(result, "language")
  code <- paste(deparse(result), collapse = " ")
  expect_true(grepl('\\.by = c\\("cyl", "am"\\)', code))
})

test_that("parse_summarize_nocode handles empty summaries", {
  result <- parse_summarize_nocode(list())

  expect_type(result, "language")
  code <- deparse(result)
  expect_true(grepl("dplyr::summarize\\(data\\)", code))
})

test_that("parse_summarize_nocode handles non-syntactic column names", {
  summaries <- list(`avg 2024` = list(func = "mean", col = "2024 Sales"))
  result <- parse_summarize_nocode(summaries)

  expect_type(result, "language")
  code <- deparse(result)
  # Non-syntactic names should be backticked
  expect_true(grepl("`avg 2024`", code))
  expect_true(grepl("`2024 Sales`", code))
})

# apply_summarize_nocode validation tests
test_that("apply_summarize_nocode validates column existence", {
  summaries <- list(avg_nonexistent = list(func = "mean", col = "nonexistent_col"))

  # Should return early without crashing (column doesn't exist)
  # We can't easily check if reactiveVal was set outside reactive context,
  # but we can verify the function handles missing columns gracefully
  shiny::testServer(
    function(input, output, session) {
      r_expr_validated <- reactiveVal()
      r_summaries_validated <- reactiveVal()

      apply_summarize_nocode(
        mtcars,
        summaries,
        r_expr_validated,
        r_summaries_validated,
        by_selection = character(),
        session = session
      )

      # Should not have been set (column doesn't exist)
      expect_null(r_expr_validated())
    },
    {}
  )
})

test_that("apply_summarize_nocode handles empty summaries", {
  shiny::testServer(
    function(input, output, session) {
      r_expr_validated <- reactiveVal()
      r_summaries_validated <- reactiveVal()

      apply_summarize_nocode(
        mtcars,
        list(),
        r_expr_validated,
        r_summaries_validated,
        by_selection = character(),
        session = NULL
      )

      # Should set empty summarize expression
      expect_type(r_expr_validated(), "language")
    },
    {}
  )
})

test_that("apply_summarize_nocode validates new names exist", {
  shiny::testServer(
    function(input, output, session) {
      r_expr_validated <- reactiveVal()
      r_summaries_validated <- reactiveVal()

      # Summary with empty name
      summaries <- list()
      summaries[[""]] <- list(func = "mean", col = "mpg")

      apply_summarize_nocode(
        mtcars,
        summaries,
        r_expr_validated,
        r_summaries_validated,
        by_selection = character(),
        session = session
      )

      # Should return early without updating (empty name)
      expect_null(r_expr_validated())
    },
    {}
  )
})

# =============================================================================
# setInputs tests - verify different parameter values produce expected output
# Note: summarize block uses renderUI for dynamic summary rows, which makes
# setInputs testing complex. We test different initial parameter values instead.
# =============================================================================

test_that("summarize - different by parameter values produce different groupings - testServer", {
  # Test without grouping
  block_no_group <- new_summarize_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_no_group),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Without grouping, should have 1 row (global summary)
      expect_equal(nrow(result), 1)
      expect_equal(result$avg_mpg, mean(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block_no_group, data = list(data = function() mtcars))
  )

  # Test with single grouping column
  block_single_group <- new_summarize_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_single_group),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # With grouping by cyl, should have one row per cylinder
      expect_equal(nrow(result), length(unique(mtcars$cyl)))
      expect_true("cyl" %in% names(result))
    },
    args = list(x = block_single_group, data = list(data = function() mtcars))
  )

  # Test with multiple grouping columns
  block_multi_group <- new_summarize_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by = c("cyl", "am")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_multi_group),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # With multiple grouping, should have more groups
      expected_groups <- nrow(unique(mtcars[, c("cyl", "am")]))
      expect_equal(nrow(result), expected_groups)
      expect_true(all(c("cyl", "am") %in% names(result)))
    },
    args = list(x = block_multi_group, data = list(data = function() mtcars))
  )
})

test_that("summarize - different summary functions produce different results - testServer", {
  # Test mean
  block_mean <- new_summarize_block(
    summaries = list(result = list(func = "mean", col = "mpg"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_mean),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$result, mean(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block_mean, data = list(data = function() mtcars))
  )

  # Test median (different from mean)
  block_median <- new_summarize_block(
    summaries = list(result = list(func = "stats::median", col = "mpg"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_median),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$result, median(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block_median, data = list(data = function() mtcars))
  )

  # Test sum (very different from mean/median)
  block_sum <- new_summarize_block(
    summaries = list(result = list(func = "sum", col = "mpg"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_sum),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$result, sum(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block_sum, data = list(data = function() mtcars))
  )
})

test_that("summarize - different columns produce different results - testServer", {
  # Test mpg column
  block_mpg <- new_summarize_block(
    summaries = list(avg = list(func = "mean", col = "mpg"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_mpg),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$avg, mean(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block_mpg, data = list(data = function() mtcars))
  )

  # Test hp column (different values)
  block_hp <- new_summarize_block(
    summaries = list(avg = list(func = "mean", col = "hp"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_hp),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$avg, mean(mtcars$hp), tolerance = 0.0001)
      # hp mean should be different from mpg mean
      expect_false(abs(result$avg - mean(mtcars$mpg)) < 0.01)
    },
    args = list(x = block_hp, data = list(data = function() mtcars))
  )

  # Test wt column
  block_wt <- new_summarize_block(
    summaries = list(avg = list(func = "mean", col = "wt"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_wt),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$avg, mean(mtcars$wt), tolerance = 0.0001)
    },
    args = list(x = block_wt, data = list(data = function() mtcars))
  )
})

# get_summary_functions tests
test_that("get_summary_functions returns expected functions", {
  funcs <- get_summary_functions()

  expect_type(funcs, "character")
  expect_true(length(funcs) > 0)

  # Check some expected functions are present (stats:: namespace for stats functions)
  expect_true("mean" %in% funcs)
  expect_true("stats::median" %in% funcs)
  expect_true("sum" %in% funcs)
  expect_true("min" %in% funcs)
  expect_true("max" %in% funcs)
  expect_true("stats::sd" %in% funcs)
  expect_true("dplyr::n" %in% funcs)
  expect_true("dplyr::n_distinct" %in% funcs)
  expect_true("dplyr::first" %in% funcs)
  expect_true("dplyr::last" %in% funcs)
})

test_that("get_summary_functions can be extended via option", {
  # Set custom functions
  old_opt <- getOption("blockr.dplyr.summary_functions")

  on.exit(options(blockr.dplyr.summary_functions = old_opt))

  options(blockr.dplyr.summary_functions = c(
    "custom function" = "custom::func"
  ))

  funcs <- get_summary_functions()

  # Custom function should be present
  expect_true("custom::func" %in% funcs)
  # Default functions should still be present
  expect_true("mean" %in% funcs)
})

# Deprecated function test
test_that("new_summarize_nocode_block is deprecated", {
  expect_warning(
    blk <- new_summarize_nocode_block(),
    "deprecated"
  )
  expect_s3_class(blk, c("summarize_block", "transform_block", "block"))
})
