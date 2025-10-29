test_that("parse_mutate handles .by parameter", {
  # Test without grouping
  expr1 <- parse_mutate(c(new_col = "mpg * 2"))
  expect_type(expr1, "language")
  code1 <- deparse(expr1)
  expect_true(grepl("dplyr::mutate\\(data, new_col = mpg \\* 2\\)", code1))
  expect_false(grepl("\\.by", code1))

  # Test with grouping
  expr2 <- parse_mutate(c(avg_mpg = "mean(mpg)"), c("cyl"))
  expect_type(expr2, "language")
  code2 <- deparse(expr2)
  expect_true(grepl("dplyr::mutate.*\\.by = c\\(\"cyl\"\\)", code2))

  # Test with multiple grouping columns
  expr3 <- parse_mutate(c(avg_mpg = "mean(mpg)"), c("cyl", "am"))
  expect_type(expr3, "language")
  code3 <- deparse(expr3)
  expect_true(grepl('\\.by = c\\("cyl".*"am"\\)', paste(code3, collapse = " ")))
})

test_that("mutate block with .by executes correctly", {
  library(dplyr)

  # Test grouped mutate
  expr <- parse_mutate(c(avg_mpg = "mean(mpg)"), c("cyl"))
  data <- mtcars[1:10, c("mpg", "cyl", "hp")]
  result <- eval(expr)

  # Should have original columns plus new column
  expect_equal(ncol(result), 4) # mpg, cyl, hp, avg_mpg
  expect_true("avg_mpg" %in% names(result))

  # Check that grouping worked correctly
  # avg_mpg should be the same for all rows with same cyl value
  unique_cyls <- unique(result$cyl)
  for (cyl_val in unique_cyls) {
    cyl_rows <- result[result$cyl == cyl_val, ]
    expect_true(
      all(cyl_rows$avg_mpg == cyl_rows$avg_mpg[1]),
      info = paste("avg_mpg should be consistent within cyl =", cyl_val)
    )
  }
})

test_that("mutate block handles empty .by parameter", {
  # Test with empty character vector
  expr1 <- parse_mutate(c(new_col = "mpg * 2"), character())
  expect_type(expr1, "language")
  code1 <- deparse(expr1)
  expect_false(grepl("\\.by", code1))

  # Test with empty string in by parameter
  expr2 <- parse_mutate(c(new_col = "mpg * 2"), c(""))
  expect_type(expr2, "language")
  code2 <- deparse(expr2)
  expect_false(grepl("\\.by", code2))
})

test_that("mutate block with multiple expressions", {
  skip_if_not_installed("dplyr")

  # Test multiple mutations
  expr <- parse_mutate(c(
    mpg2 = "mpg * 2",
    hp_per_cyl = "hp / cyl",
    is_efficient = "mpg > 20"
  ))

  expect_type(expr, "language")

  # Execute and verify
  data <- mtcars[1:5, c("mpg", "cyl", "hp")]
  result <- eval(expr)

  expect_equal(ncol(result), 6) # original 3 + 3 new columns
  expect_true("mpg2" %in% names(result))
  expect_true("hp_per_cyl" %in% names(result))
  expect_true("is_efficient" %in% names(result))

  # Verify calculations
  expect_equal(result$mpg2, result$mpg * 2)
  expect_equal(result$hp_per_cyl, result$hp / result$cyl)
})

test_that("mutate block with complex expressions using existing columns", {
  skip_if_not_installed("dplyr")

  # Test using multiple existing columns in computation
  expr <- parse_mutate(c(
    power_weight_ratio = "hp / wt",
    efficiency_score = "(mpg * hp) / (wt * 100)"
  ))

  expect_type(expr, "language")

  # Execute and verify
  data <- mtcars[1:5, c("mpg", "hp", "wt")]
  result <- eval(expr)

  expect_equal(ncol(result), 5) # original 3 + 2 new
  expect_true("power_weight_ratio" %in% names(result))
  expect_true("efficiency_score" %in% names(result))

  # Verify calculations are correct
  expect_equal(result$power_weight_ratio, result$hp / result$wt)
})

test_that("mutate block with ifelse function", {
  skip_if_not_installed("dplyr")

  # Test ifelse conditional mutation
  expr <- parse_mutate(c(
    mpg_category = 'ifelse(mpg > 20, "high", "low")',
    cyl_type = 'ifelse(cyl <= 4, "small", ifelse(cyl <= 6, "medium", "large"))'
  ))

  expect_type(expr, "language")

  # Execute and verify
  data <- mtcars[1:10, c("mpg", "cyl")]
  result <- eval(expr)

  expect_equal(ncol(result), 4)
  expect_true("mpg_category" %in% names(result))
  expect_true("cyl_type" %in% names(result))

  # Verify logical categories
  expect_true(all(result$mpg_category %in% c("high", "low")))
  expect_true(all(result$cyl_type %in% c("small", "medium", "large")))
})

test_that("mutate block with case_when function", {
  skip_if_not_installed("dplyr")

  # Test case_when for complex conditionals
  expr <- parse_mutate(c(
    efficiency = 'case_when(mpg > 25 ~ "excellent", mpg > 20 ~ "good", mpg > 15 ~ "fair", TRUE ~ "poor")'
  ))

  expect_type(expr, "language")

  # Execute and verify
  data <- mtcars[1:15, c("mpg", "cyl")]
  result <- eval(expr)

  expect_equal(ncol(result), 3)
  expect_true("efficiency" %in% names(result))
  expect_true(all(
    result$efficiency %in% c("excellent", "good", "fair", "poor")
  ))
})

test_that("mutate block overwriting existing columns", {
  skip_if_not_installed("dplyr")

  # Test mutating an existing column (should replace it)
  expr <- parse_mutate(c(
    mpg = "mpg * 1.5", # Overwrite mpg column
    hp = "hp + 10" # Overwrite hp column
  ))

  expect_type(expr, "language")

  # Execute and verify
  data <- mtcars[1:5, c("mpg", "cyl", "hp")]
  original_mpg <- data$mpg
  original_hp <- data$hp
  result <- eval(expr)

  # Should still have same number of columns
  expect_equal(ncol(result), 3)

  # Values should be transformed, not original
  expect_equal(result$mpg, original_mpg * 1.5)
  expect_equal(result$hp, original_hp + 10)
})

test_that("mutate block with NA handling", {
  skip_if_not_installed("dplyr")

  # Create data with NAs
  data <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c(10, NA, 30, 40, 50)
  )

  # Test mutations that handle NAs
  expr <- parse_mutate(c(
    x_filled = "ifelse(is.na(x), 0, x)",
    has_na = "is.na(x) | is.na(y)",
    sum_xy = "x + y" # Will produce NA where either is NA
  ))

  expect_type(expr, "language")

  result <- eval(expr)

  expect_equal(ncol(result), 5)
  expect_true("x_filled" %in% names(result))
  expect_true("has_na" %in% names(result))

  # Verify NA handling
  expect_equal(result$x_filled[3], 0) # NA was replaced with 0
  expect_true(result$has_na[2]) # Row 2 has NA in y
  expect_true(is.na(result$sum_xy[3])) # NA + number = NA
})

test_that("mutate block with grouped calculations", {
  skip_if_not_installed("dplyr")

  # Test mutate with .by performing group-wise operations
  expr <- parse_mutate(
    c(
      mpg_rank = "rank(mpg)",
      mpg_centered = "mpg - mean(mpg)"
    ),
    by = c("cyl")
  )

  expect_type(expr, "language")

  data <- mtcars[1:12, c("mpg", "cyl")]
  result <- eval(expr)

  expect_equal(ncol(result), 4)
  expect_true("mpg_rank" %in% names(result))
  expect_true("mpg_centered" %in% names(result))

  # Within each cylinder group, centered values should sum to ~0
  library(dplyr)
  group_sums <- result %>%
    group_by(cyl) %>%
    summarize(sum_centered = sum(mpg_centered))

  expect_true(all(abs(group_sums$sum_centered) < 0.01))
})

test_that("mutate block with sequential column dependencies", {
  skip_if_not_installed("dplyr")

  # Test creating columns that depend on previously created columns
  expr <- parse_mutate(c(
    mpg2 = "mpg * 2",
    mpg4 = "mpg2 * 2", # Uses mpg2 created in same mutate
    mpg8 = "mpg4 * 2" # Uses mpg4 created in same mutate
  ))

  expect_type(expr, "language")

  data <- mtcars[1:3, c("mpg", "cyl")]
  result <- eval(expr)

  expect_equal(ncol(result), 5)

  # Verify sequential dependencies work
  expect_equal(result$mpg2, result$mpg * 2)
  expect_equal(result$mpg4, result$mpg2 * 2)
  expect_equal(result$mpg8, result$mpg4 * 2)
  expect_equal(result$mpg8, result$mpg * 8)
})

# Restorability Tests - Verify blocks can be created with parameters and work immediately
test_that("mutate block restorability - single expression", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Create block with exprs parameter - this is what users would call
  blk <- new_mutate_block(exprs = list(mpg2 = "mpg * 2"))

  # Verify the block works via testServer
  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:5, c("mpg", "cyl")])),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Verify expression generation works
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      # Verify it is a call

      expr_text <- deparse(expr_result)
      expect_true(grepl("mpg2", expr_text))
      expect_true(grepl("mpg \\* 2", expr_text))
    }
  )
})

test_that("mutate block restorability - multiple expressions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Create block with multiple expressions
  blk <- new_mutate_block(
    exprs = list(
      mpg2 = "mpg * 2",
      hp_per_cyl = "hp / cyl"
    )
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:5, c("mpg", "cyl", "hp")])),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))

      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("mpg2", expr_text))
      expect_true(grepl("hp_per_cyl", expr_text))
    }
  )
})

# Data transformation tests using block_server
test_that("mutate block adds new column - testServer", {
  block <- new_mutate_block(exprs = list(mpg_squared = "mpg * mpg"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true("mpg_squared" %in% names(result))
      expect_equal(result$mpg_squared, mtcars$mpg * mtcars$mpg)
      expect_true(all(names(mtcars) %in% names(result)))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("mutate block with grouping - testServer", {
  block <- new_mutate_block(exprs = list(mean_mpg = "mean(mpg)"), by = "cyl")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true("mean_mpg" %in% names(result))

      # Verify grouping worked - mean should be constant within each cyl group
      for (cyl_val in unique(result$cyl)) {
        cyl_rows <- result[result$cyl == cyl_val, ]
        expect_true(all(cyl_rows$mean_mpg == cyl_rows$mean_mpg[1]))
      }
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
