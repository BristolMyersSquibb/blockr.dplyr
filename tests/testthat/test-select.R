test_that("select block constructor", {
  blk <- new_select_block()
  expect_s3_class(blk, c("select_block", "transform_block", "block"))
})

test_that("select block with columns parameter", {
  # Test with initial columns
  blk <- new_select_block(columns = c("mpg", "cyl"))
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # Test with single column
  blk <- new_select_block(columns = "mpg")
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # Test with empty columns
  blk <- new_select_block(columns = character(0))
  expect_s3_class(blk, c("select_block", "transform_block", "block"))
})

test_that("select block with exclude parameter", {
  # Test include mode (default)
  blk <- new_select_block(columns = c("mpg", "cyl"), exclude = FALSE)
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # Test exclude mode
  blk <- new_select_block(columns = c("gear", "carb"), exclude = TRUE)
  expect_s3_class(blk, c("select_block", "transform_block", "block"))
})

test_that("select block with various column inputs", {
  # Test empty columns
  blk <- new_select_block(character(0))
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # Test single column
  blk <- new_select_block("mpg")
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # Test multiple columns
  blk <- new_select_block(c("mpg", "cyl", "hp"))
  expect_s3_class(blk, c("select_block", "transform_block", "block"))
})

test_that("select block structure", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  blk <- new_select_block(c("mpg", "cyl"))
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # The block should be properly structured
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
})

test_that("select block expression generation - include mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Test with selected columns (include mode)
  test_data <- reactive(data.frame(
    a = c(1, 2, 3),
    b = c("x", "y", "z"),
    c = c(10, 20, 30)
  ))

  blk <- new_select_block(columns = c("a", "c"), exclude = FALSE)

  # Test that server function works
  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      # Check that server returned a list with expr and state
      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Check expression
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("dplyr::select", expr_text)))
      expect_true(any(grepl("a", expr_text)))
      expect_true(any(grepl("c", expr_text)))
    }
  )
})

test_that("select block expression generation - exclude mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Test with selected columns (exclude mode)
  test_data <- reactive(data.frame(
    a = c(1, 2, 3),
    b = c("x", "y", "z"),
    c = c(10, 20, 30)
  ))

  blk <- new_select_block(columns = c("b"), exclude = TRUE)

  # Test that server function works
  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      # Check that server returned a list with expr and state
      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Check expression - should use minus syntax
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("dplyr::select", expr_text)))
      expect_true(any(grepl("-c", expr_text))) # Minus syntax
      expect_true(any(grepl("b", expr_text)))
    }
  )
})

test_that("select block expression generation - empty selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(data.frame(
    a = c(1, 2, 3),
    b = c("x", "y", "z")
  ))

  # Empty selection in include mode = select nothing
  blk_include <- new_select_block(columns = character(0), exclude = FALSE)

  shiny::testServer(
    blk_include$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      # Should select nothing using -everything()
      expect_true(any(grepl("everything", expr_text)))
      expect_true(any(grepl("-", expr_text)))
    }
  )

  # Empty selection in exclude mode = select all
  blk_exclude <- new_select_block(columns = character(0), exclude = TRUE)

  shiny::testServer(
    blk_exclude$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      # Should select all (no column args)
      expect_true(any(grepl("dplyr::select\\(data\\)", expr_text)))
    }
  )
})

test_that("select block UI generation", {
  blk <- new_select_block(columns = c("mpg", "cyl"))

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain selectizeInput
  expect_true(grepl("selectize", ui_text, ignore.case = TRUE))

  # Should contain checkbox for exclude mode
  expect_true(grepl("checkbox", ui_text, ignore.case = TRUE))
  expect_true(grepl("Exclude", ui_text, ignore.case = TRUE))
})

test_that("select block reactive updates", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(data.frame(
    mpg = c(21, 21, 22.8),
    cyl = c(6, 6, 4),
    hp = c(110, 110, 93)
  ))

  blk <- new_select_block(columns = c("mpg"))

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Initial state - check that columns() reactive returns "mpg"
      expect_equal(result$state$columns(), "mpg")
      expect_false(isTRUE(result$state$exclude()))

      # Update columns
      session$setInputs(columns = c("mpg", "cyl"))
      session$flushReact()
      expect_equal(result$state$columns(), c("mpg", "cyl"))

      # Update exclude mode
      session$setInputs(exclude = TRUE)
      session$flushReact()
      expect_true(isTRUE(result$state$exclude()))
    }
  )
})
