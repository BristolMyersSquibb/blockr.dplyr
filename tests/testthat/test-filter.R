test_that("filter expr block constructor", {
  # Test basic constructor
  blk <- new_filter_expr_block()
  expect_s3_class(blk, c("filter_expr_block", "transform_block", "block"))

  # Test constructor with filter condition
  blk <- new_filter_expr_block("mpg > 20")
  expect_s3_class(blk, c("filter_expr_block", "transform_block", "block"))

  # Test constructor with multiple conditions
  blk <- new_filter_expr_block("mpg > 20 & cyl == 6")
  expect_s3_class(blk, c("filter_expr_block", "transform_block", "block"))

  # Test that block always uses multi-condition interface
  blk_multi <- new_filter_expr_block()
  expect_s3_class(blk_multi, c("filter_expr_block", "transform_block", "block"))
})

test_that("parse_filter_expr function", {
  # Test empty string
  expr <- blockr.dplyr:::parse_filter_expr("")
  expect_type(expr, "language")

  # Test simple condition
  expr <- blockr.dplyr:::parse_filter_expr("mpg > 20")
  expect_type(expr, "language")

  # Test complex condition
  expr <- blockr.dplyr:::parse_filter_expr("mpg > 20 & cyl == 4")
  expect_type(expr, "language")

  # Test TRUE condition
  expr <- blockr.dplyr:::parse_filter_expr("TRUE")
  expect_type(expr, "language")
})

test_that("apply_filter_expr function handles errors", {
  # Create mock reactive values
  r_expr_validated <- shiny::reactiveVal()
  r_string_validated <- shiny::reactiveVal()

  # Test with valid condition
  expect_silent(
    blockr.dplyr:::apply_filter_expr(
      mtcars,
      "mpg > 20",
      r_expr_validated,
      r_string_validated
    )
  )

  # Test with empty string
  expect_silent(
    blockr.dplyr:::apply_filter_expr(
      mtcars,
      "",
      r_expr_validated,
      r_string_validated
    )
  )

  # Test with whitespace only
  expect_silent(
    blockr.dplyr:::apply_filter_expr(
      mtcars,
      "  ",
      r_expr_validated,
      r_string_validated
    )
  )
})

test_that("mod_multi_filter_server basic functionality", {
  # Test server module with simple setup
  testServer(
    mod_multi_filter_server,
    args = list(
      get_value = function() "mpg > 20",
      get_cols = function() c("mpg", "cyl", "hp", "wt")
    ),
    {
      # Test initialization
      expect_true(is.reactive(session$returned))

      # Test initial value
      initial_result <- session$returned()
      expect_type(initial_result, "character")
      expect_equal(initial_result, "mpg > 20")
    }
  )
})

test_that("mod_multi_filter_server with multiple conditions", {
  testServer(
    mod_multi_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp", "wt")
    ),
    {
      # Test adding conditions
      session$setInputs(add_condition = 1)

      # Should still return valid result
      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("mod_multi_filter_server handles empty conditions", {
  testServer(
    mod_multi_filter_server,
    args = list(
      get_value = function() "",
      get_cols = function() c("mpg", "cyl", "hp")
    ),
    {
      result <- session$returned()
      expect_type(result, "character")
      expect_equal(result, "TRUE") # Should default to TRUE
    }
  )
})

test_that("filter expr block integration with real data", {
  # Test that filter expr block can be applied to real data
  blk <- new_filter_expr_block("mpg > 20")
  expect_s3_class(blk, "filter_expr_block")

  # Test with different conditions
  blk2 <- new_filter_expr_block("cyl == 4")
  expect_s3_class(blk2, "filter_expr_block")

  # Test complex condition
  blk3 <- new_filter_expr_block("mpg > 20 & cyl == 4 & hp < 100")
  expect_s3_class(blk3, "filter_expr_block")
})

test_that("filter expr block default behavior", {
  # Test default TRUE condition
  blk_default <- new_filter_expr_block()
  expect_s3_class(
    blk_default,
    c("filter_expr_block", "transform_block", "block")
  )

  # Test with initial condition
  blk_with_condition <- new_filter_expr_block("mpg > 20")
  expect_s3_class(
    blk_with_condition,
    c("filter_expr_block", "transform_block", "block")
  )
})

test_that("multi_filter_condition_ui creates proper structure", {
  ui <- multi_filter_condition_ui("test", "mpg > 20", TRUE)

  # Should be a div with proper classes
  expect_s3_class(ui, c("shiny.tag", "list"))
  expect_equal(ui$name, "div")
  expect_true(grepl("multi-filter-condition", ui$attribs$class))
})

test_that("mod_multi_filter_ui creates proper structure", {
  ui <- mod_multi_filter_ui("test")

  # Should be a tagList with proper elements
  expect_s3_class(ui, c("shiny.tag.list", "list"))

  # Should contain shinyjs and styles
  expect_true(any(sapply(ui, function(x) {
    if (is.list(x) && "name" %in% names(x)) {
      x$name == "style"
    } else {
      FALSE
    }
  })))
})

test_that("filter expr block state management", {
  # Test that block includes proper state for restoration
  blk <- new_filter_expr_block("mpg > 20")

  # The block should be constructible
  expect_s3_class(blk, c("filter_expr_block", "transform_block", "block"))
})

test_that("filter expr block construction with different conditions", {
  # All should work without errors
  expect_no_error(new_filter_expr_block("TRUE"))
  expect_no_error(new_filter_expr_block("mpg > 20"))
  expect_no_error(new_filter_expr_block("mpg > 20 & cyl == 4"))
})

# Restorability Tests - Verify blocks can be created with parameters and work immediately
test_that("filter_expr block restorability - simple condition", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Create block with exprs parameter - this is what users would call
  blk <- new_filter_expr_block("mpg > 20")

  # Verify the block works via testServer
  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "cyl")])),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Verify expression generation works
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("mpg > 20", expr_text))
    }
  )
})

test_that("filter_expr block restorability - complex condition", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Create block with complex AND condition
  blk <- new_filter_expr_block("mpg > 20 & cyl == 4")

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:15, c("mpg", "cyl", "hp")])),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))

      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("mpg > 20", expr_text))
      expect_true(grepl("cyl == 4", expr_text))
    }
  )
})

test_that("filter_expr block restorability - multiple OR conditions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Create block with OR condition
  blk <- new_filter_expr_block("cyl == 4 | cyl == 6")

  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:20, c("mpg", "cyl")])),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))

      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("cyl == 4", expr_text))
      expect_true(grepl("cyl == 6", expr_text))
    }
  )
})

# Data transformation tests using block_server
test_that("filter expr block filters simple condition - testServer", {
  block <- new_filter_expr_block("mpg > 20")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify filtering
      expected_rows <- nrow(mtcars[mtcars$mpg > 20, ])
      expect_equal(nrow(result), expected_rows)
      expect_true(all(result$mpg > 20))
      expect_equal(ncol(result), ncol(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("filter expr block handles multiple AND conditions - testServer", {
  block <- new_filter_expr_block("mpg > 20 & cyl == 4")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify both conditions
      expect_true(all(result$mpg > 20))
      expect_true(all(result$cyl == 4))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("filter expr block with empty condition returns all - testServer", {
  block <- new_filter_expr_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should return all rows
      expect_equal(nrow(result), nrow(mtcars))
      expect_equal(ncol(result), ncol(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("filter expr block complex OR/AND logic - testServer", {
  block <- new_filter_expr_block("(mpg > 25 | hp > 200) & cyl != 6")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify complex logic
      expect_true(all(result$cyl != 6))
      expect_true(all(result$mpg > 25 | result$hp > 200))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("filter expr block no matches returns empty - testServer", {
  block <- new_filter_expr_block("mpg > 100")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should be empty but preserve structure
      expect_equal(nrow(result), 0)
      expect_equal(ncol(result), ncol(mtcars))
      expect_equal(names(result), names(mtcars))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
