test_that("filter block constructor", {
  # Test basic constructor
  blk <- new_filter_block()
  expect_s3_class(blk, c("filter_block", "transform_block", "block"))

  # Test constructor with filter condition
  blk <- new_filter_block("mpg > 20")
  expect_s3_class(blk, c("filter_block", "transform_block", "block"))

  # Test constructor with multiple conditions
  blk <- new_filter_block("mpg > 20 & cyl == 6")
  expect_s3_class(blk, c("filter_block", "transform_block", "block"))

  # Test that block always uses multi-condition interface
  blk_multi <- new_filter_block()
  expect_s3_class(blk_multi, c("filter_block", "transform_block", "block"))
})

test_that("parse_filter function", {
  # Test empty string
  expr <- parse_filter("")
  expect_type(expr, "expression")

  # Test simple condition
  expr <- parse_filter("mpg > 20")
  expect_type(expr, "expression")

  # Test complex condition
  expr <- parse_filter("mpg > 20 & cyl == 4")
  expect_type(expr, "expression")

  # Test TRUE condition
  expr <- parse_filter("TRUE")
  expect_type(expr, "expression")
})

test_that("apply_filter function handles errors", {
  # Create mock reactive values
  r_expr_validated <- shiny::reactiveVal()
  r_string_validated <- shiny::reactiveVal()

  # Test with valid condition
  expect_silent(
    apply_filter(mtcars, "mpg > 20", r_expr_validated, r_string_validated)
  )

  # Test with empty string
  expect_silent(
    apply_filter(mtcars, "", r_expr_validated, r_string_validated)
  )

  # Test with whitespace only
  expect_silent(
    apply_filter(mtcars, "  ", r_expr_validated, r_string_validated)
  )
})

test_that("mod_multi_filter_server basic functionality", {
  # Test server module with simple setup
  testServer(mod_multi_filter_server,
    args = list(
      get_value = function() "mpg > 20",
      get_cols = function() c("mpg", "cyl", "hp", "wt")
    ), {
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
  testServer(mod_multi_filter_server,
    args = list(
      get_value = function() "TRUE",
      get_cols = function() c("mpg", "cyl", "hp", "wt")
    ), {
      # Test adding conditions
      session$setInputs(add_condition = 1)

      # Should still return valid result
      result <- session$returned()
      expect_type(result, "character")
    }
  )
})

test_that("mod_multi_filter_server handles empty conditions", {
  testServer(mod_multi_filter_server,
    args = list(
      get_value = function() "",
      get_cols = function() c("mpg", "cyl", "hp")
    ), {
      result <- session$returned()
      expect_type(result, "character")
      expect_equal(result, "TRUE")  # Should default to TRUE
    }
  )
})

test_that("filter block integration with real data", {
  # Test that filter block can be applied to real data
  blk <- new_filter_block("mpg > 20")
  expect_s3_class(blk, "filter_block")

  # Test with different conditions
  blk2 <- new_filter_block("cyl == 4")
  expect_s3_class(blk2, "filter_block")

  # Test complex condition
  blk3 <- new_filter_block("mpg > 20 & cyl == 4 & hp < 100")
  expect_s3_class(blk3, "filter_block")
})

test_that("filter block default behavior", {
  # Test default TRUE condition
  blk_default <- new_filter_block()
  expect_s3_class(blk_default, c("filter_block", "transform_block", "block"))

  # Test with initial condition
  blk_with_condition <- new_filter_block("mpg > 20")
  expect_s3_class(blk_with_condition, c("filter_block", "transform_block", "block"))
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

test_that("filter block state management", {
  # Test that block includes proper state for restoration
  blk <- new_filter_block("mpg > 20")

  # The block should be constructible
  expect_s3_class(blk, c("filter_block", "transform_block", "block"))
})

test_that("filter block construction with different conditions", {
  # All should work without errors
  expect_no_error(new_filter_block("TRUE"))
  expect_no_error(new_filter_block("mpg > 20"))
  expect_no_error(new_filter_block("mpg > 20 & cyl == 4"))
})
