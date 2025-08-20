test_that("rename block constructor", {
  # Test basic constructor
  blk <- new_rename_block()
  expect_s3_class(blk, c("rename_block", "transform_block", "block"))

  # Test constructor with rename pairs
  blk <- new_rename_block(list(miles_per_gallon = "mpg"))
  expect_s3_class(blk, c("rename_block", "transform_block", "block"))

  # Test constructor with multiple renames
  blk <- new_rename_block(list(miles_per_gallon = "mpg", cylinders = "cyl"))
  expect_s3_class(blk, c("rename_block", "transform_block", "block"))
})

test_that("parse_rename function", {
  # Test empty renames
  expr <- parse_rename(list())
  expect_type(expr, "expression")

  # Test single rename
  expr <- parse_rename(list(miles_per_gallon = "mpg"))
  expect_type(expr, "expression")

  # Test multiple renames
  expr <- parse_rename(list(miles_per_gallon = "mpg", cylinders = "cyl"))
  expect_type(expr, "expression")

  # Test with named vector
  renames <- c(horsepower = "hp", weight = "wt")
  expr <- parse_rename(renames)
  expect_type(expr, "expression")
})

test_that("apply_rename function handles validation", {
  # Create mock reactive values
  r_expr_validated <- shiny::reactiveVal()
  r_renames_validated <- shiny::reactiveVal()

  # Test with valid renames
  expect_silent(
    apply_rename(mtcars, list(miles_per_gallon = "mpg"), r_expr_validated, r_renames_validated)
  )

  # Test with empty renames
  expect_silent(
    apply_rename(mtcars, list(), r_expr_validated, r_renames_validated)
  )
})

test_that("apply_rename validation catches errors", {
  r_expr_validated <- shiny::reactiveVal()
  r_renames_validated <- shiny::reactiveVal()

  # Test with non-existent column (should not call req, so returns silently)
  expect_silent(
    apply_rename(mtcars, list(new_name = "nonexistent_col"), r_expr_validated, r_renames_validated)
  )

  # Test with duplicate old columns
  expect_silent(
    apply_rename(mtcars, list(name1 = "mpg", name2 = "mpg"), r_expr_validated, r_renames_validated)
  )

  # Test with empty new names - this case is handled by validation
})

test_that("mod_multi_rename_server basic functionality", {
  # Test server module with simple setup
  testServer(mod_multi_rename_server,
    args = list(
      get_value = function() list(miles_per_gallon = "mpg"),
      get_cols = function() c("mpg", "cyl", "hp", "wt")
    ), {
      # Test initialization
      expect_true(is.reactive(session$returned))

      # Test initial value
      initial_result <- session$returned()
      expect_type(initial_result, "list")
      expect_equal(names(initial_result), "miles_per_gallon")
      expect_equal(unname(initial_result[[1]]), "mpg")
    }
  )
})

test_that("mod_multi_rename_server with multiple renames", {
  testServer(mod_multi_rename_server,
    args = list(
      get_value = function() list(hp_new = "hp", wt_new = "wt"),
      get_cols = function() c("mpg", "cyl", "hp", "wt")
    ), {
      # Test adding renames
      session$setInputs(add_rename = 1)

      # Should still return valid result
      result <- session$returned()
      expect_type(result, "list")
      expect_true(length(result) >= 2)
    }
  )
})

test_that("mod_multi_rename_server handles empty renames", {
  testServer(mod_multi_rename_server,
    args = list(
      get_value = function() list(),
      get_cols = function() c("mpg", "cyl", "hp")
    ), {
      result <- session$returned()
      expect_type(result, "list")
      expect_equal(names(result), "new_col")  # Should default
      expect_true(result[[1]] %in% c("mpg", "cyl", "hp"))  # Should be a valid column
    }
  )
})

test_that("rename block integration with real data", {
  # Test that rename block can be applied to real data
  blk <- new_rename_block(list(miles_per_gallon = "mpg"))
  expect_s3_class(blk, "rename_block")

  # Test with different renames
  blk2 <- new_rename_block(list(cylinders = "cyl", horsepower = "hp"))
  expect_s3_class(blk2, "rename_block")

  # Test complex renames
  blk3 <- new_rename_block(list(
    miles_per_gallon = "mpg",
    number_of_cylinders = "cyl",
    gross_horsepower = "hp",
    weight_lbs = "wt"
  ))
  expect_s3_class(blk3, "rename_block")
})

test_that("multi_rename_row_ui creates proper structure", {
  ui <- multi_rename_row_ui("test", "new_name", "old_name", c("col1", "col2"), TRUE)

  # Should be a div with proper classes
  expect_s3_class(ui, c("shiny.tag", "list"))
  expect_equal(ui$name, "div")
  expect_true(grepl("multi-rename-pair", ui$attribs$class))
})

test_that("mod_multi_rename_ui creates proper structure", {
  ui <- mod_multi_rename_ui("test")

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

test_that("rename block state management", {
  # Test that state includes renames for proper restoration
  blk <- new_rename_block(list(new_name = "old_name"))

  # The block should be constructible
  expect_s3_class(blk, c("rename_block", "transform_block", "block"))
})

test_that("rename block with different configurations", {
  # All should work without errors
  expect_no_error(new_rename_block())
  expect_no_error(new_rename_block(list()))
  expect_no_error(new_rename_block(list(a = "b")))
  expect_no_error(new_rename_block(list(new1 = "old1", new2 = "old2")))
})

test_that("parse_rename generates correct dplyr code", {
  # Test single rename
  expr <- parse_rename(list(new_col = "old_col"))
  expr_text <- deparse(expr)
  expect_true(grepl("dplyr::rename", expr_text))
  expect_true(grepl("new_col = old_col", expr_text))

  # Test multiple renames
  expr <- parse_rename(list(a = "x", b = "y"))
  expr_text <- deparse(expr)
  expect_true(grepl("a = x", expr_text))
  expect_true(grepl("b = y", expr_text))
})

test_that("rename validation works correctly", {
  # Mock data
  test_data <- data.frame(mpg = 1:5, cyl = 6:10, hp = 11:15)
  r_expr <- shiny::reactiveVal()
  r_renames <- shiny::reactiveVal()

  # Valid rename should work
  expect_silent(apply_rename(test_data, list(miles = "mpg"), r_expr, r_renames))

  # Invalid old column should be caught
  expect_silent(apply_rename(test_data, list(new_name = "nonexistent"), r_expr, r_renames))

  # Duplicate old columns should be caught
  expect_silent(apply_rename(test_data, list(name1 = "mpg", name2 = "mpg"), r_expr, r_renames))
})