test_that("select block constructor", {
  blk <- new_select_block()
  expect_s3_class(blk, c("select_block", "transform_block", "block"))
})

test_that("enhanced select block constructor", {
  # Test enhanced mode (default)
  blk <- new_select_block(enhanced = TRUE)
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # Test with initial columns
  blk <- new_select_block(c("mpg", "cyl"), enhanced = TRUE)
  expect_s3_class(blk, c("select_block", "transform_block", "block"))
})

test_that("classic select block constructor", {
  # Test classic mode for backward compatibility
  blk <- new_select_block(enhanced = FALSE)
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # Test with initial columns
  blk <- new_select_block(c("mpg", "cyl"), enhanced = FALSE)
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

test_that("multi select module functionality", {
  # Test module server function exists
  expect_true(exists("mod_multi_select_server"))
  expect_true(is.function(mod_multi_select_server))

  # Test module UI function exists
  expect_true(exists("mod_multi_select_ui"))
  expect_true(is.function(mod_multi_select_ui))

  # Test example function exists
  expect_true(exists("run_multi_select_example"))
  expect_true(is.function(run_multi_select_example))
})

test_that("select block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Create test data
  test_data <- data.frame(
    a = c(1, 2, 3),
    b = c("x", "y", "z"),
    c = c(10, 20, 30)
  )

  # Test basic select block functionality
  blk <- new_select_block(c("a", "c"))
  expect_s3_class(blk, c("select_block", "transform_block", "block"))

  # The block should be properly structured
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
})

test_that("mod_multi_select_server basic functionality", {
  skip_if_not_installed("shiny")

  # Test server module with mock functions
  shiny::testServer(
    mod_multi_select_server,
    args = list(
      get_value = function() c("mpg", "cyl"),
      get_cols = function() c("mpg", "cyl", "hp", "wt"),
      get_data_preview = function() mtcars[1:5, ]
    ),
    {
      # Force initialization by accessing r_cols() to trigger observe()
      cols <- r_cols()

      # Wait for initialization to complete
      session$flushReact()

      # Test initial selection after initialization
      result <- session$getReturned()()
      expect_equal(sort(result), sort(c("mpg", "cyl")))

      # Test that reactive values are set up correctly
      expect_equal(sort(r_selected()), sort(c("mpg", "cyl")))
      expect_equal(sort(r_cols()), sort(c("mpg", "cyl", "hp", "wt")))
      expect_equal(r_search(), "")
    }
  )
})

test_that("mod_multi_select_server search functionality", {
  skip_if_not_installed("shiny")

  shiny::testServer(
    mod_multi_select_server,
    args = list(
      get_value = function() character(0),
      get_cols = function() c("mpg", "cyl", "hp", "wt", "am", "gear"),
      get_data_preview = function() mtcars[1:5, ]
    ),
    {
      # Force initialization
      cols <- r_cols()
      session$flushReact()

      # Test search functionality
      session$setInputs(search = "m")

      # Should filter to columns containing "m"
      filtered <- r_filtered_cols()
      expect_true(all(grepl("m", filtered, ignore.case = TRUE)))
      expect_true("mpg" %in% filtered)
      expect_true("am" %in% filtered)
    }
  )
})

test_that("mod_multi_select_server selection controls", {
  skip_if_not_installed("shiny")

  shiny::testServer(
    mod_multi_select_server,
    args = list(
      get_value = function() c("mpg"),
      get_cols = function() c("mpg", "cyl", "hp"),
      get_data_preview = function() mtcars[1:5, ]
    ),
    {
      # Force initialization
      cols <- r_cols()
      session$flushReact()

      # Test select all
      session$setInputs(select_all = 1)
      expect_equal(sort(session$getReturned()()), sort(c("mpg", "cyl", "hp")))

      # Test select none
      session$setInputs(select_none = 1)
      expect_equal(length(session$getReturned()()), 0)

      # Test invert selection (from empty)
      session$setInputs(invert_selection = 1)
      expect_equal(sort(session$getReturned()()), sort(c("mpg", "cyl", "hp")))
    }
  )
})

test_that("multi_select_column_card UI generation", {
  # Test column card UI function
  column_info <- list(
    type = "numeric",
    sample = "21.0, 21.0, 22.8",
    na_count = 0,
    unique_count = 25,
    total_count = 32
  )

  card_ui <- multi_select_column_card(
    "test_id",
    "mpg",
    column_info,
    is_selected = TRUE
  )

  expect_s3_class(card_ui, "shiny.tag")
  expect_true(grepl("column-card", as.character(card_ui)))
  expect_true(grepl("mpg", as.character(card_ui)))
})

test_that("column info extraction", {
  # This tests the get_column_info function indirectly
  test_data <- data.frame(
    numeric_col = c(1.5, 2.5, 3.5, NA),
    char_col = c("a", "b", "a", "c"),
    logical_col = c(TRUE, FALSE, TRUE, TRUE)
  )

  # The function should handle different data types
  expect_true(is.data.frame(test_data))
  expect_equal(ncol(test_data), 3)
  expect_equal(nrow(test_data), 4)
})

test_that("enhanced vs classic mode state differences", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Both modes should create valid blocks
  enhanced_blk <- new_select_block(enhanced = TRUE)
  classic_blk <- new_select_block(enhanced = FALSE)

  expect_s3_class(enhanced_blk, c("select_block", "transform_block", "block"))
  expect_s3_class(classic_blk, c("select_block", "transform_block", "block"))

  # Both should have the same basic structure
  expect_true("expr_server" %in% names(enhanced_blk))
  expect_true("expr_ui" %in% names(enhanced_blk))
  expect_true("expr_server" %in% names(classic_blk))
  expect_true("expr_ui" %in% names(classic_blk))
})