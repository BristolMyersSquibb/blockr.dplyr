test_that("mod_join_keys_ui creates proper structure", {
  ui <- mod_join_keys_ui("test")

  # Should be a tagList containing styles and a div
  expect_s3_class(ui, "shiny.tag.list")
  expect_true(grepl("join-keys-container", as.character(ui)))
  expect_true(
    grepl("Join Keys", as.character(ui)) ||
      grepl("Use natural join", as.character(ui))
  )
})

test_that("mod_join_keys_ui with custom label", {
  ui <- mod_join_keys_ui("test", label = "Custom Join Configuration")

  # The label parameter is not actually used in the current implementation
  # Just verify the UI is created properly
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_join_keys_server basic functionality", {
  skip_if_not_installed("shiny")

  testServer(
    mod_join_keys_server,
    args = list(
      get_x_cols = reactive(c("id", "name", "value")),
      get_y_cols = reactive(c("id", "category", "score")),
      initial_keys = c("id")
    ),
    {
      # Test initial setup
      expect_true(is.reactive(session$returned))

      # Test natural join
      session$setInputs(use_natural_join = TRUE)
      session$setInputs(natural_keys = c("id"))
      result <- session$returned()
      expect_equal(result, c("id"))
    }
  )
})

test_that("mod_join_keys_server handles custom mappings", {
  skip_if_not_installed("shiny")

  testServer(
    mod_join_keys_server,
    args = list(
      get_x_cols = reactive(c("user_id", "name")),
      get_y_cols = reactive(c("id", "category")),
      initial_keys = character()
    ),
    {
      # Switch to custom join mode
      session$setInputs(use_natural_join = FALSE)

      # Test that custom mappings can be configured
      # (Note: Full UI interaction testing would require more complex setup)
      expect_true(is.reactive(session$returned))
    }
  )
})

test_that("mod_join_keys_server handles empty columns gracefully", {
  skip_if_not_installed("shiny")

  testServer(
    mod_join_keys_server,
    args = list(
      get_x_cols = reactive(character()),
      get_y_cols = reactive(character()),
      initial_keys = character()
    ),
    {
      # Should handle empty column sets without error
      expect_true(is.reactive(session$returned))
      result <- session$returned()
      expect_equal(result, character())
    }
  )
})

test_that("helper functions work correctly", {
  # Test %||% operator
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")

  # Test keep function
  test_list <- list(
    list(x = 1, y = 2),
    list(x = 0, y = 3),
    list(x = 2, y = 1)
  )
  result <- keep(test_list, function(item) item$x > 0)
  expect_length(result, 2)
  expect_equal(result[[1]]$x, 1)
  expect_equal(result[[2]]$x, 2)

  # Test map function
  numbers <- list(1, 2, 3)
  result <- map(numbers, function(x) x * 2)
  expect_equal(result, list(2, 4, 6))

  # Test map_chr function
  items <- list(list(name = "A"), list(name = "B"))
  result <- map_chr(items, function(x) x$name)
  expect_equal(result, c("A", "B"))

  # Test keep function with lambda syntax (as used in the actual implementation)
  mappings <- list(
    list(x_col = "id", y_col = "customer_id"),
    list(x_col = "", y_col = ""),
    list(x_col = "name", y_col = "customer_name")
  )
  valid_mappings <- keep(mappings, ~ nzchar(.x$x_col) && nzchar(.x$y_col))
  expect_length(valid_mappings, 2)
  expect_equal(valid_mappings[[1]]$x_col, "id")
  expect_equal(valid_mappings[[2]]$x_col, "name")
})

test_that("mod_join_keys_server initializes custom mappings properly", {
  skip_if_not_installed("shiny")

  testServer(
    mod_join_keys_server,
    args = list(
      get_x_cols = reactive(c("id", "name")),
      get_y_cols = reactive(c("customer_id", "customer_name")),
      initial_keys = character()
    ),
    {
      # Switch to custom join mode - should initialize mappings automatically
      session$setInputs(use_natural_join = FALSE)

      # Should have initialized properly without errors
      # (This tests the fix for the "Add Join Key" button requiring two clicks)
      expect_true(is.reactive(session$returned))
      result <- session$returned()
      # In custom mode with no valid mappings, returns an empty list
      expect_equal(result, list())
    }
  )
})
