test_that("mod_by_selector_ui creates proper structure", {
  ui <- mod_by_selector_ui("test")

  # Should be a selectInput (which creates a div wrapper)
  expect_s3_class(ui, "shiny.tag")
  expect_equal(ui$name, "div")
  expect_true(grepl("Group by columns", as.character(ui)))

  # Should contain a select elemen
  select_element <- ui$children[[2]]$children[[1]]
  expect_equal(select_element$name, "select")
})

test_that("mod_by_selector_ui with custom label", {
  ui <- mod_by_selector_ui("test", label = "Custom Label")

  expect_true(grepl("Custom Label", ui$children[[1]]))
})

test_that("mod_by_selector_server basic functionality", {
  skip_if_not_installed("shiny")

  testServer(
    mod_by_selector_server,
    args = list(
      get_cols = reactive(c("col1", "col2", "col3")),
      initial_value = c("col1")
    ),
    {
      # Test initial value
      expect_equal(session$returned(), c("col1"))

      # Test column update
      session$setInputs(by_columns = c("col1", "col2"))
      expect_equal(session$returned(), c("col1", "col2"))

      # Test empty selection
      session$setInputs(by_columns = character(0))
      expect_equal(session$returned(), character(0))
    }
  )
})

test_that("mod_by_selector_server handles column updates", {
  skip_if_not_installed("shiny")

  cols <- reactiveVal(c("a", "b"))

  testServer(
    mod_by_selector_server,
    args = list(
      get_cols = cols,
      initial_value = character()
    ),
    {
      # Initial state
      expect_equal(session$returned(), character())

      # Update columns
      cols(c("x", "y", "z"))

      # Should update choices (tested through UI interaction)
      session$setInputs(by_columns = c("x", "y"))
      expect_equal(session$returned(), c("x", "y"))
    }
  )
})

test_that("mod_by_selector_server handles NULL gracefully", {
  skip_if_not_installed("shiny")

  testServer(
    mod_by_selector_server,
    args = list(
      get_cols = reactive(c("col1", "col2")),
      initial_value = character()
    ),
    {
      # Test NULL handling via ignoreNULL = FALSE
      session$setInputs(by_columns = NULL)
      expect_equal(session$returned(), character())
    }
  )
})
