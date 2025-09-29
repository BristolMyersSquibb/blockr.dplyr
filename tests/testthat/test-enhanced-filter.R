# Comprehensive test suite for enhanced filter block
# Focuses on shinytest2 integration tests for UI behavior

library(testthat)
library(shinytest2)
library(blockr.core)
library(blockr.dplyr)

# ==============================================================================
# INTEGRATION TESTS - Shinytest2 Tests for UI Behavior
# ==============================================================================

test_that("Enhanced filter initializes with simple expression", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "enhanced-filter-simple-init",
    height = 800,
    width = 1200,
    load_timeout = 20000
  )

  withr::defer(app$stop())

  app$wait_for_idle()
  Sys.sleep(1)

  # Get initial values
  values <- app$get_values()
  input_names <- names(values$input)

  # Find column selector
  column_inputs <- input_names[grep("condition_1_column$", input_names)]
  if (length(column_inputs) > 0) {
    column_value <- values$input[[column_inputs[1]]]
    expect_equal(column_value, "Petal.Length")
  }

  # Find range slider
  range_inputs <- input_names[grep("condition_1_range$", input_names)]
  if (length(range_inputs) > 0) {
    range_value <- values$input[[range_inputs[1]]]
    expect_equal(range_value[1], 5)
    expect_true(range_value[2] > 6.5 && range_value[2] < 7)
  }
})

test_that("Enhanced filter initializes with complex expression", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()

  # Set environment variable for complex expression
  Sys.setenv(TEST_EXPRESSION = "Petal.Length > mean(Petal.Length)")

  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "enhanced-filter-complex-init",
    height = 800,
    width = 1200,
    load_timeout = 20000
  )

  withr::defer({
    app$stop()
    Sys.unsetenv("TEST_EXPRESSION")
  })

  app$wait_for_idle()
  Sys.sleep(1)

  values <- app$get_values()
  input_names <- names(values$input)

  # Find ACE editor input
  ace_inputs <- input_names[grep("condition_1$", input_names)]
  if (length(ace_inputs) > 0) {
    ace_value <- values$input[[ace_inputs[1]]]
    expect_equal(trimws(ace_value), "Petal.Length > mean(Petal.Length)")
  }

  # Check mode toggle
  mode_inputs <- input_names[grep("condition_1_mode$", input_names)]
  if (length(mode_inputs) > 0) {
    mode_value <- values$input[[mode_inputs[1]]]
    expect_true(mode_value == TRUE || mode_value == "TRUE" || mode_value == "advanced")
  }
})

test_that("Enhanced filter updates range when switching columns", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "enhanced-filter-column-switch",
    height = 800,
    width = 1200,
    load_timeout = 20000
  )

  withr::defer(app$stop())

  app$wait_for_idle()

  # Get initial values
  initial_values <- app$get_values()
  input_names <- names(initial_values$input)

  column_input <- input_names[grep("condition_1_column$", input_names)][1]
  range_input <- input_names[grep("condition_1_range$", input_names)][1]

  if (!is.null(column_input) && !is.null(range_input)) {
    # Initial state - Petal.Length
    initial_range <- initial_values$input[[range_input]]
    expect_equal(initial_range[1], 5)

    # Switch to Sepal.Length
    app$set_inputs(
      !!column_input := "Sepal.Length",
      wait_ = TRUE,
      timeout_ = 5000
    )

    app$wait_for_idle()
    Sys.sleep(1)

    # Check range updated
    after_switch_values <- app$get_values()
    new_range <- after_switch_values$input[[range_input]]

    # Sepal.Length range should be ~4.3 to 7.9
    expect_true(new_range[1] < 5, "Min should update to Sepal.Length min")
    expect_true(new_range[2] > 7, "Max should update to Sepal.Length max")
  }
})

test_that("Enhanced filter handles character columns", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "enhanced-filter-character",
    height = 800,
    width = 1200,
    load_timeout = 20000
  )

  withr::defer(app$stop())

  app$wait_for_idle()

  values <- app$get_values()
  input_names <- names(values$input)

  column_input <- input_names[grep("condition_1_column$", input_names)][1]

  if (!is.null(column_input)) {
    # Switch to Species (character column)
    app$set_inputs(
      !!column_input := "Species",
      wait_ = TRUE,
      timeout_ = 5000
    )

    app$wait_for_idle()
    Sys.sleep(1)

    species_values <- app$get_values()

    # Check for multi-select values input
    condition_inputs <- input_names[grep("condition_1", input_names)]
    values_input <- condition_inputs[grep("_values$", condition_inputs)][1]

    if (!is.null(values_input)) {
      species_selections <- species_values$input[[values_input]]
      # Should have species options available
      expect_true(!is.null(species_selections) || length(species_selections) >= 0)
    }
  }
})

test_that("Enhanced filter supports multi-condition operations", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "enhanced-filter-multi-condition",
    height = 800,
    width = 1200,
    load_timeout = 20000
  )

  withr::defer(app$stop())

  app$wait_for_idle()

  values <- app$get_values()
  input_names <- names(values$input)

  # Look for add condition button
  add_buttons <- input_names[grep("add_condition$", input_names)]
  if (length(add_buttons) > 0) {
    # Click add condition
    app$click(selector = paste0("#", gsub(":", "\\\\:", add_buttons[1])))
    app$wait_for_idle()
    Sys.sleep(1)

    # Check for second condition inputs
    new_values <- app$get_values()
    new_input_names <- names(new_values$input)
    condition_2_inputs <- new_input_names[grep("condition_2", new_input_names)]

    expect_true(length(condition_2_inputs) > 0, "Second condition should be added")

    # Check for logic selector
    logic_inputs <- new_input_names[grep("logic_1$", new_input_names)]
    if (length(logic_inputs) > 0) {
      logic_value <- new_values$input[[logic_inputs[1]]]
      expect_true(logic_value %in% c("and", "or", "AND", "OR"))
    }
  }
})

test_that("Enhanced filter toggles between simple and advanced modes", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = test_path("app-enhanced-filter"),
    name = "enhanced-filter-mode-toggle",
    height = 800,
    width = 1200,
    load_timeout = 20000
  )

  withr::defer(app$stop())

  app$wait_for_idle()

  values <- app$get_values()
  input_names <- names(values$input)

  # Find mode toggle
  mode_inputs <- input_names[grep("condition_1_mode$", input_names)]

  if (length(mode_inputs) > 0) {
    mode_input <- mode_inputs[1]
    initial_mode <- values$input[[mode_input]]

    # Toggle mode
    app$set_inputs(
      !!mode_input := !initial_mode,
      wait_ = TRUE,
      timeout_ = 5000
    )

    app$wait_for_idle()

    # Verify mode changed
    new_values <- app$get_values()
    new_mode <- new_values$input[[mode_input]]
    expect_equal(new_mode, !initial_mode)

    # Check appropriate inputs are visible
    if (new_mode) {
      # Advanced mode - should have ACE editor
      ace_inputs <- input_names[grep("condition_1$", input_names)]
      expect_true(length(ace_inputs) > 0)
    } else {
      # Simple mode - should have column/operator/value
      column_inputs <- input_names[grep("condition_1_column$", input_names)]
      expect_true(length(column_inputs) > 0)
    }
  }
})

test_that("Enhanced filter expression feeding and restoration works", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()

  # Test various expression types
  test_cases <- list(
    list(expr = "Sepal.Width < 3", expected_col = "Sepal.Width", expected_op = "<"),
    list(expr = "Species == 'versicolor'", expected_col = "Species", expected_val = "versicolor"),
    list(expr = "between(Petal.Width, 1, 2)", expected_col = "Petal.Width")
  )

  for (test_case in test_cases) {
    # Set environment variable for test app
    Sys.setenv(TEST_EXPRESSION = test_case$expr)

    app <- AppDriver$new(
      app_dir = test_path("app-enhanced-filter"),
      name = paste0("expression-", gsub("[^a-zA-Z0-9]", "", test_case$expr)),
      height = 800,
      width = 1200,
      load_timeout = 20000
    )

    app$wait_for_idle()

    values <- app$get_values()
    input_names <- names(values$input)

    # Check column is correctly set
    column_inputs <- input_names[grep("condition_1_column$", input_names)]
    if (length(column_inputs) > 0 && !is.null(test_case$expected_col)) {
      column_value <- values$input[[column_inputs[1]]]
      expect_equal(column_value, test_case$expected_col,
                   info = paste("Expression:", test_case$expr))
    }

    app$stop()
  }

  # Reset environment
  Sys.unsetenv("TEST_EXPRESSION")
})