# Helper functions for shinytest2 integration tests

#' Create a temporary Shiny app for testing blocks
#'
#' @param block_code R code to create the block(s) as a character string
#' @param data_code R code to create the data as a character string (optional)
#' @param data_name Name of the data variable (default: "mtcars")
#' @return Path to the temporary app directory
create_test_app <- function(block_code, data_code = NULL, data_name = "mtcars") {
  app_dir <- tempfile("blockr_test_")
  dir.create(app_dir)

  # Get the package root directory by finding DESCRIPTION file
  # Start from tests/testthat and search upward
  find_pkg_root <- function() {
    # Try testthat::test_path() first (available during test execution)
    if (requireNamespace("testthat", quietly = TRUE)) {
      tryCatch({
        test_dir <- testthat::test_path()
        # Go up from tests/testthat to package root
        pkg_root <- normalizePath(file.path(test_dir, "..", ".."))
        if (file.exists(file.path(pkg_root, "DESCRIPTION"))) {
          return(pkg_root)
        }
      }, error = function(e) {})
    }

    # Fallback: search upward from current directory
    search_dir <- getwd()
    for (i in 1:5) {  # Search up to 5 levels
      if (file.exists(file.path(search_dir, "DESCRIPTION"))) {
        return(normalizePath(search_dir))
      }
      search_dir <- dirname(search_dir)
    }

    # Last resort: assume we're already in package root
    return(getwd())
  }

  pkg_root <- find_pkg_root()

  # Build the app content
  app_lines <- c(
    "library(blockr.core)",
    "",
    "# Load blockr.dplyr from development if available",
    sprintf("pkg_path <- '%s'", pkg_root),
    "if (requireNamespace('devtools', quietly = TRUE) && dir.exists(pkg_path) && file.exists(file.path(pkg_path, 'DESCRIPTION'))) {",
    "  tryCatch(",
    "    devtools::load_all(pkg_path, quiet = TRUE),",
    "    error = function(e) library(blockr.dplyr)",
    "  )",
    "} else {",
    "  library(blockr.dplyr)",
    "}",
    ""
  )

  # Add custom data code if provided
  if (!is.null(data_code)) {
    app_lines <- c(app_lines, data_code, "")
  }

  # Add block code and serve
  app_lines <- c(
    app_lines,
    block_code,
    ""
  )

  app_content <- paste(app_lines, collapse = "\n")
  writeLines(app_content, file.path(app_dir, "app.R"))

  return(app_dir)
}

#' Extract output data from shinytest2 app values
#'
#' This function extracts the data table output from the app's exported values.
#' The exact path may vary depending on the block structure.
#'
#' @param app The AppDriver object
#' @return The output data frame, or NULL if not found
get_output_data <- function(app) {
  values <- app$get_values()

  # Try different possible paths for the output data
  # Path 1: Direct output in export
  if (!is.null(values$export$output)) {
    output_names <- names(values$export$output)

    # Look for common output names
    for (name in output_names) {
      if (grepl("result|data|output", name, ignore.case = TRUE)) {
        data <- values$export$output[[name]]
        if (is.data.frame(data)) {
          return(data)
        }
      }
    }
  }

  # Path 2: Look in the entire values structure
  # This is a fallback - may need adjustment based on actual structure
  all_outputs <- unlist(values, recursive = TRUE)
  warning(
    "Could not find output data in standard locations. ",
    "Available outputs: ",
    paste(names(values$export$output), collapse = ", ")
  )

  return(NULL)
}

#' Verify that output table has expected columns
#'
#' @param app The AppDriver object
#' @param expected_cols Character vector of expected column names
#' @param exact If TRUE, requires exact match. If FALSE, checks that expected
#'   columns are present (but allows additional columns)
verify_table_columns <- function(app, expected_cols, exact = TRUE) {
  data <- get_output_data(app)

  if (is.null(data)) {
    stop("Could not extract output data from app")
  }

  actual_cols <- names(data)

  if (exact) {
    testthat::expect_equal(
      actual_cols,
      expected_cols,
      info = sprintf(
        "Expected columns: %s\nActual columns: %s",
        paste(expected_cols, collapse = ", "),
        paste(actual_cols, collapse = ", ")
      )
    )
  } else {
    missing_cols <- setdiff(expected_cols, actual_cols)
    testthat::expect_length(
      missing_cols,
      0,
      info = sprintf(
        "Missing expected columns: %s",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  invisible(data)
}

#' Verify that output table has expected number of rows
#'
#' @param app The AppDriver object
#' @param expected_rows Expected number of rows
verify_row_count <- function(app, expected_rows) {
  data <- get_output_data(app)

  if (is.null(data)) {
    stop("Could not extract output data from app")
  }

  testthat::expect_equal(
    nrow(data),
    expected_rows,
    info = sprintf(
      "Expected %d rows, got %d rows",
      expected_rows,
      nrow(data)
    )
  )

  invisible(data)
}

#' Verify specific data values in output table
#'
#' @param app The AppDriver object
#' @param column Column name to check
#' @param expected_values Vector of expected values (or function for complex checks)
#' @param ... Additional arguments passed to expect_equal
verify_table_data <- function(app, column, expected_values, ...) {
  data <- get_output_data(app)

  if (is.null(data)) {
    stop("Could not extract output data from app")
  }

  if (!column %in% names(data)) {
    stop(sprintf(
      "Column '%s' not found in output. Available columns: %s",
      column,
      paste(names(data), collapse = ", ")
    ))
  }

  actual_values <- data[[column]]

  if (is.function(expected_values)) {
    # Allow custom verification function
    testthat::expect_true(
      expected_values(actual_values),
      info = sprintf(
        "Custom verification failed for column '%s'",
        column
      )
    )
  } else {
    testthat::expect_equal(actual_values, expected_values, ...)
  }

  invisible(data)
}

#' Wait for a block to be ready
#'
#' @param app The AppDriver object
#' @param block_id The block ID (default: "block_1")
#' @param timeout Timeout in milliseconds (default: 10000)
wait_for_block <- function(app, block_id = "block_1", timeout = 10000) {
  # Wait for the block's result output to be present
  selector <- sprintf("#%s-result", block_id)

  tryCatch(
    {
      app$wait_for_idle(timeout = timeout)
      return(TRUE)
    },
    error = function(e) {
      warning(sprintf(
        "Block '%s' did not become ready within %dms",
        block_id,
        timeout
      ))
      return(FALSE)
    }
  )
}

#' Clean up test app directory
#'
#' @param app_dir Path to the app directory
#' @param app Optional AppDriver object to stop first
cleanup_test_app <- function(app_dir, app = NULL) {
  if (!is.null(app)) {
    tryCatch(
      app$stop(),
      error = function(e) {
        warning("Failed to stop app: ", e$message)
      }
    )
  }

  if (dir.exists(app_dir)) {
    unlink(app_dir, recursive = TRUE)
  }
}