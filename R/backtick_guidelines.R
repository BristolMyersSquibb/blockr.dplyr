#' Guidelines for Handling Non-Syntactic Column Names in blockr
#'
#' @description
#' This document outlines how blockr packages handle non-syntactic column names
#' (names with spaces, special characters, or starting with numbers).
#'
#' @section General Principles:
#'
#' **Three contexts for column name handling:**
#'
#' 1. **User expressions in ACE editors** - Autocompletion adds backticks
#'    - Example: User types "2025" and autocomplete suggests `` `2025 Sales` ``
#'    - Implementation: `ace_utils.R` handles via `backtick_if_needed()`
#'
#' 2. **Generated column names** - Automatically backtick if non-syntactic
#'    - Example: mutate with `Average Sales` = mean(x)
#'    - Implementation: Apply `backtick_if_needed()` to names in parse functions
#'
#' 3. **Column references in generated code** - Always use `backtick_if_needed()`
#'    - Example: select(), arrange(), distinct() building expressions
#'    - Implementation: Replace hardcoded backticks with `backtick_if_needed()`
#'
#' @section Implementation Guidelines:
#'
#' **Use the vectorized utility functions from utils.R:**
#'
#' - `needs_backticks(names)` - Check if names need backticks (returns logical vector)
#' - `backtick_if_needed(names)` - Apply backticks where needed (returns character vector)
#'
#' **Common patterns:**
#'
#' ```r
#' # DON'T: Unconditional backticks (causes double-backticking)
#' sprintf("`%s`", column_names)
#'
#' # DO: Conditional backticks
#' backtick_if_needed(column_names)
#'
#' # DON'T: Manual checking
#' if (make.names(name) != name) { sprintf("`%s`", name) }
#'
#' # DO: Use utility function
#' backtick_if_needed(name)
#' ```
#'
#' @section Examples:
#'
#' **Non-syntactic names that need backticks:**
#' - `2025 Sales` (starts with number)
#' - `Product-Name` (contains hyphen)
#' - `Unit Price ($)` (contains spaces and special characters)
#' - `Is Active?` (contains question mark)
#'
#' **Syntactic names that should NOT get backticks:**
#' - `normal_col`
#' - `my.column` (dots are allowed)
#' - `myColumn123`
#'
#' @section Testing:
#'
#' Always test blocks with mixed column names:
#'
#' ```r
#' test_data <- data.frame(
#'   a = 1:5, b = letters[1:5], c = 10:14, d = 20:24
#' )
#' names(test_data) <- c("2025 Sales", "Product-Name", "Unit Price ($)", "normal_col")
#' ```
#'
#' Verify that:
#' - Expressions are generated correctly
#' - No double-backticking occurs
#' - Syntactic names remain unchanged
#' - The generated dplyr code executes successfully
#'
#' @section Cross-Package Usage:
#'
#' Other blockr packages (e.g., blockr.ggplot) should follow the same guidelines:
#'
#' - Copy or import the utility functions
#' - Apply `backtick_if_needed()` to column references in generated code
#' - Test with non-syntactic column names
#'
#' For ggplot2 aesthetic mappings:
#' ```r
#' # Instead of: aes(x = column_name)
#' # Use: aes(x = `backticked_name`) when needed
#' glue::glue("x = {backtick_if_needed(column_name)}")
#' ```
#'
#' @name backtick_guidelines
#' @keywords internal
NULL