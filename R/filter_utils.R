#' Filter condition utilities
#'
#' Helper functions for managing filter conditions in the enhanced filter block
#' @keywords internal
#' @name filter_utils

#' Create a new filter condition object
#'
#' @param expression Character string with the filter expression
#' @param logical_op Logical operator to combine with previous condition ("&", "|", or NULL for first condition)
#' @param mode Filter mode ("advanced" or "simple")
#' @return A validated filter condition object
#' @keywords internal
new_filter_condition <- function(expression = "TRUE",
                                 logical_op = NULL,
                                 mode = "advanced") {
  # Validate inputs
  validate_filter_condition(
    expression = expression,
    logical_op = logical_op,
    mode = mode
  )

  structure(
    list(
      expression = expression,
      logical_op = logical_op,
      mode = mode
    ),
    class = "filter_condition"
  )
}

#' Validate filter condition parameters
#'
#' @param expression Character string with the filter expression
#' @param logical_op Logical operator ("&", "|", or NULL)
#' @param mode Filter mode ("advanced" or "simple")
#' @return NULL if valid, throws error if invalid
#' @keywords internal
validate_filter_condition <- function(expression, logical_op, mode) {
  # Check expression is character
  if (!is.character(expression) || length(expression) != 1) {
    stop("Expression must be a single character string")
  }

  # Check logical_op
  if (!is.null(logical_op) && !logical_op %in% c("&", "|")) {
    stop("Logical operator must be '&', '|', or NULL")
  }

  # Check mode
  if (!mode %in% c("advanced", "simple")) {
    stop("Mode must be 'advanced' or 'simple'")
  }

  invisible(NULL)
}

#' Split a compound filter expression into individual conditions
#'
#' Takes a filter string like "mpg > 20 & cyl == 4" and splits it into
#' individual condition expressions
#'
#' @param filter_string Character string with filter conditions
#' @return Character vector of individual filter expressions
#' @keywords internal
split_filter_conditions <- function(filter_string) {
  if (is.null(filter_string) || filter_string == "" || filter_string == "TRUE") {
    return("TRUE")
  }

  # Handle parentheses by temporarily replacing content
  paren_content <- list()
  counter <- 0

  # Replace parentheses content with placeholders
  while (grepl("\\([^()]*\\)", filter_string)) {
    counter <- counter + 1
    placeholder <- paste0("__PAREN_", counter, "__")

    match <- regmatches(filter_string, regexpr("\\([^()]*\\)", filter_string))
    paren_content[[placeholder]] <- match

    filter_string <- sub("\\([^()]*\\)", placeholder, filter_string)
  }

  # Split by & and | outside of quotes
  # First, replace quoted strings with placeholders
  quote_content <- list()
  quote_counter <- 0

  # Handle double quotes
  while (grepl('"[^"]*"', filter_string)) {
    quote_counter <- quote_counter + 1
    placeholder <- paste0("__QUOTE_", quote_counter, "__")

    match <- regmatches(filter_string, regexpr('"[^"]*"', filter_string))
    quote_content[[placeholder]] <- match

    filter_string <- sub('"[^"]*"', placeholder, filter_string)
  }

  # Handle single quotes
  while (grepl("'[^']*'", filter_string)) {
    quote_counter <- quote_counter + 1
    placeholder <- paste0("__QUOTE_", quote_counter, "__")

    match <- regmatches(filter_string, regexpr("'[^']*'", filter_string))
    quote_content[[placeholder]] <- match

    filter_string <- sub("'[^']*'", placeholder, filter_string)
  }

  # Now split by & or | with optional spaces
  parts <- strsplit(filter_string, "\\s*[&|]\\s*")[[1]]

  # Restore quotes and parentheses
  for (i in seq_along(parts)) {
    # Restore quotes
    for (placeholder in names(quote_content)) {
      parts[i] <- gsub(placeholder, quote_content[[placeholder]], parts[i], fixed = TRUE)
    }
    # Restore parentheses
    for (placeholder in names(paren_content)) {
      parts[i] <- gsub(placeholder, paren_content[[placeholder]], parts[i], fixed = TRUE)
    }
  }

  # Trim whitespace
  parts <- trimws(parts)

  # Remove empty parts
  parts[parts != ""]
}

#' Extract logical operators from a compound filter expression
#'
#' @param filter_string Character string with filter conditions
#' @return Character vector of logical operators ("&" or "|")
#' @keywords internal
extract_logical_operators <- function(filter_string) {
  if (is.null(filter_string) || filter_string == "" || filter_string == "TRUE") {
    return(character(0))
  }

  # Replace content in parentheses and quotes with placeholders
  temp_string <- filter_string

  # Remove parentheses content
  while (grepl("\\([^()]*\\)", temp_string)) {
    temp_string <- sub("\\([^()]*\\)", "__P__", temp_string)
  }

  # Remove quoted strings
  temp_string <- gsub('"[^"]*"', '__Q__', temp_string)
  temp_string <- gsub("'[^']*'", '__Q__', temp_string)

  # Extract operators
  operators <- character()
  for (i in seq_len(nchar(temp_string))) {
    char <- substr(temp_string, i, i)
    if (char %in% c("&", "|")) {
      operators <- c(operators, char)
    }
  }

  operators
}

#' Convert filter conditions to a filter expression string
#'
#' @param conditions List of filter_condition objects
#' @return Character string with the combined filter expression
#' @keywords internal
conditions_to_expression <- function(conditions) {
  if (length(conditions) == 0) {
    return("TRUE")
  }

  # Build expression string
  expr_parts <- character()

  for (i in seq_along(conditions)) {
    cond <- conditions[[i]]

    # Add logical operator if not the first condition
    if (i > 1 && !is.null(cond$logical_op)) {
      expr_parts <- c(expr_parts, cond$logical_op)
    }

    # Add the expression
    expr_parts <- c(expr_parts, cond$expression)
  }

  # Combine with spaces
  paste(expr_parts, collapse = " ")
}

#' Parse a filter string into condition objects
#'
#' @param filter_string Character string with filter conditions
#' @return List of filter_condition objects
#' @keywords internal
parse_filter_string <- function(filter_string) {
  if (is.null(filter_string) || filter_string == "") {
    return(list(new_filter_condition("TRUE")))
  }

  # Split into individual expressions
  expr_list <- split_filter_conditions(filter_string)

  # Extract logical operators
  operators <- extract_logical_operators(filter_string)

  # Create condition objects
  conditions <- list()
  for (i in seq_along(expr_list)) {
    logical_op <- if (i == 1) NULL else operators[i - 1]
    # Determine if expression is simple or complex
    mode <- if (can_parse_simple(expr_list[[i]])) "simple" else "advanced"
    conditions[[i]] <- new_filter_condition(
      expression = expr_list[[i]],
      logical_op = logical_op,
      mode = mode
    )
  }

  conditions
}

#' Check if an expression can be parsed as simple
#'
#' Simple expressions are those that can be shown in the simple UI:
#' - column == value
#' - column != value
#' - column > value
#' - column < value
#' - column >= value
#' - column <= value
#' - column %in% c(...)
#' - between(column, min, max)
#'
#' @param expression Character string with a filter expression
#' @return Logical indicating if expression is simple
#' @keywords internal
can_parse_simple <- function(expression) {
  if (is.null(expression) || expression == "" || expression == "TRUE") {
    return(FALSE)
  }

  # Pattern for simple comparisons: column [operator] value
  simple_pattern <- "^\\s*[a-zA-Z_][a-zA-Z0-9._]*\\s*(==|!=|>|<|>=|<=|%in%)\\s*"

  # Pattern for between function
  between_pattern <- "^\\s*between\\s*\\("

  grepl(simple_pattern, expression) || grepl(between_pattern, expression)
}

#' Parse a simple expression to extract column and values
#'
#' @param expression Character string with a simple filter expression
#' @return List with column and values (or NULL if can't parse)
#' @keywords internal
parse_simple_expression <- function(expression) {
  if (!can_parse_simple(expression)) {
    return(NULL)
  }

  # Handle between() function
  if (grepl("^\\s*between\\s*\\(", expression)) {
    # Extract: between(column, min, max)
    match <- regmatches(expression, regexec(
      "between\\s*\\(\\s*([^,]+)\\s*,\\s*([^,]+)\\s*,\\s*([^)]+)\\s*\\)",
      expression
    ))[[1]]

    if (length(match) == 4) {
      return(list(
        column = trimws(match[2]),
        values = as.numeric(c(trimws(match[3]), trimws(match[4])))
      ))
    }
  }

  # Handle %in% operator
  if (grepl("%in%", expression)) {
    parts <- strsplit(expression, "%in%")[[1]]
    if (length(parts) == 2) {
      column <- trimws(parts[1])
      # Remove c(...) wrapper and parse values
      values_str <- trimws(parts[2])
      values_str <- gsub("^c\\s*\\(", "", values_str)
      values_str <- gsub("\\)\\s*$", "", values_str)
      # Split by comma and clean up
      values <- strsplit(values_str, ",")[[1]]
      values <- trimws(values)
      # Remove quotes if present
      values <- gsub('^"|"|^\047|\047$', "", values)

      return(list(column = column, values = values))
    }
  }

  # Handle simple comparison operators
  operators <- c("==", "!=", ">=", "<=", ">", "<")
  for (op in operators) {
    if (grepl(op, expression, fixed = TRUE)) {
      parts <- strsplit(expression, op, fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        column <- trimws(parts[1])
        value <- trimws(parts[2])
        # Remove quotes if present
        value <- gsub('^"|"|^\047|\047$', "", value)
        # Try to convert to numeric if possible
        num_value <- suppressWarnings(as.numeric(value))
        if (!is.na(num_value)) {
          value <- num_value
        }

        return(list(column = column, values = value))
      }
    }
  }

  return(NULL)
}

#' Build a simple filter expression
#'
#' Creates a filter expression string from column, operator and values
#'
#' @param column Column name
#' @param operator Comparison operator
#' @param values Values to compare against
#' @return Character string with the filter expression
#' @keywords internal
build_simple_expression <- function(column, operator, values) {
  if (is.null(column) || column == "" || is.null(operator)) {
    return("TRUE")
  }

  # Handle different operators
  if (operator == "between") {
    if (length(values) >= 2) {
      return(sprintf("between(%s, %s, %s)", column, values[1], values[2]))
    }
  } else if (operator == "%in%") {
    if (length(values) > 0) {
      # Quote character values
      if (is.character(values)) {
        value_str <- paste0('"', values, '"', collapse = ", ")
      } else {
        value_str <- paste(values, collapse = ", ")
      }
      return(sprintf("%s %%in%% c(%s)", column, value_str))
    }
  } else if (operator %in% c("==", "!=", ">", "<", ">=", "<=")) {
    if (length(values) > 0) {
      # Quote character values
      if (is.character(values[[1]])) {
        return(sprintf('%s %s "%s"', column, operator, values[[1]]))
      } else {
        return(sprintf("%s %s %s", column, operator, values[[1]]))
      }
    }
  }

  return("TRUE")
}