#' Filter condition utilities
#'
#' Helper functions for managing filter conditions in the enhanced filter block
#' @keywords internal

#' Create a new filter condition object
#'
#' @param expression Character string with the filter expression
#' @param logical_op Logical operator to combine with previous condition ("&", "|", or NULL for first condition)
#' @param mode Filter mode ("advanced" or "simple")
#' @return A validated filter condition object
#' @keywords internal
new_filter_condition <- function(
  expression = "TRUE",
  logical_op = NULL,
  mode = "simple"
) {
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
  if (
    is.null(filter_string) || filter_string == "" || filter_string == "TRUE"
  ) {
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
      parts[i] <- gsub(
        placeholder,
        quote_content[[placeholder]],
        parts[i],
        fixed = TRUE
      )
    }
    # Restore parentheses
    for (placeholder in names(paren_content)) {
      parts[i] <- gsub(
        placeholder,
        paren_content[[placeholder]],
        parts[i],
        fixed = TRUE
      )
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
  if (
    is.null(filter_string) || filter_string == "" || filter_string == "TRUE"
  ) {
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
    return(list(new_filter_condition("TRUE", mode = "simple")))
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
  if (is.null(expression) || expression == "") {
    return(FALSE)
  }

  # "TRUE" is a valid simple expression (represents no filter)
  if (expression == "TRUE") {
    return(TRUE)
  }

  # Check for compound expressions (with & or |)
  if (grepl("\\s+[&|]\\s+", expression)) {
    return(FALSE)
  }

  # Check for between function first (it's a special case we handle)
  if (grepl("^\\s*between\\s*\\(", expression)) {
    # Check if between contains function calls in its arguments
    # Extract the arguments inside between()
    between_match <- regmatches(
      expression,
      regexec(
        "between\\s*\\(\\s*([^,]+)\\s*,\\s*([^,]+)\\s*,\\s*([^)]+)\\s*\\)",
        expression
      )
    )

    if (length(between_match[[1]]) > 0) {
      # Check if any argument contains a function call
      args <- between_match[[1]][2:4]
      # Look for function patterns in the arguments
      if (any(grepl("[a-zA-Z_][a-zA-Z0-9._]*\\s*\\(", args))) {
        return(FALSE) # between with function calls is complex
      }
    }
    return(TRUE)
  }

  # Check for function calls (except c() in %in%)
  # But don't check inside quotes
  expr_no_quotes <- gsub("'[^']*'", "", expression)
  expr_no_quotes <- gsub('"[^"]*"', "", expr_no_quotes)

  # Check for functions (but allow c() in %in% expressions)
  if (grepl("[a-zA-Z_][a-zA-Z0-9._]*\\s*\\(", expr_no_quotes)) {
    # Allow c() in %in% expressions
    if (!grepl("%in%\\s*c\\s*\\(", expr_no_quotes)) {
      return(FALSE)
    }
  }

  # Check for arithmetic operators (not inside quotes)
  # Split by comparison operator and check the right side
  if (grepl("(==|!=|>|<|>=|<=)", expression)) {
    parts <- strsplit(expression, "(==|!=|>|<|>=|<=)")[[1]]
    if (length(parts) >= 2) {
      right_side <- parts[2]
      # Remove quoted strings from right side
      right_no_quotes <- gsub("'[^']*'", "", right_side)
      right_no_quotes <- gsub('"[^"]*"', "", right_no_quotes)

      # Check for arithmetic operators (but allow negative numbers)
      # Look for operators with spaces or between identifiers
      if (
        grepl("\\s[+*/-]\\s|[a-zA-Z_][a-zA-Z0-9._]*\\s*[+*/-]", right_no_quotes)
      ) {
        return(FALSE)
      }
    }
  }

  # Check for column-to-column comparisons
  # Extract the column name pattern
  col_pattern <- "(`[^`]+`|[a-zA-Z_][a-zA-Z0-9._]*)"

  # Match pattern: column operator column (where second column is not a known constant)
  if (
    grepl(
      paste0(
        "^\\s*",
        col_pattern,
        "\\s*(==|!=|>|<|>=|<=)\\s*",
        col_pattern,
        "\\s*$"
      ),
      expression
    )
  ) {
    # Check if the right side is actually a column (not a constant)
    parts <- strsplit(expression, "(==|!=|>|<|>=|<=)")[[1]]
    if (length(parts) >= 2) {
      right_val <- trimws(parts[2])
      # If it's not a known R constant, it's probably a column
      if (!right_val %in% c("TRUE", "FALSE", "NA", "NULL", "Inf", "-Inf")) {
        return(FALSE)
      }
    }
  }

  # Check for R constants that are actually functions
  # Build a single regex pattern for all constants
  r_constants_pattern <- "\\b(pi|letters|LETTERS|month\\.abb|month\\.name|NA_character_|NA_complex_|NA_integer_|NA_real_)\\b"
  if (grepl(r_constants_pattern, expression)) {
    return(FALSE)
  }

  # Now check if it matches our simple patterns
  # Pattern 1: column [operator] value
  simple_comparison <- "^\\s*(`[^`]+`|[a-zA-Z_][a-zA-Z0-9._]*)\\s*(==|!=|>|<|>=|<=)\\s*.+$"

  # Pattern 2: column %in% c(...)
  simple_in <- "^\\s*(`[^`]+`|[a-zA-Z_][a-zA-Z0-9._]*)\\s*%in%\\s*c\\s*\\([^)]+\\)\\s*$"

  # Pattern 3: between(column, val1, val2)
  # Already handled above

  if (grepl(simple_comparison, expression) || grepl(simple_in, expression)) {
    return(TRUE)
  }

  return(FALSE)
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
    match <- regmatches(
      expression,
      regexec(
        "between\\s*\\(\\s*([^,]+)\\s*,\\s*([^,]+)\\s*,\\s*([^)]+)\\s*\\)",
        expression
      )
    )[[1]]

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

#' Parse expression for simple mode
#'
#' Extracts column name, values, and operation from filter expression
#'
#' @param expr_str Filter expression string
#' @param cols Available column names
#' @param data Optional data frame for type checking
#' @return List with column, values/range, and include mode, or NULL
#' @keywords internal
parse_simple <- function(expr_str, cols, data = NULL) {
  if (is.null(expr_str) || expr_str == "" || expr_str == "TRUE") {
    return(NULL)
  }

  # Check if expression is simple - if not, return NULL
  if (!can_parse_simple(expr_str)) {
    return(NULL)
  }

  # Find which column is referenced
  # Sort columns by length (longest first) to match more specific names first
  # This ensures "Petal.Length" matches before "Length"
  sorted_cols <- cols[order(nchar(cols), decreasing = TRUE)]

  col_found <- NULL
  for (col in sorted_cols) {
    # Check if column appears with or without backticks
    # First try with backticks (for names with special characters)
    backtick_pattern <- paste0("`", col, "`")
    if (grepl(backtick_pattern, expr_str, fixed = TRUE)) {
      col_found <- col
      break
    }

    # Then try without backticks
    # Escape special regex characters in column name
    # Need to escape each special character individually to avoid regex issues
    escaped_col <- col
    escaped_col <- gsub("\\.", "\\\\.", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\$", "\\\\$", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\^", "\\\\^", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\|", "\\\\|", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\*", "\\\\*", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\+", "\\\\+", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\?", "\\\\?", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\(", "\\\\(", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\)", "\\\\)", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\[", "\\\\[", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\]", "\\\\]", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\{", "\\\\{", escaped_col, fixed = TRUE)
    escaped_col <- gsub("\\}", "\\\\}", escaped_col, fixed = TRUE)
    # Use word boundaries that work with the escaped column name
    # Check for column followed by space, operator, or end of string
    pattern <- paste0("\\b", escaped_col, "(?=[\\s><=!%]|$)")
    if (grepl(pattern, expr_str, perl = TRUE)) {
      col_found <- col
      break
    }
  }

  if (is.null(col_found)) {
    return(NULL)
  }

  # If no data provided, return just the column name
  # This allows UI to initialize with correct column selection
  if (is.null(data) || !is.data.frame(data)) {
    return(list(column = col_found))
  }

  # If we have data, determine column type and parse full details
  if (col_found %in% colnames(data)) {
    col_data <- data[[col_found]]

    if (is.numeric(col_data)) {
      # Parse numeric patterns
      parsed <- parse_numeric_filter(
        expr_str,
        col_found,
        range(col_data, na.rm = TRUE)
      )
      if (!is.null(parsed)) return(parsed)
    } else {
      # Parse character patterns
      parsed <- parse_character_filter(expr_str, col_found)
      if (!is.null(parsed)) return(parsed)
    }
  }

  # If parsing failed but we found a column, return just the column
  return(list(column = col_found))
}

#' Parse numeric filter expression
#'
#' @param expr_str Filter expression string
#' @param col_name Column name
#' @param col_range Numeric range of column values
#' @return List with column and range, or NULL
#' @keywords internal
parse_numeric_filter <- function(expr_str, col_name, col_range) {
  # Pattern: col >= X & col <= Y
  range_pattern <- paste0(
    "\\b",
    col_name,
    "\\s*>=\\s*([0-9.-]+)\\s*&\\s*",
    col_name,
    "\\s*<=\\s*([0-9.-]+)"
  )
  if (grepl(range_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(range_pattern, expr_str))[[1]]
    if (length(matches) == 3) {
      return(list(
        column = col_name,
        range = as.numeric(c(matches[2], matches[3]))
      ))
    }
  }

  # Pattern: col == X
  eq_pattern <- paste0("\\b", col_name, "\\s*==\\s*([0-9.-]+)")
  if (grepl(eq_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(eq_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      val <- as.numeric(matches[2])
      return(list(column = col_name, range = c(val, val)))
    }
  }

  # Pattern: col >= X
  gte_pattern <- paste0("\\b", col_name, "\\s*>=\\s*([0-9.-]+)")
  if (grepl(gte_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(gte_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      return(list(
        column = col_name,
        range = c(as.numeric(matches[2]), col_range[2])
      ))
    }
  }

  # Pattern: col > X
  gt_pattern <- paste0("\\b", col_name, "\\s*>\\s*([0-9.-]+)")
  if (grepl(gt_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(gt_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      val <- as.numeric(matches[2])
      return(list(
        column = col_name,
        range = c(val, col_range[2])
      ))
    }
  }

  # Pattern: col <= X
  lte_pattern <- paste0("\\b", col_name, "\\s*<=\\s*([0-9.-]+)")
  if (grepl(lte_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(lte_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      return(list(
        column = col_name,
        range = c(col_range[1], as.numeric(matches[2]))
      ))
    }
  }

  # Pattern: col < X
  lt_pattern <- paste0("\\b", col_name, "\\s*<\\s*([0-9.-]+)")
  if (grepl(lt_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(lt_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      val <- as.numeric(matches[2])
      return(list(
        column = col_name,
        range = c(col_range[1], val)
      ))
    }
  }

  return(NULL)
}

#' Parse character filter expression
#'
#' @param expr_str Filter expression string
#' @param col_name Column name
#' @return List with column, values, and include mode, or NULL
#' @keywords internal
parse_character_filter <- function(expr_str, col_name) {
  # Pattern: !col %in% c("val1", "val2") - check negated pattern first
  not_in_pattern <- paste0("!\\s*", col_name, "\\s*%in%\\s*c\\(([^)]+)\\)")
  if (grepl(not_in_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(not_in_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      val_str <- matches[2]
      values <- regmatches(val_str, gregexpr('"[^"]*"', val_str))[[1]]
      values <- gsub('"', '', values)
      return(list(
        column = col_name,
        values = values,
        include = FALSE
      ))
    }
  }

  # Pattern: col %in% c("val1", "val2")
  in_pattern <- paste0("\\b", col_name, "\\s*%in%\\s*c\\(([^)]+)\\)")
  if (grepl(in_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(in_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      val_str <- matches[2]
      values <- regmatches(val_str, gregexpr('"[^"]*"', val_str))[[1]]
      values <- gsub('"', '', values)
      return(list(
        column = col_name,
        values = values,
        include = TRUE
      ))
    }
  }

  # Pattern: col == "val"
  eq_pattern <- paste0("\\b", col_name, '\\s*==\\s*"([^"]*)"')
  if (grepl(eq_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(eq_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      return(list(
        column = col_name,
        values = matches[2],
        include = TRUE
      ))
    }
  }

  # Pattern: col != "val"
  neq_pattern <- paste0("\\b", col_name, '\\s*!=\\s*"([^"]*)"')
  if (grepl(neq_pattern, expr_str)) {
    matches <- regmatches(expr_str, regexec(neq_pattern, expr_str))[[1]]
    if (length(matches) == 2) {
      return(list(
        column = col_name,
        values = matches[2],
        include = FALSE
      ))
    }
  }

  return(NULL)
}

#' Build simple filter expression from inputs
#'
#' Creates a filter expression from column name and values
#'
#' @param column Column name
#' @param data Data frame for type checking
#' @param range_val Numeric range values (for numeric columns)
#' @param selected_vals Selected values (for character columns)
#' @param include_mode "include" or "exclude" for character columns
#' @return Character string with filter expression
#' @keywords internal
build_simple <- function(
  column,
  data = NULL,
  range_val = NULL,
  selected_vals = NULL,
  include_mode = "include"
) {
  if (is.null(column) || column == "") {
    return("TRUE")
  }

  if (!is.null(data) && is.data.frame(data) && column %in% colnames(data)) {
    col_data <- data[[column]]

    if (is.numeric(col_data)) {
      # Numeric column - use range values
      if (!is.null(range_val) && length(range_val) == 2) {
        return(format_range(column, range_val, range(col_data, na.rm = TRUE)))
      }
    } else {
      # Character column - use selected values
      if (!is.null(selected_vals) && length(selected_vals) > 0) {
        return(format_values(column, selected_vals, include_mode))
      }
    }
  }

  return("TRUE")
}

#' Format numeric range filter
#'
#' @param column Column name
#' @param range_val Range values
#' @param col_range Full column range
#' @return Filter expression string
#' @keywords internal
format_range <- function(column, range_val, col_range) {
  # Check if values are at the extremes
  at_min <- abs(range_val[1] - col_range[1]) < 0.001
  at_max <- abs(range_val[2] - col_range[2]) < 0.001

  if (range_val[1] == range_val[2]) {
    # Single value
    return(paste0(column, " == ", range_val[1]))
  } else if (at_min && at_max) {
    # Full range - no filter needed
    return("TRUE")
  } else if (at_min) {
    # Only upper bound
    return(paste0(column, " <= ", range_val[2]))
  } else if (at_max) {
    # Only lower bound
    return(paste0(column, " >= ", range_val[1]))
  } else {
    # Both bounds
    return(paste0(
      column,
      " >= ",
      range_val[1],
      " & ",
      column,
      " <= ",
      range_val[2]
    ))
  }
}

#' Format value-based filter
#'
#' @param column Column name
#' @param values Selected values
#' @param include_mode "include" or "exclude"
#' @return Filter expression string
#' @keywords internal
format_values <- function(column, values, include_mode = "include") {
  # Quote character values
  quoted_vals <- paste0('"', values, '"', collapse = ", ")

  if (include_mode == "include") {
    if (length(values) == 1) {
      return(paste0(column, ' == "', values, '"'))
    } else {
      return(paste0(column, " %in% c(", quoted_vals, ")"))
    }
  } else {
    # Exclude mode
    if (length(values) == 1) {
      return(paste0(column, ' != "', values, '"'))
    } else {
      return(paste0("!", column, " %in% c(", quoted_vals, ")"))
    }
  }
}

#' Combine filter conditions with logic operators
#'
#' @param conditions List of condition strings
#' @param operators Character vector of operators ("&" or "|")
#' @return Combined filter expression
#' @keywords internal
combine_filters <- function(conditions, operators = NULL) {
  if (length(conditions) == 0) {
    return("TRUE")
  }

  if (length(conditions) == 1) {
    return(conditions[[1]])
  }

  # Default to AND if no operators provided
  if (is.null(operators) || length(operators) < length(conditions) - 1) {
    operators <- rep("&", length(conditions) - 1)
  }

  # Combine conditions with operators
  result <- conditions[[1]]
  for (i in seq_len(length(conditions) - 1)) {
    result <- paste(result, operators[i], conditions[[i + 1]])
  }

  return(result)
}
