#' Rename block constructor
#'
#' This block allows renaming columns in a data frame using the visual interface
#' (see [dplyr::rename()]). Changes are applied after clicking the submit button.
#' Uses new_name = old_name syntax where new_name is what you want to call the column
#' and old_name is the current column name.
#'
#' @param renames Named list or vector of renames in new_name = old_name format
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for rename operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon div
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_rename_block(), list(data = mtcars))
#'
#' # With predefined renames
#' serve(
#'   new_rename_block(list(miles_per_gallon = "mpg", cylinders = "cyl")),
#'   list(data = mtcars)
#' )
#'
#' # Connected blocks example
#' serve(
#'   new_board(
#'     blocks = list(
#'       a = new_dataset_block(),
#'       b = new_rename_block(list(horsepower = "hp"))
#'     ),
#'     links = links(
#'       from = c("a"),
#'       to = c("b")
#'     )
#'   )
#' )
#' }
#' @export
new_rename_block <- function(
  renames = list(new_col = "old_col"),
  ...
) {
  # The state must be a list with named elements
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          r_renames <- mod_multi_rename_server(
            id = "mr",
            get_value = \() renames,
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_rename(renames))
          r_renames_validated <- reactiveVal(renames)

          # Validate and update on submit
          observeEvent(input$submit, {
            apply_rename(
              data(),
              r_renames(),
              r_expr_validated,
              r_renames_validated,
              session
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              renames = reactive(as.list(r_renames_validated()))
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        mod_multi_rename_ui(NS(id, "mr")),
        div(
          style = "text-align: right; margin-top: 10px;",
          actionButton(
            NS(id, "submit"),
            "Submit",
            icon = icon("paper-plane"),
            class = "btn-primary"
          )
        )
      )
    },
    class = "rename_block",
    ...
  )
}

#' Parse rename pairs into dplyr expression
#'
#' @param rename_pairs Named list or vector where names are new column names
#'   and values are old column names
#' @return Parsed expression for dplyr::rename()
#' @noRd
parse_rename <- function(rename_pairs = list()) {
  if (
    length(rename_pairs) == 0 ||
      all(names(rename_pairs) == "" | is.na(names(rename_pairs)))
  ) {
    # No renames specified
    return(parse(text = "dplyr::rename(data)")[1])
  } else {
    # Convert list to character vector if needed
    if (is.list(rename_pairs)) {
      rename_pairs <- unlist(rename_pairs)
    }

    # Convert to rename syntax: new_name = old_name
    # Apply backticks to non-syntactic names
    new_names <- backtick_if_needed(names(rename_pairs))
    old_names <- backtick_if_needed(rename_pairs)

    rename_exprs <- paste(
      sprintf("%s = %s", new_names, old_names),
      collapse = ", "
    )
    text <- glue::glue("dplyr::rename(data, {rename_exprs})")
  }
  parse(text = text)[1]
}

#' Apply rename operation with validation
#'
#' @param data Input data frame
#' @param renames Rename pairs to apply
#' @param r_expr_validated Reactive value for validated expression
#' @param r_renames_validated Reactive value for validated renames
#' @noRd
apply_rename <- function(
  data,
  renames,
  r_expr_validated,
  r_renames_validated,
  session = NULL
) {
  # Convert list to character vector if needed
  if (is.list(renames)) {
    renames <- unlist(renames)
  }

  # Validation: check if old column names exist
  if (length(renames) > 0) {
    old_cols <- unname(renames)
    data_cols <- colnames(data)
    missing_cols <- setdiff(old_cols, data_cols)

    if (length(missing_cols) > 0) {
      if (!is.null(session)) {
        showNotification(
          sprintf(
            "Column(s) not found in data: %s",
            paste(missing_cols, collapse = ", ")
          ),
          type = "error",
          duration = 5
        )
      }
      return()
    }

    # Check for duplicate old column names
    if (any(duplicated(old_cols))) {
      duplicate_cols <- old_cols[duplicated(old_cols)]
      if (!is.null(session)) {
        showNotification(
          sprintf(
            "Cannot rename the same column multiple times: %s",
            paste(unique(duplicate_cols), collapse = ", ")
          ),
          type = "error",
          duration = 5
        )
      }
      return()
    }

    # Check for empty new names
    new_names <- names(renames)
    if (any(new_names == "" | is.na(new_names))) {
      if (!is.null(session)) {
        showNotification(
          "All new column names must be provided",
          type = "error",
          duration = 5
        )
      }
      return()
    }
  }

  if (length(renames) == 0) {
    expr <- parse_rename(list())
    r_expr_validated(expr)
    r_renames_validated(renames)
    return()
  }

  expr <- try(parse_rename(renames))

  # Validation
  if (inherits(expr, "try-error")) {
    if (exists("session") && !is.null(session)) {
      showNotification(
        paste("Parse error:", expr),
        type = "error",
        duration = 5
      )
    }
    return()
  }

  # Test the expression
  ans <- try(eval(expr))
  if (inherits(ans, "try-error")) {
    if (exists("session") && !is.null(session)) {
      showNotification(
        paste("Evaluation error:", ans),
        type = "error",
        duration = 5
      )
    }
    return()
  }

  r_expr_validated(expr)
  r_renames_validated(renames)
}
