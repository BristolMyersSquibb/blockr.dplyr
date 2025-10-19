#' Join Keys Mapping Module
#'
#' A reusable Shiny module for configuring join keys between two data frames.
#' Supports both same-name joins (natural joins) and different-name joins
#' with multi-column capability.
#'
#' @param id Character string. Module ID.
#' @param label Character string. Label for the join keys section.
#'
#' @return For UI function, returns a shiny tag. For server function, returns a reactive
#'   containing join key specifications.
#'
#' @export
mod_join_keys_ui <- function(id, label = "Join Keys") {
  ns <- NS(id)

  tagList(
    # Add CSS styling to match rename block
    tags$style(
      "
      .join-keys-container {
        margin-top: 0;
      }

      .join-keys-container .form-group {
        width: 100%;
      }

      .join-keys-container label {
        white-space: nowrap;
      }

      .join-mapping-pair {
        display: flex;
        width: 100%;
        align-items: flex-end;
        gap: 4px;
        margin-bottom: 8px;
      }

      .join-mapping-pair .join-left {
        flex: 0 0 35%;
      }

      .join-mapping-pair .join-arrow {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        justify-content: center;
        color: var(--bs-gray-400);
        font-size: 0.9em;
        width: 30px;
        height: 38px;
        margin-bottom: 0;
      }

      .join-mapping-pair .join-right {
        flex: 1;
      }

      .join-mapping-pair .join-delete {
        flex: 0 0 auto;
        height: 38px;
        width: 35px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #6c757d;
        border: none;
        background: transparent;
        padding: 0;
      }

      .join-mapping-pair .join-delete:hover {
        color: #dc3545;
        background: rgba(220, 53, 69, 0.1);
      }

      /* Remove default margins from Shiny inputs */
      .join-mapping-pair .shiny-input-container {
        margin-bottom: 0 !important;
      }

      /* Ensure inputs and selects fill their containers and align properly */
      .join-mapping-pair .form-control,
      .join-mapping-pair .selectize-control,
      .join-mapping-pair .selectize-input {
        width: 100% !important;
        height: 38px !important;
        margin-bottom: 0 !important;
      }

      .join-mapping-pair .selectize-input {
        min-height: 38px;
        line-height: 24px;
        padding-top: 4px;
        padding-bottom: 4px;
        display: flex;
        align-items: center;
      }

      /* Ensure labels don't add extra bottom margin and align properly */
      .join-mapping-pair label.control-label {
        margin-bottom: 4px;
        line-height: 1.2;
      }

      .join-actions {
        display: flex;
        justify-content: flex-start;
        align-items: center;
        margin-top: 0.5rem;
      }

      .join-actions .btn-outline-secondary {
        border-color: #dee2e6;
        color: #6c757d;
      }

      .join-actions .btn-outline-secondary:hover {
        border-color: #adb5bd;
        background-color: #f8f9fa;
        color: #495057;
      }
      "
    ),

    div(
      class = "join-keys-container",

      # Natural join option (same column names)
      div(
        class = "form-group",
        checkboxInput(
          ns("use_natural_join"),
          label = "Use natural join (match columns with same names)",
          value = FALSE
        )
      ),

      # Natural join column selector
      conditionalPanel(
        condition = sprintf("input['%s']", ns("use_natural_join")),
        selectInput(
          ns("natural_keys"),
          label = "Select columns to join on",
          choices = character(),
          selected = character(),
          multiple = TRUE
        )
      ),

      # Custom join mappings
      conditionalPanel(
        condition = sprintf("!input['%s']", ns("use_natural_join")),
        div(
          id = ns("custom_mappings"),
          h6(
            "Custom Column Mappings",
            style = "margin-top: 15px; margin-bottom: 10px;"
          ),
          uiOutput(ns("join_mappings_ui")),
          div(
            class = "join-actions",
            actionButton(
              ns("add_mapping"),
              label = "Add Join Key",
              icon = icon("plus"),
              class = "btn btn-outline-secondary btn-sm"
            )
          )
        )
      )
    )
  )
}

#' Join Keys Server Module
#'
#' @param id Character string. Module ID.
#' @param get_x_cols Reactive function that returns column names from the first dataset.
#' @param get_y_cols Reactive function that returns column names from the second dataset.
#' @param initial_keys List or character vector. Initial join keys.
#'
#' @export
mod_join_keys_server <- function(
  id,
  get_x_cols,
  get_y_cols,
  initial_keys = character()
) {
  moduleServer(id, function(input, output, session) {
    # Process initial_keys IMMEDIATELY (not in observe) - matches rename block pattern
    initial_mappings <- if (length(initial_keys) > 0) {
      if (is.character(initial_keys)) {
        # Natural join - return empty for custom mappings
        list()
      } else if (is.list(initial_keys)) {
        # Check format and convert
        first_elem <- initial_keys[[1]]
        if (is.list(first_elem) && !is.null(first_elem$x_col)) {
          # Already internal format
          initial_keys
        } else {
          # Named list format - convert to internal
          lapply(names(initial_keys), function(x_col) {
            list(x_col = x_col, y_col = initial_keys[[x_col]])
          })
        }
      } else {
        list()
      }
    } else {
      list()
    }

    # Determine initial natural join state
    # Natural join only when by parameter is empty/NULL
    initial_natural <- length(initial_keys) == 0

    # Initialize reactiveVals with IMMEDIATE values (like rename block)
    r_custom_mappings <- reactiveVal(initial_mappings)
    r_natural_join <- reactiveVal(initial_natural)
    r_x_cols <- reactiveVal(character())
    r_y_cols <- reactiveVal(character())

    # Update checkbox to match initial state (can be in observe since checkbox updates are fine)
    observe({
      updateCheckboxInput(session, "use_natural_join", value = r_natural_join())
    })

    # Update column choices when data changes
    observe({
      req(get_x_cols, get_y_cols)
      x_cols <- get_x_cols()
      y_cols <- get_y_cols()

      if (length(x_cols) > 0 && length(y_cols) > 0) {
        r_x_cols(x_cols)
        r_y_cols(y_cols)

        # Update natural join choices
        common_cols <- intersect(x_cols, y_cols)
        updateSelectInput(
          session,
          "natural_keys",
          choices = common_cols,
          selected = if (r_natural_join()) {
            if (is.character(initial_keys)) {
              initial_keys
            } else {
              common_cols
            }
          } else {
            character()
          }
        )
      }
    })

    # Track natural join toggle
    observeEvent(input$use_natural_join, {
      r_natural_join(input$use_natural_join)

      # When switching to custom mode, ensure we have at least one mapping
      if (!input$use_natural_join && length(r_custom_mappings()) == 0) {
        r_custom_mappings(list(list(x_col = "", y_col = "")))
      }
    })

    # Generate custom mapping UI
    output$join_mappings_ui <- renderUI({
      mappings <- r_custom_mappings()

      # Get column names directly from reactive providers
      req(get_x_cols, get_y_cols)
      x_cols <- get_x_cols()
      y_cols <- get_y_cols()

      # Only render if we have mappings (they should be initialized by now)
      if (length(mappings) > 0) {
        map(seq_along(mappings), function(i) {
          create_mapping_row(
            ns = session$ns,
            index = i,
            mapping = mappings[[i]],
            x_cols = x_cols,
            y_cols = y_cols,
            show_remove = (length(mappings) > 1)
          )
        })
      }
    })

    # Add new mapping
    observeEvent(
      input$add_mapping,
      {
        # Read current values from inputs first
        current <- get_current_mappings()
        if (length(current) == 0) {
          current <- r_custom_mappings()
        }

        new_mapping <- list(x_col = "", y_col = "")
        r_custom_mappings(c(current, list(new_mapping)))
      },
      ignoreInit = TRUE
    )

    # Note: We do NOT sync inputs back to r_custom_mappings() in an observe()
    # This would cause timing issues with renderUI - inputs don't exist yet on first render
    # Instead, we read directly from inputs in the final reactive (see below)

    # Function to read current mappings from inputs
    get_current_mappings <- function() {
      mappings <- r_custom_mappings()
      if (length(mappings) == 0) {
        return(list())
      }

      # Read from inputs
      result <- list()
      for (i in seq_along(mappings)) {
        x_col <- input[[paste0("x_col_", i)]]
        y_col <- input[[paste0("y_col_", i)]]

        if (
          !is.null(x_col) && !is.null(y_col) && nzchar(x_col) && nzchar(y_col)
        ) {
          result <- c(result, list(list(x_col = x_col, y_col = y_col)))
        }
      }
      result
    }

    # Handle mapping deletion
    observe({
      mappings <- r_custom_mappings()
      if (length(mappings) > 0) {
        for (i in seq_along(mappings)) {
          local({
            idx <- i
            observeEvent(
              input[[paste0("delete_", idx)]],
              {
                # Read current values from inputs first
                current <- get_current_mappings()
                if (length(current) == 0) {
                  current <- r_custom_mappings()
                }

                if (length(current) > 1) {
                  # Keep at least one mapping
                  r_custom_mappings(current[-idx])
                }
              },
              ignoreInit = TRUE
            )
          })
        }
      }
    })

    # Return reactive join specification
    reactive({
      if (r_natural_join()) {
        # Natural join - return character vector
        input$natural_keys %||% character()
      } else {
        # Custom join - return named list
        # Two-phase pattern: use stored values if inputs don't exist yet
        mappings <- r_custom_mappings()
        has_inputs <- length(mappings) > 0 &&
          any(sapply(seq_along(mappings), function(i) {
            paste0("x_col_", i) %in%
              names(input) &&
              paste0("y_col_", i) %in% names(input)
          }))

        if (has_inputs) {
          # Phase 2: Read from inputs
          current_mappings <- get_current_mappings()
          valid_mappings <- keep(
            current_mappings,
            ~ nzchar(.x$x_col) && nzchar(.x$y_col)
          )
        } else {
          # Phase 1: Use stored initial values
          valid_mappings <- keep(
            mappings,
            ~ nzchar(.x$x_col) && nzchar(.x$y_col)
          )
        }

        if (length(valid_mappings) == 0) {
          list()
        } else {
          # Convert to named list format: list("left_col" = "right_col")
          result <- list()
          for (mapping in valid_mappings) {
            result[[mapping$x_col]] <- mapping$y_col
          }
          result
        }
      }
    })
  })
}

# Helper function to create a mapping row UI (matches rename block style)
create_mapping_row <- function(
  ns,
  index,
  mapping,
  x_cols,
  y_cols,
  show_remove = TRUE
) {
  div(
    class = "join-mapping-pair",

    # Left dataset column
    div(
      class = "join-left",
      selectInput(
        ns(paste0("x_col_", index)),
        label = if (index == 1) "Left Dataset" else NULL,
        choices = c("Select column..." = "", x_cols),
        selected = mapping$x_col,
        width = "100%"
      )
    ),

    # Equals indicator
    div(
      class = "join-arrow",
      icon("equals")
    ),

    # Right dataset column
    div(
      class = "join-right",
      selectInput(
        ns(paste0("y_col_", index)),
        label = if (index == 1) "Right Dataset" else NULL,
        choices = c("Select column..." = "", y_cols),
        selected = mapping$y_col,
        width = "100%"
      )
    ),

    # Delete button (conditionally rendered like rename block)
    if (show_remove) {
      actionButton(
        ns(paste0("delete_", index)),
        label = NULL,
        icon = icon("xmark"),
        class = "btn btn-sm join-delete",
        title = "Remove join key"
      )
    }
  )
}


#' Keep function for filtering lists
#' @keywords internal
keep <- function(x, .p) {
  if (length(x) == 0) {
    return(x)
  }

  # Handle lambda syntax (~expression) by converting to function
  if (inherits(.p, "formula")) {
    # Extract the formula body and create a proper function
    f_env <- environment(.p)
    f_body <- .p[[2]] # Get the RHS of the formula
    .p <- function(.x) {
      eval(f_body, envir = list(.x = .x), enclos = f_env)
    }
  }

  # Use base R Filter function
  Filter(.p, x)
}

#' Map function for applying functions to lists
#' @keywords internal
map <- function(x, .f) {
  # Handle lambda syntax (~expression) by converting to function
  if (inherits(.f, "formula")) {
    f_env <- environment(.f)
    f_body <- .f[[2]] # Get the RHS of the formula
    .f <- function(.x) {
      eval(f_body, envir = list(.x = .x), enclos = f_env)
    }
  }

  lapply(x, .f)
}

#' Map character function
#' @keywords internal
map_chr <- function(x, .f) {
  # Handle lambda syntax (~expression) by converting to function
  if (inherits(.f, "formula")) {
    f_env <- environment(.f)
    f_body <- .f[[2]] # Get the RHS of the formula
    .f <- function(.x) {
      eval(f_body, envir = list(.x = .x), enclos = f_env)
    }
  }

  vapply(x, .f, character(1))
}
