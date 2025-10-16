#' Multi rename module for multiple column renames
#'
#' A Shiny module that manages multiple old_name -> new_name mappings for column renaming.
#' Supports adding and removing rename pairs dynamically.
#'
#' @param id The module ID
#' @param get_value Function that returns initial values as a named list/vector
#' @param get_cols Function that returns column names for dropdown selection
#'
#' @return A reactive expression containing the current rename pairs
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div selectInput textInput
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @keywords internal
mod_multi_rename_server <- function(id, get_value, get_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    # Handle both list and named vector inputs
    initial_values <- get_value()
    if (is.character(initial_values) && !is.null(names(initial_values))) {
      # Convert named vector to list
      initial_values <- as.list(initial_values)
    }
    if (length(initial_values) == 0 || is.null(initial_values)) {
      available_cols <- get_cols()
      default_old_col <- if (length(available_cols) > 0) {
        available_cols[1]
      } else {
        "old_col"
      }
      initial_values <- list(new_col = default_old_col)
    }

    # Store renames as reactive value
    r_renames <- reactiveVal(initial_values)
    r_cols <- reactive(get_cols())

    # Track which rename indices exist
    r_rename_indices <- reactiveVal(seq_along(initial_values))
    r_next_index <- reactiveVal(length(initial_values) + 1)

    # Collect current values from all inputs
    get_current_renames <- function() {
      indices <- r_rename_indices()
      if (length(indices) == 0) {
        return(list())
      }

      result <- list()
      for (i in indices) {
        new_name_id <- paste0("rename_", i, "_new")
        old_name_id <- paste0("rename_", i, "_old")

        new_name <- input[[new_name_id]]
        old_name <- input[[old_name_id]]

        if (
          !is.null(new_name) &&
            !is.null(old_name) &&
            new_name != "" &&
            old_name != ""
        ) {
          result[[new_name]] <- old_name
        }
      }

      if (length(result) == 0) {
        available_cols <- r_cols()
        default_old_col <- if (length(available_cols) > 0) {
          available_cols[1]
        } else {
          "old_col"
        }
        result <- list(new_col = default_old_col)
      }

      result
    }

    # Add new rename pair
    observeEvent(input$add_rename, {
      current_indices <- r_rename_indices()
      new_index <- r_next_index()

      # Add new index
      r_rename_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Get current renames and add new one
      current <- get_current_renames()

      # Generate unique new name
      available_cols <- r_cols()
      new_name <- "new_col"
      i <- 1
      while (new_name %in% names(current)) {
        new_name <- paste0("new_col_", i)
        i <- i + 1
      }

      # Choose first available old column not already used
      used_old_cols <- unname(unlist(current))
      available_old <- setdiff(available_cols, used_old_cols)
      old_name <- if (length(available_old) > 0) {
        available_old[1]
      } else {
        available_cols[1]
      }

      current[[new_name]] <- old_name
      r_renames(current)
    })

    # Remove rename handlers - create them dynamically
    observe({
      indices <- r_rename_indices()

      lapply(indices, function(i) {
        observeEvent(input[[paste0("rename_", i, "_remove")]], {
          current_indices <- r_rename_indices()

          if (length(current_indices) > 1) {
            # Remove this index
            new_indices <- setdiff(current_indices, i)
            r_rename_indices(new_indices)

            # Update renames
            current <- get_current_renames()
            r_renames(current)
          }
        })
      })
    })

    # Render UI dynamically
    output$renames_ui <- renderUI({
      indices <- r_rename_indices()
      renames <- r_renames()
      available_cols <- r_cols()

      if (length(indices) == 0) {
        return(NULL)
      }

      rename_names <- names(renames)
      rename_values <- unname(renames)

      # Create UI for each rename pair
      tagList(
        lapply(seq_along(indices), function(j) {
          i <- indices[j]
          new_name <- if (j <= length(rename_names)) {
            rename_names[j]
          } else {
            "new_col"
          }
          old_name <- if (j <= length(rename_values)) {
            rename_values[j]
          } else {
            available_cols[1]
          }

          multi_rename_row_ui(
            ns(paste0("rename_", i)),
            new_name = new_name,
            old_name = old_name,
            available_cols = available_cols,
            show_remove = (length(indices) > 1)
          )
        })
      )
    })

    # Return the reactive renames
    reactive({
      # Check if any inputs exist yet - if not, use stored renames
      indices <- r_rename_indices()
      has_inputs <- any(sapply(indices, function(i) {
        paste0("rename_", i, "_new") %in%
          names(input) &&
          paste0("rename_", i, "_old") %in% names(input)
      }))

      if (has_inputs) {
        # Use current input values
        get_current_renames()
      } else {
        # Use stored renames (for initialization)
        r_renames()
      }
    })
  })
}

#' Create multi rename UI module
#'
#' @param id The module ID
#' @param extra_button Optional UI element (e.g., submit button) to display on the right side
#' @return A div containing the UI elements
#' @keywords internal
mod_multi_rename_ui <- function(id, extra_button = NULL) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      .multi-rename-container {
        margin-top: -8px;
      }

      .multi-rename-pair {
        display: flex;
        width: 100%;
        align-items: stretch;
        gap: 8px;
        margin-bottom: 8px;
      }

      .multi-rename-pair .rename-new {
        flex: 0 0 35%;
      }

      .multi-rename-pair .rename-arrow {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        justify-content: center;
        color: var(--bs-gray-600);
        font-size: 1.2em;
        width: 30px;
      }

      .multi-rename-pair .rename-old {
        flex: 1;
      }

      .multi-rename-pair .rename-delete {
        flex: 0 0 auto;
        height: 38px;
        width: 45px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #6c757d;
        border: none;
        background: transparent;
        padding: 0;
      }

      .multi-rename-pair .rename-delete:hover {
        color: #dc3545;
        background: rgba(220, 53, 69, 0.1);
      }

      /* Remove default margins from Shiny inputs */
      .multi-rename-pair .shiny-input-container {
        margin-bottom: 0 !important;
      }

      /* Ensure inputs and selects fill their containers and align properly */
      .multi-rename-pair .form-control,
      .multi-rename-pair .selectize-control,
      .multi-rename-pair .selectize-input {
        width: 100% !important;
        height: 38px !important;
        margin-bottom: 0 !important;
      }

      .multi-rename-pair .selectize-input {
        min-height: 38px;
        line-height: 24px;
        padding-top: 4px;
        padding-bottom: 4px;
        display: flex;
        align-items: center;
      }

      .multi-rename-actions {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-top: 0.5rem;
      }

      .multi-rename-actions .btn-outline-secondary {
        border-color: #dee2e6;
        color: #6c757d;
      }

      .multi-rename-actions .btn-outline-secondary:hover {
        border-color: #adb5bd;
        background-color: #f8f9fa;
        color: #495057;
      }
    "
    ),
    div(
      class = "multi-rename-container",
      uiOutput(ns("renames_ui")),
      div(
        class = "multi-rename-actions",
        actionButton(
          ns("add_rename"),
          label = "Add Rename",
          icon = icon("plus"),
          class = "btn btn-outline-secondary btn-sm"
        ),
        extra_button
      )
    )
  )
}

#' Create UI for a single rename pair row
#'
#' @param id Row identifier
#' @param new_name New column name
#' @param old_name Old column name
#' @param available_cols Available column names for dropdown
#' @param show_remove Whether to show remove button
#' @return A div containing the row UI
multi_rename_row_ui <- function(
  id,
  new_name = "new_col",
  old_name = "",
  available_cols = character(),
  show_remove = TRUE
) {
  div(
    class = "multi-rename-pair",
    div(
      class = "rename-new",
      textInput(
        paste0(id, "_new"),
        label = NULL,
        value = new_name,
        placeholder = "New column name"
      )
    ),
    div(
      class = "rename-arrow",
      icon("arrow-left")
    ),
    div(
      class = "rename-old",
      selectInput(
        paste0(id, "_old"),
        label = NULL,
        choices = available_cols,
        selected = old_name,
        width = "100%"
      )
    ),
    if (show_remove) {
      actionButton(
        paste0(id, "_remove"),
        label = NULL,
        icon = icon("xmark"),
        class = "btn btn-sm rename-delete",
        title = "Remove rename"
      )
    }
  )
}

#' Run example app demonstrating multi rename functionality
#'
#' @examples
#' \dontrun{
#' run_multi_rename_example()
#' }
#' @keywords internal
run_multi_rename_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      h3("Multi Rename Example"),
      mod_multi_rename_ui("mr"),
      hr(),
      h4("Current Renames:"),
      verbatimTextOutput("renames"),
      h4("Generated Code:"),
      verbatimTextOutput("code")
    ),
    server = function(input, output, session) {
      r_result <- mod_multi_rename_server(
        "mr",
        get_value = function() list(miles_per_gallon = "mpg"),
        get_cols = function() c("mpg", "cyl", "hp", "wt", "am", "gear")
      )

      output$renames <- renderPrint({
        renames <- r_result()
        if (length(renames) > 0) {
          for (new_name in names(renames)) {
            cat(sprintf("%s <- %s\n", new_name, renames[[new_name]]))
          }
        }
      })

      output$code <- renderPrint({
        renames <- r_result()
        if (length(renames) > 0) {
          pairs <- paste(
            sprintf("%s = %s", names(renames), renames),
            collapse = ",\n  "
          )
          cat(sprintf("dplyr::rename(data,\n  %s\n)", pairs))
        }
      })
    }
  )
}
