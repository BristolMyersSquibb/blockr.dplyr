#' Enhanced filter condition module with simple/advanced toggle
#'
#' A Shiny module that manages multiple filter conditions with AND/OR logic.
#' Supports adding and removing conditions dynamically, with toggle between
#' simple and advanced modes (Phase 1: toggle only, no simple UI yet).
#'
#' @param id The module ID
#' @param get_value Function that returns initial values as a character vector or list
#' @param get_cols Function that returns column names for autocompletion
#'
#' @return A reactive expression containing the current filter conditions
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div radioButtons
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @keywords internal
mod_enhanced_filter_server <- function(id, get_value, get_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    initial_value <- get_value()
    if (is.character(initial_value) && length(initial_value) == 1) {
      # Parse single string into conditions (basic implementation)
      if (initial_value == "" || is.null(initial_value)) {
        initial_conditions <- list("TRUE")
      } else {
        # Preserve the actual condition
        initial_conditions <- list(initial_value)
      }
    } else if (is.list(initial_value)) {
      initial_conditions <- initial_value
    } else {
      initial_conditions <- list("TRUE")
    }

    # Store conditions as reactive value
    r_conditions <- reactiveVal(initial_conditions)
    r_cols <- reactive(get_cols())

    # Track which condition indices exist
    r_condition_indices <- reactiveVal(seq_along(initial_conditions))
    r_next_index <- reactiveVal(length(initial_conditions) + 1)

    # Track AND/OR logic between conditions
    r_logic_operators <- reactiveVal(rep(
      "&",
      max(0, length(initial_conditions) - 1)
    ))

    # Track mode for each condition (advanced by default for now)
    r_condition_modes <- reactiveVal(
      setNames(
        rep("advanced", length(initial_conditions)),
        seq_along(initial_conditions)
      )
    )

    # Initialize ACE editors for existing conditions
    observe({
      indices <- r_condition_indices()
      for (i in indices) {
        initialize_ace_editor(session, ns(paste0("condition_", i)), r_cols())
      }
    })

    # Collect current values from all inputs
    get_current_conditions <- function() {
      indices <- r_condition_indices()
      if (length(indices) == 0) {
        return(list())
      }

      result <- list()
      for (i in indices) {
        # For now, always use ACE editor (advanced mode)
        condition_id <- paste0("condition_", i)
        condition <- input[[condition_id]]

        if (!is.null(condition) && condition != "") {
          result <- append(result, condition)
        }
      }

      if (length(result) == 0) {
        result <- list("TRUE")
      }

      result
    }

    # Get current logic operators
    get_current_logic <- function() {
      indices <- r_condition_indices()
      if (length(indices) <= 1) {
        return(character(0))
      }

      operators <- character(0)
      for (i in seq_len(length(indices) - 1)) {
        logic_id <- paste0("logic_", i)
        op <- input[[logic_id]]
        if (is.null(op)) {
          op <- "&"
        } # Default to AND
        operators <- c(operators, op)
      }
      operators
    }

    # Add new condition
    observeEvent(input$add_condition, {
      current_indices <- r_condition_indices()
      new_index <- r_next_index()

      # Add new index
      r_condition_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Add new logic operator if we have more than one condition
      current_logic <- r_logic_operators()
      if (length(current_indices) >= 1) {
        r_logic_operators(c(current_logic, "&")) # Default to AND
      }

      # Add mode for new condition
      current_modes <- r_condition_modes()
      current_modes[[as.character(new_index)]] <- "advanced"
      r_condition_modes(current_modes)

      # Update conditions
      current <- get_current_conditions()
      current <- append(current, "TRUE")
      r_conditions(current)
    })

    # Remove condition handlers - create them dynamically
    observe({
      indices <- r_condition_indices()

      lapply(indices, function(i) {
        observeEvent(input[[paste0("condition_", i, "_remove")]], {
          current_indices <- r_condition_indices()

          if (length(current_indices) > 1) {
            # Remove this index
            new_indices <- setdiff(current_indices, i)
            r_condition_indices(new_indices)

            # Update logic operators (remove one if needed)
            current_logic <- r_logic_operators()
            if (length(current_logic) >= length(new_indices)) {
              # Remove the last operator
              r_logic_operators(current_logic[-length(current_logic)])
            }

            # Remove mode for this condition
            current_modes <- r_condition_modes()
            current_modes[[as.character(i)]] <- NULL
            r_condition_modes(current_modes)

            # Update conditions
            current <- get_current_conditions()
            r_conditions(current)
          }
        })
      })
    })

    # Track mode changes
    observe({
      indices <- r_condition_indices()
      modes <- r_condition_modes()

      for (i in indices) {
        mode_id <- paste0("condition_", i, "_mode")
        current_mode <- input[[mode_id]]

        if (!is.null(current_mode)) {
          modes[[as.character(i)]] <- current_mode
        }
      }

      r_condition_modes(modes)
    })

    # Update logic operators when they change
    observe({
      indices <- r_condition_indices()
      if (length(indices) > 1) {
        new_logic <- get_current_logic()
        r_logic_operators(new_logic)
      }
    })

    # Render UI dynamically
    output$conditions_ui <- renderUI({
      indices <- r_condition_indices()
      conditions <- r_conditions()
      logic_ops <- r_logic_operators()
      modes <- r_condition_modes()

      if (length(indices) == 0) {
        return(NULL)
      }

      # Create UI for each condition
      ui_elements <- list()

      for (j in seq_along(indices)) {
        i <- indices[j]
        condition <- if (j <= length(conditions)) conditions[[j]] else "TRUE"
        mode <- modes[[as.character(i)]] %||% "advanced"

        # Add the condition row with mode toggle
        ui_elements <- append(
          ui_elements,
          list(
            enhanced_filter_condition_ui(
              ns(paste0("condition_", i)),
              value = condition,
              mode = mode,
              show_remove = (length(indices) > 1)
            )
          )
        )

        # Add logic operator dropdown between conditions (except after last)
        if (j < length(indices)) {
          logic_value <- if (j <= length(logic_ops)) logic_ops[j] else "&"
          ui_elements <- append(
            ui_elements,
            list(
              div(
                class = "d-flex justify-content-start my-2",
                selectInput(
                  ns(paste0("logic_", j)),
                  label = NULL,
                  choices = list("AND" = "&", "OR" = "|"),
                  selected = logic_value,
                  width = "80px"
                )
              )
            )
          )
        }
      }

      tagList(ui_elements)
    })

    # Initialize ACE editors when new ones are added
    observeEvent(r_condition_indices(), {
      indices <- r_condition_indices()
      for (i in indices) {
        condition_id <- paste0("condition_", i)
        if (!condition_id %in% names(input)) {
          # New editor, initialize it
          initialize_ace_editor(session, ns(condition_id), r_cols())
        }
      }
    })

    # Return the reactive conditions as a combined string
    reactive({
      # Check if any inputs exist yet - if not, use stored conditions
      indices <- r_condition_indices()
      has_inputs <- any(sapply(indices, function(i) {
        paste0("condition_", i) %in% names(input)
      }))

      if (has_inputs) {
        # Use current input values
        conditions <- get_current_conditions()
        logic_ops <- r_logic_operators()
      } else {
        # Use stored conditions (for initialization)
        conditions <- r_conditions()
        logic_ops <- r_logic_operators()
      }

      if (length(conditions) == 0) {
        return("TRUE")
      } else if (length(conditions) == 1) {
        return(conditions[[1]])
      } else {
        # Combine conditions with logic operators
        result <- conditions[[1]]
        for (i in seq_len(length(conditions) - 1)) {
          op <- if (i <= length(logic_ops)) logic_ops[i] else "&"
          result <- paste(result, op, conditions[[i + 1]])
        }
        return(result)
      }
    })
  })
}

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create enhanced filter UI module
#'
#' @param id The module ID
#' @return A div containing the UI elements
#' @keywords internal
mod_enhanced_filter_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      .enhanced-filter-condition {
        border: 1px solid var(--bs-border-color);
        border-radius: var(--bs-border-radius);
        padding: 10px;
        margin-bottom: 10px;
      }

      .enhanced-filter-condition .mode-toggle {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 10px;
      }

      .enhanced-filter-condition .shiny-ace {
        border: none;
        margin: 7px;
        margin-bottom: 7.5px;
      }

      .enhanced-filter-condition .condition-code {
        flex: 1;
      }

      .enhanced-filter-condition .condition-delete {
        margin-top: 5px;
      }

      .enhanced-filter-condition .condition-delete:hover {
        color: var(--bs-white);
        border-color: var(--bs-danger);
        background: var(--bs-danger);
      }
    "
    ),
    div(
      class = "enhanced-filter-container",
      uiOutput(ns("conditions_ui")),
      div(
        class = "d-flex justify-content-start mt-2",
        actionButton(
          ns("add_condition"),
          label = "Add Condition",
          icon = icon("plus"),
          class = "btn btn-success btn-sm"
        )
      )
    )
  )
}

#' Create UI for a single enhanced filter condition row
#'
#' @param id Row identifier
#' @param value Condition value
#' @param mode Current mode (simple/advanced)
#' @param show_remove Whether to show remove button
#' @return A div containing the row UI
enhanced_filter_condition_ui <- function(id, value = "TRUE", mode = "advanced", show_remove = TRUE) {
  div(
    class = "enhanced-filter-condition",

    # Mode toggle at the top
    div(
      class = "mode-toggle",
      radioButtons(
        paste0(id, "_mode"),
        label = "Mode:",
        choices = c("Advanced" = "advanced", "Simple" = "simple"),
        selected = mode,
        inline = TRUE
      ),
      if (show_remove) {
        actionButton(
          paste0(id, "_remove"),
          label = NULL,
          icon = icon("trash-can"),
          class = "btn btn-outline-danger btn-sm condition-delete"
        )
      }
    ),

    # For now, always show ACE editor (Phase 1)
    # Later we'll add conditionalPanel for simple mode
    div(
      class = "condition-code",
      setup_ace_editor(id, value = value)
    )
  )
}