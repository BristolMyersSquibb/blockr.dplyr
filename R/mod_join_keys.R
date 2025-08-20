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
  
  div(
    class = "join-keys-container",
    h5(label, style = "margin-bottom: 15px;"),
    
    # Natural join option (same column names)
    div(
      class = "form-group",
      checkboxInput(
        ns("use_natural_join"),
        label = "Use natural join (match columns with same names)",
        value = TRUE
      )
    ),
    
    # Natural join column selector
    conditionalPanel(
      condition = sprintf("input['%s']", ns("use_natural_join")),
      selectInput(
        ns("natural_keys"),
        label = "Select columns to join on:",
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
        h6("Custom Column Mappings", style = "margin-top: 15px; margin-bottom: 10px;"),
        uiOutput(ns("join_mappings_ui")),
        div(
          style = "margin-top: 10px;",
          actionButton(
            ns("add_mapping"),
            label = "+ Add Join Key",
            class = "btn-outline-primary btn-sm"
          )
        )
      )
    ),
    
    # Join key summary
    div(
      class = "join-summary mt-3 p-2",
      style = "background-color: #f8f9fa; border-radius: 4px; border: 1px solid #dee2e6;",
      h6("Join Summary:", style = "margin-bottom: 5px; color: #495057;"),
      verbatimTextOutput(ns("join_summary"), placeholder = TRUE)
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
mod_join_keys_server <- function(id, get_x_cols, get_y_cols, initial_keys = character()) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for managing join mappings
    r_custom_mappings <- reactiveVal(list())
    r_natural_join <- reactiveVal(TRUE)
    r_x_cols <- reactiveVal(character())
    r_y_cols <- reactiveVal(character())
    
    # Initialize with provided keys
    observe({
      if (length(initial_keys) > 0) {
        if (is.character(initial_keys)) {
          # Simple character vector - natural join
          updateCheckboxInput(session, "use_natural_join", value = TRUE)
          r_natural_join(TRUE)
        } else if (is.list(initial_keys)) {
          # Complex mapping - custom join
          updateCheckboxInput(session, "use_natural_join", value = FALSE)
          r_natural_join(FALSE)
          r_custom_mappings(initial_keys)
        }
      }
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
            if (is.character(initial_keys)) initial_keys else common_cols
          } else character()
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
      
      # Only render if we have mappings (they should be initialized by now)
      if (length(mappings) > 0) {
        map(seq_along(mappings), function(i) {
          create_mapping_row(session$ns, i, mappings[[i]], r_x_cols(), r_y_cols())
        })
      }
    })
    
    # Add new mapping
    observeEvent(input$add_mapping, {
      current <- r_custom_mappings()
      new_mapping <- list(x_col = "", y_col = "")
      r_custom_mappings(c(current, list(new_mapping)))
    }, ignoreInit = TRUE)
    
    # Handle mapping updates and deletions
    observe({
      # Listen for all mapping input changes
      mappings <- r_custom_mappings()
      if (length(mappings) > 0) {
        updated_mappings <- map(seq_along(mappings), function(i) {
          x_col <- input[[paste0("x_col_", i)]]
          y_col <- input[[paste0("y_col_", i)]]
          list(x_col = x_col %||% "", y_col = y_col %||% "")
        })
        
        # Only update if changed to avoid infinite loops
        if (!identical(updated_mappings, mappings)) {
          r_custom_mappings(updated_mappings)
        }
      }
    })
    
    # Handle mapping deletion
    observe({
      mappings <- r_custom_mappings()
      if (length(mappings) > 0) {
        for (i in seq_along(mappings)) {
          local({
            idx <- i
            observeEvent(input[[paste0("delete_", idx)]], {
              current <- r_custom_mappings()
              if (length(current) > 1) {  # Keep at least one mapping
                r_custom_mappings(current[-idx])
              }
            }, ignoreInit = TRUE)
          })
        }
      }
    })
    
    # Generate join summary
    output$join_summary <- renderText({
      if (r_natural_join()) {
        keys <- input$natural_keys %||% character()
        if (length(keys) == 0) {
          "No join keys selected"
        } else if (length(keys) == 1) {
          paste("Natural join on:", keys[1])
        } else {
          paste("Natural join on:", paste(keys, collapse = ", "))
        }
      } else {
        mappings <- r_custom_mappings()
        valid_mappings <- keep(mappings, ~nzchar(.x$x_col) && nzchar(.x$y_col))
        
        if (length(valid_mappings) == 0) {
          "No valid join mappings configured"
        } else {
          join_specs <- map_chr(valid_mappings, ~paste(.x$x_col, "→", .x$y_col))
          paste("Custom join on:", paste(join_specs, collapse = ", "))
        }
      }
    })
    
    # Return reactive join specification
    reactive({
      if (r_natural_join()) {
        # Natural join - return character vector
        input$natural_keys %||% character()
      } else {
        # Custom join - return list of mappings
        mappings <- r_custom_mappings()
        valid_mappings <- keep(mappings, ~nzchar(.x$x_col) && nzchar(.x$y_col))
        
        if (length(valid_mappings) == 0) {
          character()
        } else {
          # Return in dplyr join_by() format
          join_specs <- map(valid_mappings, function(mapping) {
            if (mapping$x_col == mapping$y_col) {
              mapping$x_col  # Same name
            } else {
              setNames(mapping$y_col, mapping$x_col)  # Different names
            }
          })
          
          if (length(join_specs) == 1 && is.character(join_specs[[1]])) {
            join_specs[[1]]
          } else {
            join_specs
          }
        }
      }
    })
  })
}

# Helper function to create a mapping row UI
create_mapping_row <- function(ns, index, mapping, x_cols, y_cols) {
  div(
    class = "join-mapping-row mb-2",
    style = "display: grid; grid-template-columns: 1fr auto 1fr auto; gap: 10px; align-items: end;",
    
    # Left dataset column
    div(
      selectInput(
        ns(paste0("x_col_", index)),
        label = if (index == 1) "Left Dataset" else NULL,
        choices = c("Select column..." = "", x_cols),
        selected = mapping$x_col,
        width = "100%"
      )
    ),
    
    # Arrow indicator
    div(
      style = "display: flex; align-items: center; height: 38px; margin-bottom: 15px;",
      icon("arrow-right", style = "color: #6c757d;")
    ),
    
    # Right dataset column  
    div(
      selectInput(
        ns(paste0("y_col_", index)),
        label = if (index == 1) "Right Dataset" else NULL,
        choices = c("Select column..." = "", y_cols),
        selected = mapping$y_col,
        width = "100%"
      )
    ),
    
    # Delete button
    div(
      style = "display: flex; align-items: center; height: 38px; margin-bottom: 15px;",
      actionButton(
        ns(paste0("delete_", index)),
        label = NULL,
        icon = icon("trash"),
        class = "btn-outline-danger btn-sm",
        title = "Remove this join key"
      )
    )
  )
}

#' Helper function for NULL default
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Keep function for filtering lists
#' @keywords internal
keep <- function(x, .p) {
  if (length(x) == 0) return(x)
  
  # Handle lambda syntax (~expression) by converting to function
  if (inherits(.p, "formula")) {
    # Extract the formula body and create a proper function
    f_env <- environment(.p)
    f_body <- .p[[2]]  # Get the RHS of the formula
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
    f_body <- .f[[2]]  # Get the RHS of the formula
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
    f_body <- .f[[2]]  # Get the RHS of the formula
    .f <- function(.x) {
      eval(f_body, envir = list(.x = .x), enclos = f_env)
    }
  }
  
  vapply(x, .f, character(1))
}