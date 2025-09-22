#' Enhanced filter condition module with simple/advanced toggle
#'
#' A Shiny module that manages multiple filter conditions with AND/OR logic.
#' Supports adding and removing conditions dynamically, with toggle between
#' simple and advanced modes (Phase 1: toggle only, no simple UI yet).
#'
#' @param id The module ID
#' @param get_value Function that returns initial values as a character vector or list
#' @param get_cols Function that returns column names for autocompletion
#' @param get_data Function that returns the current data frame (optional)
#'
#' @return A reactive expression containing the current filter conditions
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div radioButtons selectInput updateSelectInput sliderInput updateRadioButtons showNotification icon
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs useShinyjs runjs show hide delay addClass removeClass enable disable
#' @importFrom htmltools tags
#' @keywords internal
mod_enhanced_filter_server <- function(
  id,
  get_value,
  get_cols,
  get_data = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Use condition constructor from filter_utils.R

    # Initialize with values from get_value
    initial_value <- get_value()
    if (is.character(initial_value) && length(initial_value) == 1) {
      # Parse single string into conditions
      initial_conditions <- parse_filter_string(initial_value)
    } else if (is.list(initial_value)) {
      # Already list of conditions (for state restoration)
      initial_conditions <- initial_value
    } else {
      initial_conditions <- list(new_filter_condition("TRUE", mode = "simple"))
    }

    # Store conditions as reactive value (now stores full objects)
    r_conditions <- reactiveVal(initial_conditions)
    r_cols <- reactive(get_cols())

    # Track which condition indices exist
    r_condition_indices <- reactiveVal(seq_along(initial_conditions))
    r_next_index <- reactiveVal(length(initial_conditions) + 1)

    # Track advanced expressions that are pending (not yet applied)
    r_pending_advanced <- reactiveVal(list())

    # (removed r_current_values - we now update r_conditions directly)

    # Track AND/OR logic between conditions
    r_logic_operators <- reactiveVal(rep(
      "&",
      max(0, length(initial_conditions) - 1)
    ))

    # Removed duplicate ACE initialization observer - see lines 775-786 for the proper one

    # Collect current values from all inputs
    get_current_conditions <- function() {
      indices <- r_condition_indices()
      if (length(indices) == 0) {
        return(list())
      }

      result <- list()
      current_conditions <- r_conditions()

      for (idx in seq_along(indices)) {
        i <- indices[idx]
        # Get mode from condition object
        mode <- if (
          idx <= length(current_conditions) &&
            is.list(current_conditions[[idx]])
        ) {
          current_conditions[[idx]]$mode %||% "simple"
        } else {
          "simple"
        }

        if (mode == "simple") {
          # Build expression from simple UI inputs using utility function
          expr <- build_simple_expression(i)
          if (!is.null(expr) && expr != "TRUE") {
            result <- append(result, expr)
          }
        } else {
          # Advanced mode - use ACE editor
          condition_id <- paste0("condition_", i)
          condition <- input[[condition_id]]

          if (!is.null(condition) && condition != "") {
            result <- append(result, condition)
          }
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

      # Get current conditions - already up-to-date from ACE observers
      current_conditions <- r_conditions()

      # Add the new empty condition
      new_cond <- new_filter_condition(
        "TRUE", # Start with TRUE for new conditions
        if (length(current_conditions) > 0) "&" else NULL,
        mode = "simple" # Default to simple mode for new conditions
      )

      # Append to existing conditions (use list() to preserve structure)
      r_conditions(append(current_conditions, list(new_cond)))

      # Add new index
      r_condition_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Add new logic operator if we have more than one condition
      current_logic <- r_logic_operators()
      if (length(current_indices) >= 1) {
        r_logic_operators(c(current_logic, "&")) # Default to AND
      }
    })

    # Remove condition handlers - create them dynamically
    observe({
      indices <- r_condition_indices()

      lapply(indices, function(i) {
        observeEvent(input[[paste0("condition_", i, "_remove")]], {
          current_indices <- r_condition_indices()

          if (length(current_indices) > 1) {
            # Find position of i in current_indices
            position_to_remove <- which(current_indices == i)

            # Only proceed if we found the position
            if (length(position_to_remove) > 0) {
              # Remove the condition from r_conditions at the correct position
              current_conditions <- r_conditions()
              if (position_to_remove <= length(current_conditions)) {
                current_conditions <- current_conditions[-position_to_remove]
                r_conditions(current_conditions)
              }

              # Remove this index from indices
              new_indices <- setdiff(current_indices, i)
              r_condition_indices(new_indices)

              # Update logic operators (remove one if needed)
              current_logic <- r_logic_operators()
              if (length(current_logic) >= length(new_indices)) {
                # Remove the corresponding operator
                if (position_to_remove <= length(current_logic)) {
                  current_logic <- current_logic[-position_to_remove]
                } else if (length(current_logic) > 0) {
                  current_logic <- current_logic[-length(current_logic)]
                }
                r_logic_operators(current_logic)
              }
            }
          }
        })
      })
    })

    # Wrapper function for parse_simple utility
    parse_expression_for_simple <- function(expr_str, i) {
      parse_simple(expr_str, r_cols(), get_data())
    }

    # Track mode changes and update conditionalPanel
    observe({
      indices <- r_condition_indices()
      current_conditions <- r_conditions()

      for (i in indices) {
        mode_id <- paste0("condition_", i, "_mode")
        mode_value <- input[[mode_id]]

        if (!is.null(mode_value)) {
          # Convert checkbox value to mode string
          current_mode <- if (mode_value) "advanced" else "simple"

          # Update mode label
          shinyjs::html(
            paste0("condition_", i, "_mode_label"),
            if (mode_value) "Advanced" else "Simple"
          )

          # Get the old mode from the condition object
          cond_index <- which(indices == i)
          old_mode <- if (
            length(cond_index) == 1 && cond_index <= length(current_conditions)
          ) {
            if (is.list(current_conditions[[cond_index]])) {
              current_conditions[[cond_index]]$mode
            } else {
              "simple"
            }
          } else {
            "simple"
          }

          # Only process if mode actually changed
          if (old_mode != current_mode) {
            # Update the mode in the actual condition object
            if (
              length(cond_index) == 1 &&
                cond_index <= length(current_conditions)
            ) {
              if (is.list(current_conditions[[cond_index]])) {
                current_conditions[[cond_index]]$mode <- current_mode
                r_conditions(current_conditions)
              }
            }

            # Use shinyjs to toggle visibility based on mode
            if (current_mode == "simple") {
              shinyjs::show(paste0("condition_", i, "_simple_panel"))
              shinyjs::hide(paste0("condition_", i, "_advanced_panel"))

              # When switching to simple mode, try to parse the expression
              if (old_mode != "simple") {
                expr_str <- input[[paste0("condition_", i)]]
                parsed <- parse_simple(expr_str, r_cols(), get_data())

                if (!is.null(parsed)) {
                  # Update column selection
                  updateSelectInput(
                    session,
                    paste0("condition_", i, "_column"),
                    selected = parsed$column
                  )

                  # Capture values in local scope before delay
                  local({
                    local_i <- i
                    local_range <- parsed$range
                    local_values <- parsed$values
                    local_include <- parsed$include

                    # Small delay to allow column change to process
                    shinyjs::delay(50, {
                      if (!is.null(local_range)) {
                        # Update range slider
                        updateSliderInput(
                          session,
                          paste0("condition_", local_i, "_range"),
                          value = local_range
                        )
                      } else if (!is.null(local_values)) {
                        # Update multi-select and include/exclude
                        updateSelectInput(
                          session,
                          paste0("condition_", local_i, "_values"),
                          selected = local_values
                        )
                        updateRadioButtons(
                          session,
                          paste0("condition_", local_i, "_include"),
                          selected = if (local_include) "include" else "exclude"
                        )
                      }
                    })
                  })
                }
              }
            } else {
              # Switching to advanced mode
              shinyjs::hide(paste0("condition_", i, "_simple_panel"))
              shinyjs::show(paste0("condition_", i, "_advanced_panel"))

              # Disable apply button initially when switching to advanced
              shinyjs::delay(50, {
                shinyjs::disable(paste0("condition_", i, "_apply"))
                shinyjs::removeClass(
                  selector = paste0(
                    "#",
                    session$ns(paste0("condition_", i, "_apply"))
                  ),
                  class = "has-changes"
                )
              })

              # When switching from simple to advanced, ensure the ACE editor shows the built expression
              if (old_mode == "simple") {
                # Get the current expression in the ACE editor
                current_expr <- input[[paste0("condition_", i)]]

                # Build expression from simple mode inputs
                expr <- build_simple_expression(i)

                # Only update if the expressions are actually different
                if (
                  !is.null(expr) &&
                    !is.null(current_expr) &&
                    expr != current_expr
                ) {
                  # Use a delay to ensure ACE editor is visible and initialized
                  # But capture the expression value to avoid race conditions
                  local({
                    local_expr <- expr
                    local_i <- i
                    shinyjs::delay(150, {
                      updateAceEditor(
                        session,
                        paste0("condition_", local_i),
                        value = local_expr
                      )
                    })
                  })
                }
              }
            }
          }
        }
      }
    })

    # Update column dropdowns when data changes or UI is created
    observe({
      indices <- r_condition_indices()
      cols <- r_cols()

      if (length(cols) == 0) {
        return()
      }

      for (i in indices) {
        column_id <- paste0("condition_", i, "_column")

        # Update column choices
        updateSelectInput(
          session,
          inputId = column_id,
          choices = cols,
          selected = input[[column_id]] %||% cols[1]
        )
      }
    })

    # Update simple UI based on column selection
    observe({
      indices <- r_condition_indices()

      # Skip if no get_data function provided
      if (is.null(get_data)) {
        return()
      }

      data <- get_data() # Get current data to check column types

      # If data is not a data frame, we can't determine column types yet
      if (!is.data.frame(data)) {
        return()
      }

      for (i in indices) {
        local({
          idx <- i # Capture i in local scope
          column_id <- paste0("condition_", idx, "_column")

          # Watch for column changes
          observeEvent(input[[column_id]], {
            selected_col <- input[[column_id]]

            # Store previous value to detect actual changes
            prev_col_key <- paste0("prev_col_", idx)
            prev_col <- isolate(session$userData[[prev_col_key]])

            # Only proceed if column actually changed
            if (!is.null(prev_col) && identical(prev_col, selected_col)) {
              return() # No change, don't update
            }

            # Update stored previous value
            session$userData[[prev_col_key]] <- selected_col

            # Get the namespace-qualified IDs for UI elements
            numeric_ui_id <- paste0("condition_", idx, "_numeric_ui")
            character_ui_id <- paste0("condition_", idx, "_character_ui")

            if (
              is.null(selected_col) ||
                selected_col == "" ||
                !selected_col %in% colnames(data)
            ) {
              # Hide both UIs if no valid column selected
              shinyjs::hide(numeric_ui_id)
              shinyjs::hide(character_ui_id)
              return()
            }

            col_data <- data[[selected_col]]

            if (is.numeric(col_data)) {
              # Show numeric UI, hide character UI
              shinyjs::show(numeric_ui_id)
              shinyjs::hide(character_ui_id)

              # Update slider range and values
              col_range <- range(col_data, na.rm = TRUE)

              # Get the actual range from the stored condition
              current_conditions <- r_conditions()
              current_indices <- r_condition_indices()
              cond_index <- which(current_indices == idx)
              slider_value <- col_range # Default to full range

              # Validate that we have exactly one matching index
              if (length(cond_index) != 1) {
                return()
              }

              if (cond_index <= length(current_conditions)) {
                cond_obj <- current_conditions[[cond_index]]
                expr <- if (is.list(cond_obj)) cond_obj$expression else cond_obj

                # Parse the expression to get the range
                if (!is.null(expr) && expr != "" && expr != "TRUE") {
                  parsed <- parse_simple(expr, colnames(data), data)
                  if (!is.null(parsed) && !is.null(parsed$range)) {
                    slider_value <- parsed$range
                  }
                }
              }

              updateSliderInput(
                session,
                paste0("condition_", idx, "_range"),
                min = col_range[1],
                max = col_range[2],
                value = slider_value, # Use parsed value, not col_range
                step = if (all(col_data == floor(col_data), na.rm = TRUE)) {
                  1
                } else {
                  0.01
                }
              )
            } else {
              # Show character UI, hide numeric UI
              shinyjs::hide(numeric_ui_id)
              shinyjs::show(character_ui_id)

              # Update multi-select choices
              unique_vals <- unique(as.character(col_data))
              unique_vals <- unique_vals[!is.na(unique_vals)]

              # Get the actual selected values from the stored condition
              current_conditions <- r_conditions()
              current_indices <- r_condition_indices()
              cond_index <- which(current_indices == idx)
              selected_vals <- if (length(unique_vals) > 0) {
                unique_vals[1]
              } else {
                NULL
              } # Default

              # Validate that we have exactly one matching index
              if (length(cond_index) != 1) {
                return()
              }

              if (cond_index <= length(current_conditions)) {
                cond_obj <- current_conditions[[cond_index]]
                expr <- if (is.list(cond_obj)) cond_obj$expression else cond_obj

                # Parse the expression to get the selected values
                if (!is.null(expr) && expr != "" && expr != "TRUE") {
                  parsed <- parse_simple(expr, colnames(data), data)
                  if (!is.null(parsed) && !is.null(parsed$values)) {
                    selected_vals <- parsed$values
                    # Also update the include/exclude radio button
                    updateRadioButtons(
                      session,
                      paste0("condition_", idx, "_include"),
                      selected = if (parsed$include) "include" else "exclude"
                    )
                  }
                }
              }

              updateSelectInput(
                session,
                paste0("condition_", idx, "_values"),
                choices = unique_vals,
                selected = selected_vals # Use parsed values
              )
            }
          })
        })
      }
    })

    # Function to build expression from simple UI for a specific condition
    build_simple_expression <- function(i) {
      column_id <- paste0("condition_", i, "_column")
      selected_col <- input[[column_id]]

      if (is.null(selected_col) || selected_col == "") {
        return("TRUE")
      }

      data <- get_data()
      range_id <- paste0("condition_", i, "_range")
      range_val <- input[[range_id]]
      values_id <- paste0("condition_", i, "_values")
      selected_vals <- input[[values_id]]
      include_id <- paste0("condition_", i, "_include")
      include_mode <- input[[include_id]] %||% "include"

      build_simple(
        column = selected_col,
        data = data,
        range_val = range_val,
        selected_vals = selected_vals,
        include_mode = include_mode
      )
    }

    # Track which observers have been created to avoid duplicates
    observers_created <- reactiveVal(list())

    # Update ACE editor when simple UI inputs change
    # Create separate observers for each condition's simple UI inputs
    observe({
      indices <- r_condition_indices()
      created <- isolate(observers_created())

      for (i in indices) {
        # Only create observers if they haven't been created yet
        if (!as.character(i) %in% names(created)) {
          local({
            idx <- i

            # Watch column selection
            observeEvent(
              input[[paste0("condition_", idx, "_column")]],
              {
                # Check if in simple mode
                current_conditions <- isolate(r_conditions())
                current_indices <- isolate(r_condition_indices())
                cond_index <- which(current_indices == idx)

                in_simple_mode <- FALSE
                if (
                  length(cond_index) == 1 &&
                    cond_index <= length(current_conditions)
                ) {
                  if (is.list(current_conditions[[cond_index]])) {
                    in_simple_mode <- current_conditions[[cond_index]]$mode ==
                      "simple"
                  }
                }

                if (in_simple_mode) {
                  expr <- build_simple_expression(idx)
                  updateAceEditor(
                    session,
                    paste0("condition_", idx),
                    value = expr
                  )
                }
              },
              ignoreInit = TRUE
            )

            # Watch range slider
            observeEvent(
              input[[paste0("condition_", idx, "_range")]],
              {
                # Check if in simple mode
                current_conditions <- isolate(r_conditions())
                current_indices <- isolate(r_condition_indices())
                cond_index <- which(current_indices == idx)

                in_simple_mode <- FALSE
                if (
                  length(cond_index) == 1 &&
                    cond_index <= length(current_conditions)
                ) {
                  if (is.list(current_conditions[[cond_index]])) {
                    in_simple_mode <- current_conditions[[cond_index]]$mode ==
                      "simple"
                  }
                }

                if (in_simple_mode) {
                  expr <- build_simple_expression(idx)
                  updateAceEditor(
                    session,
                    paste0("condition_", idx),
                    value = expr
                  )
                }
              },
              ignoreInit = TRUE
            )

            # Watch values selection
            observeEvent(
              input[[paste0("condition_", idx, "_values")]],
              {
                # Check if in simple mode
                current_conditions <- isolate(r_conditions())
                current_indices <- isolate(r_condition_indices())
                cond_index <- which(current_indices == idx)

                in_simple_mode <- FALSE
                if (
                  length(cond_index) == 1 &&
                    cond_index <= length(current_conditions)
                ) {
                  if (is.list(current_conditions[[cond_index]])) {
                    in_simple_mode <- current_conditions[[cond_index]]$mode ==
                      "simple"
                  }
                }

                if (in_simple_mode) {
                  expr <- build_simple_expression(idx)
                  updateAceEditor(
                    session,
                    paste0("condition_", idx),
                    value = expr
                  )
                }
              },
              ignoreInit = TRUE
            )

            # Watch include/exclude radio
            observeEvent(
              input[[paste0("condition_", idx, "_include")]],
              {
                # Check if in simple mode
                current_conditions <- isolate(r_conditions())
                current_indices <- isolate(r_condition_indices())
                cond_index <- which(current_indices == idx)

                in_simple_mode <- FALSE
                if (
                  length(cond_index) == 1 &&
                    cond_index <= length(current_conditions)
                ) {
                  if (is.list(current_conditions[[cond_index]])) {
                    in_simple_mode <- current_conditions[[cond_index]]$mode ==
                      "simple"
                  }
                }

                if (in_simple_mode) {
                  expr <- build_simple_expression(idx)
                  updateAceEditor(
                    session,
                    paste0("condition_", idx),
                    value = expr
                  )
                }
              },
              ignoreInit = TRUE
            )
          })

          # Mark this observer as created
          created[[as.character(i)]] <- TRUE
          observers_created(created)
        }
      }
    })

    # Create observers for each logic dropdown
    logic_observers <- list()
    observe({
      indices <- r_condition_indices()

      # Create observers for each logic dropdown between conditions
      if (length(indices) > 1) {
        for (j in seq_len(length(indices) - 1)) {
          logic_id <- paste0("logic_", j)
          observer_id <- paste0("logic_observer_", j)

          # Create observer if it doesn't exist
          if (is.null(logic_observers[[observer_id]])) {
            logic_observers[[observer_id]] <<- observeEvent(
              input[[logic_id]],
              {
                logic_val <- input[[logic_id]]
                if (!is.null(logic_val)) {
                  # Update the logical_op in the corresponding condition
                  current_conditions <- r_conditions()

                  # The logic operator between condition j and j+1 belongs to condition j+1
                  if ((j + 1) <= length(current_conditions)) {
                    if (is.list(current_conditions[[j + 1]])) {
                      current_conditions[[j + 1]]$logical_op <- logic_val
                      r_conditions(current_conditions)
                    }
                  }

                  # Also update r_logic_operators for consistency
                  current_logic <- r_logic_operators()
                  if (j <= length(current_logic)) {
                    current_logic[j] <- logic_val
                    r_logic_operators(current_logic)
                  }
                }
              },
              ignoreInit = FALSE
            )
          }
        }
      }
    })

    # Create observers for each ACE editor to track pending changes
    ace_observers <- list()
    observe({
      indices <- r_condition_indices()
      current_conditions <- isolate(r_conditions())

      for (j in seq_along(indices)) {
        i <- indices[j]
        ace_id <- paste0("condition_", i)
        observer_id <- paste0("ace_observer_", i)

        # Create observer if it doesn't exist
        if (is.null(ace_observers[[observer_id]])) {
          ace_observers[[observer_id]] <<- observeEvent(
            input[[ace_id]],
            {
              ace_val <- input[[ace_id]]
              if (!is.null(ace_val)) {
                # Check if this is in advanced mode
                conditions <- r_conditions()
                current_indices <- r_condition_indices()
                cond_index <- which(current_indices == i)

                if (
                  length(cond_index) > 0 && cond_index <= length(conditions)
                ) {
                  cond_obj <- conditions[[cond_index]]
                  is_advanced <- is.list(cond_obj) &&
                    (cond_obj$mode %||% "simple") == "advanced"

                  if (is_advanced) {
                    # Mark this condition as pending (not applied yet)
                    pending <- r_pending_advanced()
                    pending[[as.character(i)]] <- ace_val
                    r_pending_advanced(pending)

                    # Enable the apply button when there are changes
                    shinyjs::enable(paste0("condition_", i, "_apply"))
                    shinyjs::addClass(
                      selector = paste0(
                        "#",
                        session$ns(paste0("condition_", i, "_apply"))
                      ),
                      class = "has-changes"
                    )
                  } else {
                    # Simple mode - update immediately
                    if (is.list(conditions[[cond_index]])) {
                      conditions[[cond_index]]$expression <- ace_val
                    } else {
                      # Legacy string - convert to object
                      conditions[[cond_index]] <- new_filter_condition(
                        ace_val,
                        if (cond_index == 1) NULL else "&"
                      )
                    }
                    # Update immediately for simple mode
                    r_conditions(conditions)
                  }
                }
              }
            },
            ignoreInit = TRUE
          )
        }
      }
    })

    # Create observers for apply buttons in advanced mode
    apply_observers <- list()
    observe({
      indices <- r_condition_indices()

      for (i in indices) {
        apply_id <- paste0("condition_", i, "_apply")
        observer_id <- paste0("apply_observer_", i)

        if (is.null(apply_observers[[observer_id]])) {
          apply_observers[[observer_id]] <<- observeEvent(
            input[[apply_id]],
            {
              # Get the current ACE editor value
              ace_val <- input[[paste0("condition_", i)]]

              if (!is.null(ace_val)) {
                # Apply this advanced expression
                conditions <- r_conditions()
                current_indices <- r_condition_indices()
                cond_index <- which(current_indices == i)

                if (
                  length(cond_index) > 0 && cond_index <= length(conditions)
                ) {
                  # Update the condition
                  if (is.list(conditions[[cond_index]])) {
                    conditions[[cond_index]]$expression <- ace_val
                  } else {
                    conditions[[cond_index]] <- new_filter_condition(
                      ace_val,
                      if (cond_index == 1) NULL else "&"
                    )
                  }
                  r_conditions(conditions)

                  # Remove from pending
                  pending <- r_pending_advanced()
                  pending[[as.character(i)]] <- NULL
                  r_pending_advanced(pending)

                  # Disable the apply button after applying
                  shinyjs::disable(paste0("condition_", i, "_apply"))
                  shinyjs::removeClass(
                    selector = paste0(
                      "#",
                      session$ns(paste0("condition_", i, "_apply"))
                    ),
                    class = "has-changes"
                  )

                  # Show success feedback
                  showNotification(
                    "Filter expression applied",
                    type = "message",
                    duration = 2
                  )
                }
              }
            }
          )
        }
      }
    })

    # Render UI dynamically
    output$conditions_ui <- renderUI({
      indices <- r_condition_indices()
      # Isolate these so we don't re-render on content changes
      conditions <- isolate(r_conditions())
      logic_ops <- isolate(r_logic_operators())
      cols <- r_cols()

      if (length(indices) == 0) {
        return(NULL)
      }

      # Don't try to use ACE values here - let the UI initialize with stored conditions
      # The ACE editors will update themselves via observers

      # Create UI for each condition
      ui_elements <- list()

      for (position in seq_along(indices)) {
        condition_id <- indices[position]
        # Use stored condition from r_conditions (our single source of truth)
        # Access by position in the list, not by the condition ID!
        if (position <= length(conditions)) {
          cond_obj <- conditions[[position]]
          # Extract expression from condition object
          if (is.list(cond_obj)) {
            condition <- cond_obj$expression %||% ""
            # Use mode from condition object
            mode <- cond_obj$mode %||% "simple"
          } else {
            # Legacy string format
            condition <- cond_obj
            mode <- "simple"
          }
        } else {
          condition <- "TRUE" # Empty expression for new conditions
          mode <- "simple"
        }

        # Add the condition row with mode toggle
        data_for_ui <- if (!is.null(get_data)) get_data() else NULL

        ui_elements <- append(
          ui_elements,
          list(
            enhanced_filter_condition_ui(
              ns(paste0("condition_", condition_id)),
              value = condition,
              mode = mode,
              show_remove = (length(indices) > 1),
              available_columns = cols,
              data = data_for_ui
            )
          )
        )

        # Add logic operator dropdown between conditions (except after last)
        if (position < length(indices)) {
          logic_value <- if (position <= length(logic_ops)) {
            logic_ops[position]
          } else {
            "&"
          }
          ui_elements <- append(
            ui_elements,
            list(
              div(
                class = "d-flex justify-content-start my-2",
                selectInput(
                  ns(paste0("logic_", position)),
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

    # Initialize ACE editors when new conditions are added
    observeEvent(r_condition_indices(), {
      indices <- r_condition_indices()
      cols <- r_cols()
      conditions <- r_conditions()

      for (i in indices) {
        condition_id <- paste0("condition_", i)
        if (!condition_id %in% names(input)) {
          # New editor, initialize it
          initialize_ace_editor(session, ns(condition_id), cols)
        }

        # Initialize apply button state - disabled by default
        # Find the condition index
        cond_index <- which(indices == i)
        if (length(cond_index) > 0 && cond_index <= length(conditions)) {
          cond_obj <- conditions[[cond_index]]
          is_advanced <- is.list(cond_obj) &&
            (cond_obj$mode %||% "simple") == "advanced"
          if (is_advanced) {
            # Start disabled for advanced mode
            shinyjs::delay(100, {
              shinyjs::disable(paste0("condition_", i, "_apply"))
            })
          }
        }
      }
    })

    # Return the combined filter string and pending status
    filter_string <- reactive({
      # Always get the latest conditions from r_conditions()
      conditions <- r_conditions()

      if (length(conditions) == 0) {
        return("TRUE")
      } else if (length(conditions) == 1) {
        # Extract expression from condition object if needed
        cond <- conditions[[1]]
        if (is.list(cond)) {
          return(cond$expression %||% "TRUE")
        } else {
          return(cond)
        }
      } else {
        # Build filter expression using logical operators from condition objects
        expressions <- character(0)
        for (i in seq_along(conditions)) {
          cond <- conditions[[i]]
          if (is.list(cond)) {
            expr <- cond$expression %||% "TRUE"
            expressions <- c(expressions, expr)
          } else {
            expressions <- c(expressions, cond)
          }
        }

        # Combine conditions using the logical_op from each condition
        result <- expressions[[1]]
        for (i in 2:length(expressions)) {
          # Get the logical operator from the condition object (not from r_logic_operators)
          op <- "&" # Default to AND
          if (i <= length(conditions) && is.list(conditions[[i]])) {
            op <- conditions[[i]]$logical_op %||% "&"
          }
          result <- paste(result, op, expressions[[i]])
        }
        return(result)
      }
    })

    # Check if any advanced expressions are pending
    has_pending_advanced <- reactive({
      length(r_pending_advanced()) > 0
    })

    # Return both the filter string and pending status
    list(
      string = filter_string,
      pending_advanced = has_pending_advanced
    )
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
        align-items: center;
        border: 1px solid var(--bs-border-color);
        padding: 10px;
        padding-right: 35px; /* Make room for the X button */
        margin-bottom: 3px;
        position: relative;
      }

      .enhanced-filter-condition .mode-toggle {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 00px;
      }

      .enhanced-filter-condition .mode-controls {
        display: flex;
        align-items: center;
      }

      .enhanced-filter-condition .mode-label {
        font-weight: 500;
      }

      .enhanced-filter-condition .shiny-ace {
        border: none;
        margin: 7px;
        margin-bottom: 7.5px;
      }

      .enhanced-filter-condition .condition-code {
        flex: 1;
      }

      .enhanced-filter-condition .btn-close {
        opacity: 0.5;
      }

      .enhanced-filter-condition .btn-close:hover {
        opacity: 0.8;
      }

      /* Apply button styling - Variant 2: Outlined Arrow */
      .apply-btn {
        height: 36px;
        padding: 4px 12px;
        display: inline-flex;
        align-items: center;
        gap: 0px;
        font-size: 14px;
        border-color: #0284c7;
        color: #0284c7;
        background: transparent;
        transition: all 0.15s ease-in-out;
      }

      .apply-btn:hover:not(:disabled) {
        background-color: #f0f9ff;
        border-color: #0284c7;
        color: #0284c7;
      }

      .apply-btn:disabled {
        opacity: 0.5;
        cursor: not-allowed;
        border-color: #94a3b8;
        color: #94a3b8;
      }

      .apply-btn .apply-arrow {
        font-size: 16px;
        line-height: 1;
      }

      /* When there are pending changes */
      .apply-btn.has-changes {
        border-color: #0284c7;
        color: #0284c7;
        font-weight: 500;
      }

      .apply-btn.has-changes:hover {
        background-color: #e0f2fe;
        border-color: #0369a1;
        color: #0369a1;
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
          class = "btn btn-outline-primary btn-sm"
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
#' @param available_columns Available column names
#' @param data Optional data frame for parsing expressions
#' @return A div containing the row UI
enhanced_filter_condition_ui <- function(
  id,
  value = "TRUE",
  mode = "simple",
  show_remove = TRUE,
  available_columns = character(),
  data = NULL
) {
  # Parse the expression to get initial values for simple mode
  parsed <- NULL
  selected_column <- NULL
  range_values <- c(0, 100)
  selected_values <- NULL
  include_mode <- "include"

  if (value != "TRUE" && value != "") {
    # Always parse the expression to get column, even without data
    parsed <- parse_simple(value, available_columns, data)
    if (!is.null(parsed)) {
      selected_column <- parsed$column
      if (!is.null(parsed$range)) {
        # For numeric columns, use the parsed range
        range_values <- parsed$range
      } else if (!is.null(parsed$values)) {
        # For character columns, use the parsed values
        selected_values <- parsed$values
        include_mode <- if (parsed$include) "include" else "exclude"
      }
    }
  }

  # If we have a selected column, determine its type and set appropriate ranges
  col_min <- 0
  col_max <- 100
  col_choices <- character(0)

  if (
    !is.null(selected_column) &&
      !is.null(data) &&
      selected_column %in% colnames(data)
  ) {
    col_data <- data[[selected_column]]
    if (is.numeric(col_data)) {
      col_range <- range(col_data, na.rm = TRUE)
      col_min <- col_range[1]
      col_max <- col_range[2]
      # If no parsed range, use full column range
      if (is.null(parsed) || is.null(parsed$range)) {
        range_values <- col_range
      }
    } else {
      # Character/factor column
      col_choices <- unique(as.character(col_data))
      col_choices <- col_choices[!is.na(col_choices)]
    }
  }

  div(
    id = paste0(id, "_panel"),
    class = "enhanced-filter-condition position-relative",
    # X button positioned absolutely on the right
    if (show_remove) {
      tags$button(
        id = paste0(id, "_remove"),
        type = "button",
        class = "btn-close ms-3",
        `aria-label` = "Remove condition",
        onclick = paste0(
          "Shiny.setInputValue('",
          id,
          "_remove', Math.random())"
        )
      )
    },
    # Mode toggle at the top
    div(
      class = "mode-toggle",
      # Left side: Label
      # span("Mode:", class = "mode-label"),
      # Right side: Toggle switch and remove button
      div(
        class = "mode-controls",
        # Toggle switch (checked = advanced, unchecked = simple)
        div(
          class = "form-check form-switch d-inline-block",
          tags$input(
            class = "form-check-input",
            type = "checkbox",
            id = paste0(id, "_mode"),
            role = "switch",
            checked = if (mode == "advanced") "checked" else NULL,
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "left",
            title = "Toggle between simple mode (off) and advanced mode (on)"
          ),
          tags$label(
            class = "form-check-label ms-2",
            `for` = paste0(id, "_mode"),
            id = paste0(id, "_mode_label"),
            if (mode == "advanced") "Advanced" else "Simple"
          )
        )
      )
    ),
    # Simple vs advanced mode panels
    div(
      # Simple mode UI (initially hidden if mode is advanced)
      div(
        id = paste0(id, "_simple_panel"),
        style = if (mode != "simple") "display: none;" else "",
        class = "condition-simple",
        div(
          class = "row mb-2",
          div(
            class = "col-md-6",
            selectInput(
              paste0(id, "_column"),
              label = tags$small("Column", class = "text-muted"),
              choices = available_columns,
              selected = selected_column,
              width = "100%"
            )
          ),
          div(
            class = "col-md-6",
            # Static UI elements that are shown/hidden based on column type
            # Numeric column: range slider (hidden by default)
            div(
              id = paste0(id, "_numeric_ui"),
              style = if (
                !is.null(selected_column) &&
                  !is.null(data) &&
                  selected_column %in% colnames(data) &&
                  is.numeric(data[[selected_column]])
              ) {
                ""
              } else {
                "display: none;"
              },
              sliderInput(
                paste0(id, "_range"),
                label = tags$small("Value Range", class = "text-muted"),
                min = col_min,
                max = col_max,
                value = range_values,
                width = "100%"
              )
            ),
            # Character/factor column: multi-select (hidden by default)
            div(
              id = paste0(id, "_character_ui"),
              style = if (
                !is.null(selected_column) &&
                  !is.null(data) &&
                  selected_column %in% colnames(data) &&
                  !is.numeric(data[[selected_column]])
              ) {
                ""
              } else {
                "display: none;"
              },
              selectInput(
                paste0(id, "_values"),
                label = tags$small("Values", class = "text-muted"),
                choices = col_choices,
                selected = selected_values,
                multiple = TRUE
              ),
              radioButtons(
                paste0(id, "_include"),
                label = NULL,
                choices = c("Include" = "include", "Exclude" = "exclude"),
                selected = include_mode,
                inline = TRUE
              )
            )
          )
        )
      ),

      # Advanced mode UI (ACE editor with apply button)
      div(
        id = paste0(id, "_advanced_panel"),
        style = if (mode == "simple") "display: none;" else "",
        tags$label(
          class = "form-check-label pb-2",
          tags$small("R Expression", class = "text-muted")
        ),
        class = "condition-code",
        div(
          class = "input-group d-flex align-items-start gap-2",
          div(
            style = "flex: 1;",
            setup_ace_editor(id, value = value)
          ),
          actionButton(
            paste0(id, "_apply"),
            label = "Apply",
            icon = icon("arrow-right"),
            class = "btn btn-outline-success btn-sm apply-btn",
            title = "Apply expression"
          )
        )
      )
    )
  )
}
