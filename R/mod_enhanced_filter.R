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
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div radioButtons selectInput updateSelectInput sliderInput updateRadioButtons
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs useShinyjs runjs show hide delay
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
      modes <- r_condition_modes()

      for (i in indices) {
        mode <- modes[[as.character(i)]] %||% "advanced"

        if (mode == "simple") {
          # Build expression from simple UI inputs
          column_id <- paste0("condition_", i, "_column")
          selected_col <- input[[column_id]]

          if (!is.null(selected_col) && selected_col != "") {
            # Check if data is available to determine column type
            if (!is.null(get_data)) {
              data <- get_data()
              if (is.data.frame(data) && selected_col %in% colnames(data)) {
                col_data <- data[[selected_col]]

                if (is.numeric(col_data)) {
                  # Numeric column - use range slider values
                  range_id <- paste0("condition_", i, "_range")
                  range_val <- input[[range_id]]
                  col_range <- range(col_data, na.rm = TRUE)

                  if (!is.null(range_val) && length(range_val) == 2) {
                    # Check if values are at the extremes
                    at_min <- abs(range_val[1] - col_range[1]) < 0.001
                    at_max <- abs(range_val[2] - col_range[2]) < 0.001

                    if (range_val[1] == range_val[2]) {
                      # Single value
                      expr <- paste0(selected_col, " == ", range_val[1])
                    } else if (at_min && at_max) {
                      # Full range - no filter needed
                      expr <- "TRUE"
                    } else if (at_min) {
                      # Only upper bound
                      expr <- paste0(selected_col, " <= ", range_val[2])
                    } else if (at_max) {
                      # Only lower bound
                      expr <- paste0(selected_col, " >= ", range_val[1])
                    } else {
                      # Both bounds
                      expr <- paste0(
                        selected_col,
                        " >= ",
                        range_val[1],
                        " & ",
                        selected_col,
                        " <= ",
                        range_val[2]
                      )
                    }
                    result <- append(result, expr)
                  }
                } else {
                  # Character/factor column - use multi-select values
                  values_id <- paste0("condition_", i, "_values")
                  include_id <- paste0("condition_", i, "_include")

                  selected_vals <- input[[values_id]]
                  include_mode <- input[[include_id]] %||% "include"

                  if (!is.null(selected_vals) && length(selected_vals) > 0) {
                    # Quote character values
                    quoted_vals <- paste0(
                      '"',
                      selected_vals,
                      '"',
                      collapse = ", "
                    )

                    if (include_mode == "include") {
                      if (length(selected_vals) == 1) {
                        expr <- paste0(
                          selected_col,
                          ' == "',
                          selected_vals,
                          '"'
                        )
                      } else {
                        expr <- paste0(
                          selected_col,
                          " %in% c(",
                          quoted_vals,
                          ")"
                        )
                      }
                    } else {
                      # Exclude mode
                      if (length(selected_vals) == 1) {
                        expr <- paste0(
                          selected_col,
                          ' != "',
                          selected_vals,
                          '"'
                        )
                      } else {
                        expr <- paste0(
                          "!",
                          selected_col,
                          " %in% c(",
                          quoted_vals,
                          ")"
                        )
                      }
                    }
                    result <- append(result, expr)
                  }
                }
              }
            }
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

    # Function to parse expression and extract column/values for simple mode
    parse_expression_for_simple <- function(expr_str, i) {
      if (is.null(expr_str) || expr_str == "" || expr_str == "TRUE") {
        return(NULL)
      }

      # Try to extract column name and values from common patterns
      cols <- r_cols()

      # Find which column is referenced
      col_found <- NULL
      for (col in cols) {
        if (grepl(paste0("\\b", col, "\\b"), expr_str)) {
          col_found <- col
          break
        }
      }

      if (!is.null(col_found) && !is.null(get_data)) {
        data <- get_data()
        if (is.data.frame(data) && col_found %in% colnames(data)) {
          col_data <- data[[col_found]]

          if (is.numeric(col_data)) {
            # Get the data range for this column
            col_range <- range(col_data, na.rm = TRUE)

            # Try to parse numeric patterns
            # Pattern: col >= X & col <= Y
            range_pattern <- paste0(
              "\\b",
              col_found,
              "\\s*>=\\s*([0-9.-]+)\\s*&\\s*",
              col_found,
              "\\s*<=\\s*([0-9.-]+)"
            )
            if (grepl(range_pattern, expr_str)) {
              matches <- regmatches(
                expr_str,
                regexec(range_pattern, expr_str)
              )[[1]]
              if (length(matches) == 3) {
                return(list(
                  column = col_found,
                  range = as.numeric(c(matches[2], matches[3]))
                ))
              }
            }

            # Pattern: col == X
            eq_pattern <- paste0("\\b", col_found, "\\s*==\\s*([0-9.-]+)")
            if (grepl(eq_pattern, expr_str)) {
              matches <- regmatches(expr_str, regexec(eq_pattern, expr_str))[[
                1
              ]]
              if (length(matches) == 2) {
                val <- as.numeric(matches[2])
                return(list(
                  column = col_found,
                  range = c(val, val)
                ))
              }
            }

            # Pattern: col > X (set range from X to max)
            gt_pattern <- paste0("\\b", col_found, "\\s*>\\s*([0-9.-]+)")
            if (grepl(gt_pattern, expr_str)) {
              matches <- regmatches(expr_str, regexec(gt_pattern, expr_str))[[
                1
              ]]
              if (length(matches) == 2) {
                val <- as.numeric(matches[2])
                # Set range slightly above the value to max
                return(list(
                  column = col_found,
                  range = c(val + 0.01, col_range[2])
                ))
              }
            }

            # Pattern: col >= X (set range from X to max)
            gte_pattern <- paste0("\\b", col_found, "\\s*>=\\s*([0-9.-]+)")
            if (grepl(gte_pattern, expr_str)) {
              matches <- regmatches(expr_str, regexec(gte_pattern, expr_str))[[
                1
              ]]
              if (length(matches) == 2) {
                val <- as.numeric(matches[2])
                return(list(
                  column = col_found,
                  range = c(val, col_range[2])
                ))
              }
            }

            # Pattern: col < X (set range from min to X)
            lt_pattern <- paste0("\\b", col_found, "\\s*<\\s*([0-9.-]+)")
            if (grepl(lt_pattern, expr_str)) {
              matches <- regmatches(expr_str, regexec(lt_pattern, expr_str))[[
                1
              ]]
              if (length(matches) == 2) {
                val <- as.numeric(matches[2])
                # Set range from min to slightly below the value
                return(list(
                  column = col_found,
                  range = c(col_range[1], val - 0.01)
                ))
              }
            }

            # Pattern: col <= X (set range from min to X)
            lte_pattern <- paste0("\\b", col_found, "\\s*<=\\s*([0-9.-]+)")
            if (grepl(lte_pattern, expr_str)) {
              matches <- regmatches(expr_str, regexec(lte_pattern, expr_str))[[
                1
              ]]
              if (length(matches) == 2) {
                val <- as.numeric(matches[2])
                return(list(
                  column = col_found,
                  range = c(col_range[1], val)
                ))
              }
            }
          } else {
            # Try to parse character patterns
            # Pattern: col %in% c("val1", "val2")
            in_pattern <- paste0("\\b", col_found, "\\s*%in%\\s*c\\(([^)]+)\\)")
            if (grepl(in_pattern, expr_str)) {
              matches <- regmatches(expr_str, regexec(in_pattern, expr_str))[[
                1
              ]]
              if (length(matches) == 2) {
                # Extract quoted values
                val_str <- matches[2]
                values <- regmatches(val_str, gregexpr('"[^"]*"', val_str))[[1]]
                values <- gsub('"', '', values)
                return(list(
                  column = col_found,
                  values = values,
                  include = TRUE
                ))
              }
            }

            # Pattern: !col %in% c("val1", "val2")
            not_in_pattern <- paste0(
              "!\\s*",
              col_found,
              "\\s*%in%\\s*c\\(([^)]+)\\)"
            )
            if (grepl(not_in_pattern, expr_str)) {
              matches <- regmatches(
                expr_str,
                regexec(not_in_pattern, expr_str)
              )[[1]]
              if (length(matches) == 2) {
                val_str <- matches[2]
                values <- regmatches(val_str, gregexpr('"[^"]*"', val_str))[[1]]
                values <- gsub('"', '', values)
                return(list(
                  column = col_found,
                  values = values,
                  include = FALSE
                ))
              }
            }

            # Pattern: col == "val"
            eq_str_pattern <- paste0("\\b", col_found, '\\s*==\\s*"([^"]*)"')
            if (grepl(eq_str_pattern, expr_str)) {
              matches <- regmatches(
                expr_str,
                regexec(eq_str_pattern, expr_str)
              )[[1]]
              if (length(matches) == 2) {
                return(list(
                  column = col_found,
                  values = matches[2],
                  include = TRUE
                ))
              }
            }

            # Pattern: col != "val"
            neq_str_pattern <- paste0("\\b", col_found, '\\s*!=\\s*"([^"]*)"')
            if (grepl(neq_str_pattern, expr_str)) {
              matches <- regmatches(
                expr_str,
                regexec(neq_str_pattern, expr_str)
              )[[1]]
              if (length(matches) == 2) {
                return(list(
                  column = col_found,
                  values = matches[2],
                  include = FALSE
                ))
              }
            }
          }
        }
      }

      NULL
    }

    # Track mode changes and update conditionalPanel
    observe({
      indices <- r_condition_indices()
      modes <- r_condition_modes()

      for (i in indices) {
        mode_id <- paste0("condition_", i, "_mode")
        current_mode <- input[[mode_id]]

        if (!is.null(current_mode)) {
          old_mode <- modes[[as.character(i)]]
          modes[[as.character(i)]] <- current_mode

          # Use shinyjs to toggle visibility based on mode
          if (current_mode == "simple") {
            shinyjs::show(paste0("condition_", i, "_simple_panel"))
            shinyjs::hide(paste0("condition_", i, "_advanced_panel"))

            # When switching to simple mode, try to parse the expression
            if (old_mode != "simple") {
              expr_str <- input[[paste0("condition_", i)]]
              parsed <- parse_expression_for_simple(expr_str, i)

              if (!is.null(parsed)) {
                # Update column selection
                updateSelectInput(
                  session,
                  paste0("condition_", i, "_column"),
                  selected = parsed$column
                )

                # Delay updating the dependent inputs to allow column change to process
                shinyjs::delay(100, {
                  if (!is.null(parsed$range)) {
                    # Update range slider
                    updateSliderInput(
                      session,
                      paste0("condition_", i, "_range"),
                      value = parsed$range
                    )
                  } else if (!is.null(parsed$values)) {
                    # Update multi-select and include/exclude
                    updateSelectInput(
                      session,
                      paste0("condition_", i, "_values"),
                      selected = parsed$values
                    )
                    updateRadioButtons(
                      session,
                      paste0("condition_", i, "_include"),
                      selected = if (parsed$include) "include" else "exclude"
                    )
                  }
                })
              }
            }
          } else {
            # Switching to advanced mode
            shinyjs::hide(paste0("condition_", i, "_simple_panel"))
            shinyjs::show(paste0("condition_", i, "_advanced_panel"))

            # When switching from simple to advanced, ensure the ACE editor shows the built expression
            if (old_mode == "simple") {
              expr <- build_simple_expression(i)
              # Use a small delay to ensure the ACE editor is visible before updating
              shinyjs::delay(50, {
                updateAceEditor(session, paste0("condition_", i), value = expr)
              })
            }
          }
        }
      }

      r_condition_modes(modes)
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

    # Render simple UI for each condition based on column type
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

          output[[paste0("condition_", idx, "_simple_ui")]] <- renderUI({
            column_id <- paste0("condition_", idx, "_column")
            selected_col <- input[[column_id]]

            if (is.null(selected_col) || !selected_col %in% colnames(data)) {
              return(div("Select a column"))
            }

            col_data <- data[[selected_col]]

            if (is.numeric(col_data)) {
              # Numeric column: range slider
              col_range <- range(col_data, na.rm = TRUE)
              sliderInput(
                ns(paste0("condition_", idx, "_range")),
                label = "Range",
                min = col_range[1],
                max = col_range[2],
                value = col_range,
                step = if (all(col_data == floor(col_data), na.rm = TRUE)) {
                  1
                } else {
                  0.01
                }
              )
            } else {
              # Character/factor column: multi-select
              unique_vals <- unique(as.character(col_data))
              unique_vals <- unique_vals[!is.na(unique_vals)]

              tagList(
                selectInput(
                  ns(paste0("condition_", idx, "_values")),
                  label = "Values",
                  choices = unique_vals,
                  selected = unique_vals[1],
                  multiple = TRUE
                ),
                radioButtons(
                  ns(paste0("condition_", idx, "_include")),
                  label = NULL,
                  choices = c("Include" = "include", "Exclude" = "exclude"),
                  selected = "include",
                  inline = TRUE
                )
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

      if (!is.null(get_data)) {
        data <- get_data()
        if (is.data.frame(data) && selected_col %in% colnames(data)) {
          col_data <- data[[selected_col]]

          if (is.numeric(col_data)) {
            # Numeric column - use range slider values
            range_id <- paste0("condition_", i, "_range")
            range_val <- input[[range_id]]
            col_range <- range(col_data, na.rm = TRUE)

            if (!is.null(range_val) && length(range_val) == 2) {
              # Check if values are at the extremes
              at_min <- abs(range_val[1] - col_range[1]) < 0.001
              at_max <- abs(range_val[2] - col_range[2]) < 0.001

              if (range_val[1] == range_val[2]) {
                # Single value
                return(paste0(selected_col, " == ", range_val[1]))
              } else if (at_min && at_max) {
                # Full range - no filter needed
                return("TRUE")
              } else if (at_min) {
                # Only upper bound
                return(paste0(selected_col, " <= ", range_val[2]))
              } else if (at_max) {
                # Only lower bound
                return(paste0(selected_col, " >= ", range_val[1]))
              } else {
                # Both bounds
                return(paste0(
                  selected_col,
                  " >= ",
                  range_val[1],
                  " & ",
                  selected_col,
                  " <= ",
                  range_val[2]
                ))
              }
            }
          } else {
            # Character/factor column - use multi-select values
            values_id <- paste0("condition_", i, "_values")
            include_id <- paste0("condition_", i, "_include")

            selected_vals <- input[[values_id]]
            include_mode <- input[[include_id]] %||% "include"

            if (!is.null(selected_vals) && length(selected_vals) > 0) {
              # Quote character values
              quoted_vals <- paste0('"', selected_vals, '"', collapse = ", ")

              if (include_mode == "include") {
                if (length(selected_vals) == 1) {
                  return(paste0(selected_col, ' == "', selected_vals, '"'))
                } else {
                  return(paste0(selected_col, " %in% c(", quoted_vals, ")"))
                }
              } else {
                # Exclude mode
                if (length(selected_vals) == 1) {
                  return(paste0(selected_col, ' != "', selected_vals, '"'))
                } else {
                  return(paste0(
                    "!",
                    selected_col,
                    " %in% c(",
                    quoted_vals,
                    ")"
                  ))
                }
              }
            }
          }
        }
      }

      "TRUE"
    }

    # Update ACE editor when simple UI inputs change
    observe({
      indices <- r_condition_indices()
      modes <- r_condition_modes()

      for (i in indices) {
        mode <- modes[[as.character(i)]] %||% "advanced"

        if (mode == "simple") {
          # Watch all simple UI inputs for this condition
          local({
            idx <- i

            # Watch column selection
            observeEvent(
              input[[paste0("condition_", idx, "_column")]],
              {
                if (modes[[as.character(idx)]] == "simple") {
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

            # Watch range slider for numeric columns
            observeEvent(
              input[[paste0("condition_", idx, "_range")]],
              {
                if (modes[[as.character(idx)]] == "simple") {
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

            # Watch values selection for character columns
            observeEvent(
              input[[paste0("condition_", idx, "_values")]],
              {
                if (modes[[as.character(idx)]] == "simple") {
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

            # Watch include/exclude radio buttons
            observeEvent(
              input[[paste0("condition_", idx, "_include")]],
              {
                if (modes[[as.character(idx)]] == "simple") {
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
        }
      }
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
      cols <- r_cols()

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
              show_remove = (length(indices) > 1),
              available_columns = cols
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

    # Initialize ACE editors when new conditions are added
    observeEvent(r_condition_indices(), {
      indices <- r_condition_indices()
      cols <- r_cols()

      for (i in indices) {
        condition_id <- paste0("condition_", i)
        if (!condition_id %in% names(input)) {
          # New editor, initialize it
          initialize_ace_editor(session, ns(condition_id), cols)
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
#' @param available_columns Character vector of column names available for filtering
#' @return A div containing the row UI
enhanced_filter_condition_ui <- function(
  id,
  value = "TRUE",
  mode = "advanced",
  show_remove = TRUE,
  available_columns = character()
) {
  div(
    class = "enhanced-filter-condition",

    # Mode toggle at the top
    div(
      class = "mode-toggle",
      radioButtons(
        paste0(id, "_mode"),
        label = "Mode",
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

    # Simple vs advanced mode panels
    tagList(
      # Simple mode UI (initially hidden if mode is advanced)
      div(
        id = paste0(id, "_simple_panel"),
        style = if (mode != "simple") "display: none;" else "",
        class = "condition-simple",
        div(
          class = "row mb-2",
          div(
            class = "col-md-4",
            selectInput(
              paste0(id, "_column"),
              label = "Column",
              choices = c("Select column..." = "", available_columns),
              selected = NULL,
              width = "100%"
            )
          ),
          div(
            class = "col-md-8",
            # Placeholder for operation-specific UI (Phase 3)
            uiOutput(paste0(id, "_simple_ui"))
          )
        )
      ),

      # Advanced mode UI (ACE editor)
      div(
        id = paste0(id, "_advanced_panel"),
        style = if (mode == "simple") "display: none;" else "",
        class = "condition-code",
        setup_ace_editor(id, value = value)
      )
    )
  )
}
