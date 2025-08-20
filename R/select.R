#' Select block constructor
#'
#' This block allows to perform column subsetting on `data.frame` objects (see
#' [dplyr::select()]).
#'
#' @param columns Selected columns
#' @param enhanced Use enhanced multi-select interface (default TRUE)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_select_block <- function(columns = character(), enhanced = TRUE, ...) {

  if (enhanced) {
    # Use enhanced multi-select interface
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            cols <- reactive(colnames(data()))

            # Use the multi select module
            r_selected <- mod_multi_select_server(
              "multi_select",
              get_value = function() columns,  # Return initial columns as-is
              get_cols = cols,
              get_data_preview = data
            )

            list(
              expr = reactive({
                selected_cols <- r_selected()

                if (length(selected_cols) == 0) {
                  # No columns selected - select none (this will result in empty data frame)
                  parse(text = "dplyr::select(data, -dplyr::everything())")[[1]]
                } else {
                  # Build select expression
                  bquote(
                    dplyr::select(data, ..(cols)),
                    list(cols = lapply(selected_cols, as.name)),
                    splice = TRUE
                  )
                }
              }),
              state = list(
                columns = r_selected,
                enhanced = reactive(TRUE)
              )
            )
          }
        )
      },
      function(id) {
        mod_multi_select_ui(NS(id, "multi_select"))
      },
      class = "select_block",
      ...
    )
  } else {
    # Use classic simple interface for backward compatibility
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {

            sels <- reactiveVal(columns)
            cols <- reactive(colnames(data()))

            observeEvent(
              input$columns,
              sels(intersect(input$columns, cols()))
            )

            observe(
              {
                updateSelectInput(
                  session,
                  inputId = "columns",
                  choices = cols(),
                  selected = sels()
                )
              }
            )

            list(
              expr = reactive(
                bquote(
                  dplyr::select(data, ..(cols)),
                  list(cols = lapply(sels(), as.name)),
                  splice = TRUE
                )
              ),
              state = list(
                columns = sels,
                enhanced = reactive(FALSE)
              )
            )
          }
        )
      },
      function(id) {
        selectInput(
          inputId = NS(id, "columns"),
          label = "Columns",
          choices = list(),
          multiple = TRUE
        )
      },
      class = "select_block",
      ...
    )
  }
}
