#' Select block constructor
#'
#' This block allows to perform column subsetting on `data.frame` objects (see
#' [dplyr::select()]).
#'
#' @param columns Selected columns
#' @param interface Interface type: "table" or "cards" (default "table")
#' @param show_selected_on_top Whether to show selected columns on top (default TRUE for table interface)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_select_block <- function(columns = character(), interface = "table", show_selected_on_top = TRUE, ...) {

  # Determine interface type
  interface <- match.arg(interface, c("table", "cards"))

  if (interface == "table") {
    # Use new table-based interface
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            cols <- reactive(colnames(data()))

            # Use the table select module
            r_selected <- mod_table_select_server(
              "table_select",
              get_value = function() columns,
              get_cols = cols,
              get_data_preview = data,
              show_selected_on_top = show_selected_on_top
            )

            list(
              expr = reactive({
                selected_cols <- r_selected()

                if (length(selected_cols) == 0) {
                  # No columns selected - select none
                  parse(text = "dplyr::select(data, -dplyr::everything())")[[1]]
                } else {
                  # Build select expression with column names
                  cols_str <- paste(sprintf("`%s`", selected_cols), collapse = ", ")
                  parse(text = glue::glue("dplyr::select(data, {cols_str})"))[[1]]
                }
              }),
              state = list(
                columns = r_selected,
                interface = reactive("table"),
                show_selected_on_top = reactive(show_selected_on_top)
              )
            )
          }
        )
      },
      function(id) {
        mod_table_select_ui(NS(id, "table_select"), show_selected_on_top = show_selected_on_top)
      },
      class = "select_block",
      ...
    )
  } else {
    # Use enhanced multi-select interface (cards)
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            cols <- reactive(colnames(data()))

            # Use the multi select module
            r_selected <- mod_multi_select_server(
              "multi_select",
              get_value = function() columns,
              get_cols = cols,
              get_data_preview = data
            )

            list(
              expr = reactive({
                selected_cols <- r_selected()

                if (length(selected_cols) == 0) {
                  # No columns selected - select none
                  parse(text = "dplyr::select(data, -dplyr::everything())")[[1]]
                } else {
                  # Build select expression with column names
                  cols_str <- paste(sprintf("`%s`", selected_cols), collapse = ", ")
                  parse(text = glue::glue("dplyr::select(data, {cols_str})"))[[1]]
                }
              }),
              state = list(
                columns = r_selected,
                interface = reactive("cards")
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
  }
}
