#' @importFrom blockr.core register_blocks
register_dplyr_blocks <- function() {
  register_blocks(
    c(
      "new_select_block",
      "new_join_block",
      "new_arrange_block",
      "new_mutate_block",
      "new_summarize_block",
      "new_filter_block",
      "new_distinct_block",
      "new_bind_rows_block",
      "new_bind_cols_block",
      "new_rename_block",
      "new_slice_block"
    ),
    name = c(
      "select block",
      "join block",
      "arrange block",
      "mutate block",
      "summarize block",
      "filter block",
      "distinct block",
      "bind rows block",
      "bind columns block",
      "rename block",
      "slice block"
    ),
    description = c(
      "Subset columns in a data.frame",
      "Join together two data.frames",
      "Order to the rows of a data.frame",
      "Add or modify columns in a data.frame",
      "Summarize row groups in a data.frame",
      "Filter rows in a data.frame based on conditions",
      "Remove duplicate rows from a data.frame",
      "Stack data.frames vertically by matching column names",
      "Combine data.frames side-by-side (requires same row count)",
      "Rename columns in a data.frame",
      "Select rows by position, value, or sampling with optional grouping"
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
