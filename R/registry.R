#' @importFrom blockr.core register_blocks
register_dplyr_blocks <- function() {
  register_blocks(
    c(
      "new_select_block",
      "new_join_block",
      "new_arrange_block",
      "new_mutate_block",
      "new_summarize_block",
      "new_summarize_nocode_block",
      "new_filter_expr_block",
      "new_value_filter_block",
      "new_bind_rows_block",
      "new_bind_cols_block",
      "new_rename_block",
      "new_slice_block",
      "new_pivot_longer_block",
      "new_pivot_wider_block",
      "new_unite_block",
      "new_separate_block"
    ),
    name = c(
      "select block",
      "join block",
      "arrange block",
      "mutate block",
      "summarize block",
      "summarize no-code block",
      "expression filter block",
      "value filter block",
      "bind rows block",
      "bind columns block",
      "rename block",
      "slice block",
      "pivot longer block",
      "pivot wider block",
      "unite block",
      "separate block"
    ),
    description = c(
      "Subset columns in a data.frame (supports distinct rows)",
      "Join together two data.frames",
      "Order to the rows of a data.frame",
      "Add or modify columns in a data.frame",
      "Summarize row groups in a data.frame",
      "Summarize data using dropdown selections (no coding required)",
      "Filter rows using R expressions with AND/OR logic",
      "Filter rows by selecting values from columns",
      "Stack data.frames vertically by matching column names",
      "Combine data.frames side-by-side (requires same row count)",
      "Rename columns in a data.frame",
      "Select rows by position, value, or sampling with optional grouping",
      "Reshape data from wide to long format",
      "Reshape data from long to wide format",
      "Combine multiple columns into one by pasting values together",
      "Split one column into multiple columns using a separator"
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
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "layout-sidebar-inset-reverse", # select block
      "bezier2", # join block
      "sort-down-alt", # arrange block
      "pencil-square", # mutate block
      "calculator", # summarize block
      "sliders", # summarize_nocode block
      "code-slash", # filter_expr block
      "filter", # value_filter block
      "arrows-collapse-vertical", # bind_rows block
      "arrows-collapse", # bind_cols block
      "tags", # rename block
      "scissors", # slice block
      "arrow-down-up", # pivot_longer block
      "arrow-left-right", # pivot_wider block
      "link", # unite block
      "distribute-horizontal" # separate block
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
