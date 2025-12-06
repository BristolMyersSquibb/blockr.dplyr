#' @importFrom blockr.core register_blocks
register_dplyr_blocks <- function() {
  register_blocks(
    c(
      "new_select_block",
      "new_join_block",
      "new_arrange_block",
      "new_mutate_expr_block",
      "new_summarize_block",
      "new_summarize_expr_block",
      "new_filter_block",
      "new_filter_expr_block",
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
      "Select Columns",
      "Lookup & Merge",
      "Sort Rows",
      "Calculate Columns",
      "Aggregate Data",
      "Aggregate (Advanced)",
      "Filter Rows",
      "Filter (Advanced)",
      "Stack Tables",
      "Combine Side-by-Side",
      "Rename Columns",
      "Pick Rows",
      "Unpivot (Wide to Long)",
      "Pivot (Long to Wide)",
      "Combine Columns",
      "Split Column"
    ),
    description = c(
      "Choose which columns to keep or remove. Can also remove duplicate rows. (dplyr: select, distinct)",
      "Combine two tables by matching values in a shared column. (dplyr: left_join, inner_join, etc.)",
      "Sort your data by one or more columns, ascending or descending. (dplyr: arrange)",
      "Create new columns or modify existing ones using R formulas. (dplyr: mutate)",
      "Calculate totals, averages, counts, and other statistics. No coding required. (dplyr: summarize)",
      "Create summary statistics using R expressions. Supports grouping. (dplyr: summarize)",
      "Keep only rows that match selected values. No coding required. (dplyr: filter)",
      "Filter rows using R expressions with AND/OR logic. (dplyr: filter)",
      "Stack tables vertically by matching column names. (dplyr: bind_rows)",
      "Place tables next to each other horizontally. Both tables must have the same number of rows. (dplyr: bind_cols)",
      "Change column names to make them more readable. (dplyr: rename)",
      "Select specific rows: first N, last N, random sample, or rows with min/max values. (dplyr: slice)",
      "Convert columns into rows. Useful when column headers contain data values. (tidyr: pivot_longer)",
      "Convert rows into columns. Turn categories into separate columns. (tidyr: pivot_wider)",
      "Merge multiple columns into one by pasting values together. (tidyr: unite)",
      "Split one column into multiple columns using a delimiter. (tidyr: separate)"
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
      "pencil-square", # mutate_expr block
      "sliders", # summarize block (simple)
      "calculator", # summarize_expr block
      "filter", # filter block (simple)
      "code-slash", # filter_expr block
      "arrows-collapse", # bind_rows block (vertical arrows = rows stacking)
      "arrows-collapse-vertical", # bind_cols block (horizontal arrows = columns joining)
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
