register_dplyr_blocks <- function() {
  register_blocks(
    c(
      "new_select_block",
      "new_join_block",
      "new_arrange_block",
      "new_mutate_block",
      "new_summarize_block",
      "new_filter_block"
    ),
    name = c(
      "select block",
      "join block",
      "arrange block",
      "mutate block",
      "summarize block",
      "filter block"
    ),
    description = c(
      "Subset columns in a data.frame",
      "Join together two data.frames",
      "Order to the rows of a data.frame",
      "Add or modify columns in a data.frame",
      "Summarize row groups in a data.frame",
      "Filter rows in a data.frame based on conditions"
    ),
    category = c(
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
