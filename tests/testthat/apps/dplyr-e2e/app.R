library(blockr.core)
library(blockr.dplyr)

# Clean dataset for pivot_wider testing (no duplicates per id+name combo)
pivot_data <- data.frame(
  person = c("A", "A", "B", "B", "C", "C"),
  measure = c("height", "weight", "height", "weight", "height", "weight"),
  value = c(170, 70, 180, 85, 165, 60),
  unit = c("cm", "kg", "cm", "kg", "cm", "kg")
)

single_col_data <- data.frame(x = 1:5)
single_val_data <- data.frame(id = rep("ONLY", 3), val = 1:3)

serve(
  new_board(
    blocks = c(
      data_mtcars = new_dataset_block("mtcars"),
      data_mtcars2 = new_dataset_block("mtcars"),
      data_iris = new_dataset_block("iris"),
      data_pivot = new_static_block(data = pivot_data),
      data_single = new_static_block(data = single_col_data),
      data_one_val = new_static_block(data = single_val_data),
      filter = new_filter_block(),
      select = new_select_block(),
      mutate = new_mutate_block(),
      summarize = new_summarize_block(),
      arrange = new_arrange_block(),
      rename = new_rename_block(),
      slice = new_slice_block(),
      pivot_longer = new_pivot_longer_block(),
      pivot_wider = new_pivot_wider_block(),
      unite = new_unite_block(),
      separate = new_separate_block(),
      join = new_join_block(),
      bind_rows = new_bind_rows_block(),
      bind_cols = new_bind_cols_block(),
      select_single = new_select_block(),
      filter_single = new_filter_block()
    ),
    links = c(
      new_link("data_mtcars", "filter", "data"),
      new_link("data_mtcars", "select", "data"),
      new_link("data_mtcars", "mutate", "data"),
      new_link("data_mtcars", "summarize", "data"),
      new_link("data_mtcars", "arrange", "data"),
      new_link("data_mtcars", "rename", "data"),
      new_link("data_mtcars", "slice", "data"),
      new_link("data_mtcars", "unite", "data"),
      new_link("data_iris", "pivot_longer", "data"),
      new_link("data_pivot", "pivot_wider", "data"),
      new_link("data_iris", "separate", "data"),
      new_link("data_mtcars", "join", "x"),
      new_link("data_iris", "join", "y"),
      new_link("data_mtcars", "bind_rows", "x"),
      new_link("data_iris", "bind_rows", "y"),
      new_link("data_mtcars", "bind_cols", "x"),
      new_link("data_mtcars2", "bind_cols", "y"),
      new_link("data_single", "select_single", "data"),
      new_link("data_one_val", "filter_single", "data")
    )
  ),
  id = "board"
)
