pkgload::load_all("blockr.dplyr")
library(blockr.dock)
library(blockr.dag)
library(blockr.extra)

options(
  blockr.html_table_preview = TRUE,
  "g6R.mode" = "dev",
  "g6R.preserve_elements_position" = TRUE
)

serve(
  new_dock_board(
    blocks = c(
      data1 = new_dataset_block("mtcars"),
      data2 = new_dataset_block("iris"),
      data3 = new_dataset_block("mtcars"),

      # Column-picker blocks
      select = new_select_block(),
      arrange = new_arrange_block(),
      rename = new_rename_block(),

      # Expression blocks
      filter = new_filter_block(),
      mutate = new_mutate_block(),
      summarize = new_summarize_block(),

      # Join (binary — needs two inputs)
      join = new_join_block(),

      # Reshape blocks
      slice = new_slice_block(),
      pivot_longer = new_pivot_longer_block(),
      pivot_wider = new_pivot_wider_block(),
      unite = new_unite_block(),
      separate = new_separate_block(),

      # Variadic blocks
      bind_rows = new_bind_rows_block(),
      bind_cols = new_bind_cols_block()
    ),
    links = list(
      # data1 feeds most blocks
      list(from = "data1", to = "select", input = "data"),
      list(from = "data1", to = "arrange", input = "data"),
      list(from = "data1", to = "rename", input = "data"),
      list(from = "data1", to = "filter", input = "data"),
      list(from = "data1", to = "mutate", input = "data"),
      list(from = "data1", to = "summarize", input = "data"),
      list(from = "data1", to = "slice", input = "data"),
      list(from = "data1", to = "pivot_longer", input = "data"),
      list(from = "data1", to = "pivot_wider", input = "data"),
      list(from = "data1", to = "unite", input = "data"),
      list(from = "data1", to = "separate", input = "data"),

      # Join gets both data sources
      list(from = "data1", to = "join", input = "x"),
      list(from = "data2", to = "join", input = "y"),

      # Bind blocks get both sources (distinct input names)
      list(from = "data1", to = "bind_rows", input = "x"),
      list(from = "data2", to = "bind_rows", input = "y"),
      list(from = "data1", to = "bind_cols", input = "x"),
      list(from = "data3", to = "bind_cols", input = "y")
    ),
    extensions = new_dag_extension()
  )
)
