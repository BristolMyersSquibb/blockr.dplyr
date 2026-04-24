pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.ai")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
library(blockr.dag)
library(blockr.extra)

options(
  blockr.html_table_preview = TRUE,
  "g6R.mode" = "dev",
  "g6R.preserve_elements_position" = TRUE
)

# ADaM data via blockr.dm's example dataset — every column carries
# attr(col, "label"), so migrated pickers should render secondary labels
# alongside the column name (e.g. "USUBJID  Unique Subject Identifier").
serve(
  new_dock_board(
    blocks = c(
      dm = new_dm_example_block(dataset = "pharmaverseadam"),
      adsl = new_dm_pull_block(table = "adsl"),
      adae = new_dm_pull_block(table = "adae"),

      # Column-picker blocks — single ADSL input
      select = new_select_block(),
      arrange = new_arrange_block(),
      rename = new_rename_block(),

      # Expression blocks
      filter = new_filter_block(),
      mutate = new_mutate_block(),
      summarize = new_summarize_block(),

      # Join (binary — ADSL x ADAE)
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
      list(from = "dm", to = "adsl", input = "data"),
      list(from = "dm", to = "adae", input = "data"),

      # adsl feeds most blocks
      list(from = "adsl", to = "select", input = "data"),
      list(from = "adsl", to = "arrange", input = "data"),
      list(from = "adsl", to = "rename", input = "data"),
      list(from = "adsl", to = "filter", input = "data"),
      list(from = "adsl", to = "mutate", input = "data"),
      list(from = "adsl", to = "summarize", input = "data"),
      list(from = "adsl", to = "slice", input = "data"),
      list(from = "adsl", to = "pivot_longer", input = "data"),
      list(from = "adsl", to = "pivot_wider", input = "data"),
      list(from = "adsl", to = "unite", input = "data"),
      list(from = "adsl", to = "separate", input = "data"),

      # Join: ADSL x ADAE
      list(from = "adsl", to = "join", input = "x"),
      list(from = "adae", to = "join", input = "y"),

      # bind_rows: union of columns, row counts can differ
      list(from = "adsl", to = "bind_rows", input = "x"),
      list(from = "adae", to = "bind_rows", input = "y"),

      # bind_cols: requires equal row counts — feed adsl on both sides
      list(from = "adsl", to = "bind_cols", input = "x"),
      list(from = "adsl", to = "bind_cols", input = "y")
    ),
    extensions = new_dag_extension()
  ),
  plugins = custom_plugins(c(ai_ctrl_block()))
)
