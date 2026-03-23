pkgload::load_all("/workspace/blockr.dplyr")
library(blockr.dock)
library(blockr.dag)
library(blockr.ai)

options(shiny.port = 7860, shiny.launch.browser = FALSE)

# Test data for reshape blocks
df_wide <- data.frame(
  id = 1:4,
  score_2024 = c(85, 92, 78, 95),
  score_2025 = c(90, 88, 82, 97)
)

df_long <- data.frame(
  id = c(1L, 1L, 2L, 2L),
  var = c("a", "b", "a", "b"),
  val = c(10, 40, 20, 50)
)

df_names <- data.frame(
  first = c("John", "Jane", "Bob"),
  last = c("Doe", "Smith", "Jones"),
  age = c(30, 25, 40)
)

df_full <- data.frame(
  full_name = c("John Doe", "Jane Smith", "Bob Jones"),
  age = c(30, 25, 40)
)

serve(
  new_dock_board(
    blocks = c(
      # mtcars-based blocks
      data = new_dataset_block("mtcars"),
      filter = new_filter_block(),
      select = new_select_block(),
      mutate = new_mutate_block(),
      summarize = new_summarize_block(),
      arrange = new_arrange_block(),
      rename = new_rename_block(),
      slice = new_slice_block(),

      # Reshape blocks with custom data
      wide_data = new_static_block(df_wide),
      pivot_longer = new_pivot_longer_block(),

      long_data = new_static_block(df_long),
      pivot_wider = new_pivot_wider_block(),

      names_data = new_static_block(df_names),
      unite = new_unite_block(),

      full_data = new_static_block(df_full),
      separate = new_separate_block()
    ),
    links = c(
      new_link("data", "filter", "data"),
      new_link("data", "select", "data"),
      new_link("data", "mutate", "data"),
      new_link("data", "summarize", "data"),
      new_link("data", "arrange", "data"),
      new_link("data", "rename", "data"),
      new_link("data", "slice", "data"),
      new_link("wide_data", "pivot_longer", "data"),
      new_link("long_data", "pivot_wider", "data"),
      new_link("names_data", "unite", "data"),
      new_link("full_data", "separate", "data")
    ),
    extensions = list(dag = new_dag_extension())
  ),
  plugins = custom_plugins(ai_ctrl_block())
)
