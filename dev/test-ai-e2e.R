pkgload::load_all("/workspace/blockr.dplyr")
library(blockr.dock)
library(blockr.dag)
library(blockr.ai)

options(shiny.port = 7860, shiny.launch.browser = FALSE)

serve(
  new_dock_board(
    blocks = c(
      data = new_dataset_block("mtcars"),
      filter = new_filter_block(),
      select = new_select_block(),
      mutate = new_mutate_block(),
      summarize = new_summarize_block(),
      arrange = new_arrange_block()
    ),
    links = c(
      new_link("data", "filter", "data"),
      new_link("data", "select", "data"),
      new_link("data", "mutate", "data"),
      new_link("data", "summarize", "data"),
      new_link("data", "arrange", "data")
    ),
    extensions = list(dag = new_dag_extension())
  ),
  plugins = custom_plugins(ai_ctrl_block())
)
