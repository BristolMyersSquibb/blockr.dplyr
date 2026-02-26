# Test App 1: Basic Transforms (iris)
#
# Chain: dataset(iris) -> filter -> select -> arrange -> slice
# Each block supports AI-driven external control.
#
# Usage:
#   Rscript dev/test-ai-basic.R
#   (launches on port 7654)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.ai")

serve(
  new_dock_board(
    blocks = c(
      data    = new_dataset_block("iris"),
      filter  = new_filter_block(),
      select  = new_select_block(),
      arrange = new_arrange_block(),
      slice   = new_slice_block(type = "head", n = 5)
    ),
    links = c(
      new_link("data",    "filter",  "data"),
      new_link("filter",  "select",  "data"),
      new_link("select",  "arrange", "data"),
      new_link("arrange", "slice",   "data")
    ),
    extensions = list(dag = new_dag_extension())
  ),
  plugins = custom_plugins(ai_ctrl_block())
)
