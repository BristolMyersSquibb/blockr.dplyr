# Test App 2: Expression & Aggregation (mtcars)
#
# Chain: dataset(mtcars) -> mutate_expr -> summarize -> rename
# Each block supports AI-driven external control.
#
# Usage:
#   Rscript dev/test-ai-expr.R
#   (launches on port 7654)

pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.dock")
pkgload::load_all("/workspace/blockr.dag")
pkgload::load_all("/workspace/blockr.dplyr")
pkgload::load_all("/workspace/blockr.ai")

app <- serve(
  new_dock_board(
    blocks = c(
      data      = new_dataset_block("mtcars"),
      mutate    = new_mutate_expr_block(exprs = list(new_col = "1")),
      summarize = new_summarize_block(
        summaries = list(count = list(func = "dplyr::n", col = ""))
      ),
      rename    = new_rename_block(renames = list(new_col = character()))
    ),
    links = c(
      new_link("data",      "mutate",    "data"),
      new_link("mutate",    "summarize", "data"),
      new_link("summarize", "rename",    "data")
    ),
    extensions = list(dag = new_dag_extension())
  ),
  plugins = custom_plugins(ai_ctrl_block())
)

shiny::runApp(app, port = 7654L, host = "0.0.0.0", launch.browser = FALSE)
