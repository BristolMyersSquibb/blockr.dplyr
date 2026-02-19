pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.dock")
pkgload::load_all("/workspace/blockr.dag")
pkgload::load_all("/workspace/blockr.dplyr")
pkgload::load_all("/workspace/blockr.ai")

app <- serve(
  new_dock_board(
    blocks = c(
      data      = new_dataset_block("iris"),
      summarize = new_summarize_block(
        summaries = list(count = list(func = "dplyr::n", col = ""))
      )
    ),
    links = c(
      new_link("data", "summarize", "data")
    ),
    extensions = list(dag = new_dag_extension())
  ),
  plugins = custom_plugins(ai_ctrl_block())
)

shiny::runApp(app, port = 7654L, host = "0.0.0.0", launch.browser = FALSE)
