pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.extra")

options(
  blockr.html_table_preview = TRUE
)

serve(
  new_dock_board(
    extensions = new_dag_extension()
  ),
  plugins = custom_plugins(c(manage_project()))
)
