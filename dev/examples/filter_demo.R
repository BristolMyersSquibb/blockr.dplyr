# JS blocks demo — filter, mutate, summarize
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr")
pkgload::load_all("blockr.extra")

options(
  blockr.html_table_preview = TRUE
)


run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    filtered = new_filter_block()
  ),
  links = c(
    new_link("data", "filtered", "data")
  )
)
