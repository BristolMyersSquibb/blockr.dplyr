# Filter block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    filtered = new_filter_js_block("mpg > 20"),
    value_filtered = new_filter_block()
  ),
  links = c(
    new_link("data", "filtered", "data"),
    new_link("filtered", "value_filtered", "data")
  )
)
