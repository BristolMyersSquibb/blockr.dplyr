# Unified filter demo — auto-detecting column types
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "iris"),
    filtered = new_filter_unified_block()
  ),
  links = c(
    new_link("data", "filtered", "data")
  )
)
