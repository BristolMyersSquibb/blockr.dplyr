# Unified blocks demo — filter, mutate, summarize
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    filtered = new_filter_unified_block(),
    mutated = new_mutate_unified_block(
      exprs = list(hp_per_cyl = "hp / cyl")
    ),
    summary = new_summarize_unified_block()
  ),
  links = c(
    new_link("data", "filtered", "data"),
    new_link("filtered", "mutated", "data"),
    new_link("filtered", "summary", "data")
  )
)
