# JS blocks demo — filter, mutate, summarize
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    filtered = new_filter_js_block("mpg > 15"),
    mutated = new_mutate_js_block(exprs = list(hp_per_cyl = "hp / cyl")),
    summary = new_summarize_js_block(exprs = list(
      avg_mpg = "mean(mpg)",
      max_hp = "max(hp)"
    ))
  ),
  links = c(
    new_link("data", "filtered", "data"),
    new_link("filtered", "mutated", "data"),
    new_link("filtered", "summary", "data")
  )
)
