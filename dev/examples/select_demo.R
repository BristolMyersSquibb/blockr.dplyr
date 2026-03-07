# Select block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    selected = new_select_block(
      columns = c("cyl", "gear"),
      distinct = TRUE
    ),
    count_combos = new_summarize_expr_block(
      exprs = list(count = "n()"),
      by = c("cyl", "gear"),
      unpack = FALSE
    )
  ),
  links = c(
    new_link("data", "selected", "data"),
    new_link("data", "count_combos", "data")
  )
)
