# Slice block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    head_slice = new_slice_block(type = "head", n = 5),
    max_mpg = new_slice_block(type = "max", order_by = "mpg", n = 3, with_ties = FALSE),
    min_mpg = new_slice_block(type = "min", order_by = "mpg", n = 3, with_ties = FALSE),
    max_summary = new_summarize_expr_block(
      exprs = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        avg_wt = "mean(wt)",
        count = "n()"
      ),
      unpack = FALSE
    ),
    min_summary = new_summarize_expr_block(
      exprs = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        avg_wt = "mean(wt)",
        count = "n()"
      ),
      unpack = FALSE
    )
  ),
  links = c(
    new_link("data", "head_slice", "data"),
    new_link("data", "max_mpg", "data"),
    new_link("data", "min_mpg", "data"),
    new_link("max_mpg", "max_summary", "data"),
    new_link("min_mpg", "min_summary", "data")
  )
)
