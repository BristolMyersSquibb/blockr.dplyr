# Filter block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    filtered = new_filter_expr_block(
      exprs = list(high_mpg = "mpg > 20")
    ),
    value_filtered = new_filter_block(
      conditions = list(
        list(column = "cyl", values = c(4, 6))
      )
    ),
    summary = new_summarize_expr_block(
      exprs = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        count = "n()"
      ),
      by = "cyl",
      unpack = FALSE
    )
  ),
  links = c(
    new_link("data", "filtered", "data"),
    new_link("filtered", "value_filtered", "data"),
    new_link("value_filtered", "summary", "data")
  )
)
