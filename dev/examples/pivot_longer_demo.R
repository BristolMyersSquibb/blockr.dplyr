# Pivot longer block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    subset_data = new_select_block(
      columns = c("mpg", "cyl", "hp", "wt", "qsec")
    ),
    long_format = new_pivot_longer_block(
      cols = c("mpg", "hp", "wt", "qsec")
    ),
    metric_summary = new_summarize_expr_block(
      exprs = list(
        mean_value = "mean(value)",
        min_value = "min(value)",
        max_value = "max(value)"
      ),
      by = "metric"
    ),
    sorted = new_arrange_block(
      columns = list(list(column = "mean_value", direction = "desc"))
    )
  ),
  links = c(
    new_link("data", "subset_data", "data"),
    new_link("subset_data", "long_format", "data"),
    new_link("long_format", "metric_summary", "data"),
    new_link("metric_summary", "sorted", "data")
  )
)
