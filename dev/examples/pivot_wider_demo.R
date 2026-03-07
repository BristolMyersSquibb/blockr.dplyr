# Pivot wider block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "ChickWeight"),
    subset_times = new_filter_expr_block(expr = "Time %in% c(0, 10, 20)"),
    first_chicks = new_filter_expr_block(expr = "Chick %in% c(1, 2, 3, 4, 5)"),
    wide_format = new_pivot_wider_block(
      names_from = "Time",
      values_from = "weight",
      names_prefix = "day_"
    ),
    with_gains = new_mutate_expr_block(
      exprs = list(
        gain_0_to_10 = "day_10 - day_0",
        gain_10_to_20 = "day_20 - day_10",
        total_gain = "day_20 - day_0"
      )
    ),
    sorted = new_arrange_block(
      columns = list(list(column = "total_gain", direction = "desc"))
    )
  ),
  links = c(
    new_link("data", "subset_times", "data"),
    new_link("subset_times", "first_chicks", "data"),
    new_link("first_chicks", "wide_format", "data"),
    new_link("wide_format", "with_gains", "data"),
    new_link("with_gains", "sorted", "data")
  )
)
