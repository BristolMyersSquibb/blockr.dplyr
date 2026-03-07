# Mutate expression block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    enhanced = new_mutate_expr_block(
      exprs = list(
        efficiency = "mpg / wt",
        engine_size = "dplyr::case_when(cyl <= 4 ~ 'Small', cyl == 6 ~ 'Medium', TRUE ~ 'Large')",
        power_ratio = "hp / wt * 0.01",
        mpg_rounded = "round(mpg, 1)"
      ),
      by = "cyl"
    ),
    results = new_select_block(
      columns = c("mpg", "mpg_rounded", "wt", "hp", "efficiency", "power_ratio", "engine_size")
    ),
    sorted = new_arrange_block(
      columns = list(list(column = "efficiency", direction = "desc"))
    )
  ),
  links = c(
    new_link("data", "enhanced", "data"),
    new_link("enhanced", "results", "data"),
    new_link("results", "sorted", "data")
  )
)
