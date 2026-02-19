# Arrange block demo
pkgload::load_all()

blockr::run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    categorized = new_mutate_expr_block(
      exprs = list(
        car_type = "dplyr::case_when(cyl <= 4 ~ 'Economy', cyl <= 6 ~ 'Standard', TRUE ~ 'Performance')"
      )
    ),
    sorted = new_arrange_block(
      columns = list(
        list(column = "car_type", direction = "asc"),
        list(column = "mpg", direction = "desc"),
        list(column = "hp", direction = "desc")
      )
    ),
    top_performers = new_slice_block(type = "head", n = 10),
    summary = new_summarize_expr_block(
      exprs = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        max_mpg = "max(mpg)",
        min_hp = "min(hp)",
        count = "n()"
      ),
      by = "car_type",
      unpack = FALSE
    )
  ),
  links = c(
    new_link("data", "categorized", "data"),
    new_link("categorized", "sorted", "data"),
    new_link("sorted", "top_performers", "data"),
    new_link("top_performers", "summary", "data")
  )
)
