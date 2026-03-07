# Rename block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    renamed = new_rename_block(
      renames = list(
        miles_per_gallon = "mpg",
        cylinders = "cyl",
        horsepower = "hp",
        weight_1000lbs = "wt"
      )
    ),
    prep = new_mutate_expr_block(
      exprs = list(cyl_group = "as.factor(cylinders)")
    ),
    summary = new_summarize_expr_block(
      exprs = list(
        avg_mpg = "mean(miles_per_gallon)",
        avg_hp = "mean(horsepower)",
        count = "n()"
      ),
      by = "cyl_group",
      unpack = FALSE
    )
  ),
  links = c(
    new_link("data", "renamed", "data"),
    new_link("renamed", "prep", "data"),
    new_link("prep", "summary", "data")
  )
)
