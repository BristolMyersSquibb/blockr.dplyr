# Summarize expression block demo
pkgload::load_all()

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    prep = new_mutate_expr_block(
      exprs = list(cyl_group = "as.factor(cyl)")
    ),
    summary = new_summarize_expr_block(
      exprs = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        avg_wt = "mean(wt)",
        count = "n()"
      ),
      by = "cyl_group",
      unpack = FALSE
    )
  ),
  links = c(
    new_link("data", "prep", "data"),
    new_link("prep", "summary", "data")
  )
)
