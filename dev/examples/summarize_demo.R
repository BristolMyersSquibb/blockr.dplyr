# Summarize block demo (no-code style with dropdown functions)
pkgload::load_all()

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    summary = new_summarize_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg"),
        avg_hp = list(func = "mean", col = "hp"),
        count = list(func = "dplyr::n", col = "")
      ),
      by = "cyl"
    )
  ),
  links = c(
    new_link("data", "summary", "data")
  )
)
