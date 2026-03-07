# Unite block demo
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr")

run_app(
  blocks = c(
    source_data = new_dataset_block(dataset = "BOD"),
    unite_cols = new_unite_block(
      col = "time___demand",
      cols = c("Time", "demand"),
      sep = "___"
    )
  ),
  links = c(
    new_link("source_data", "unite_cols", "data")
  )
)
