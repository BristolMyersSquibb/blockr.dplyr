# Bind columns block demo
pkgload::load_all()

run_app(
  blocks = c(
    iris_data = new_dataset_block(dataset = "iris"),
    mtcars_data = new_dataset_block(dataset = "mtcars"),
    airquality_data = new_dataset_block(dataset = "airquality"),
    head1 = new_head_block(n = 5),
    head2 = new_head_block(n = 5),
    head3 = new_head_block(n = 5),
    combined_cols = new_bind_cols_block()
  ),
  links = c(
    new_link("iris_data", "head1", "data"),
    new_link("mtcars_data", "head2", "data"),
    new_link("airquality_data", "head3", "data"),
    new_link("head1", "combined_cols", "1"),
    new_link("head2", "combined_cols", "2"),
    new_link("head3", "combined_cols", "3")
  )
)
