# Bind rows block demo
pkgload::load_all()

run_app(
  blocks = c(
    iris_data = new_dataset_block(dataset = "iris"),
    setosa_data = new_filter_expr_block(
      exprs = list(Species = "Species == 'setosa'")
    ),
    versicolor_data = new_filter_expr_block(
      exprs = list(Species = "Species == 'versicolor'")
    ),
    virginica_data = new_filter_expr_block(
      exprs = list(Species = "Species == 'virginica'")
    ),
    combined_data = new_bind_rows_block(),
    result_summary = new_summarize_expr_block(
      exprs = list(
        count = "n()",
        avg_sepal_length = "mean(Sepal.Length)",
        avg_sepal_width = "mean(Sepal.Width)"
      ),
      by = "Species",
      unpack = FALSE
    )
  ),
  links = c(
    new_link("iris_data", "setosa_data", "data"),
    new_link("iris_data", "versicolor_data", "data"),
    new_link("iris_data", "virginica_data", "data"),
    new_link("setosa_data", "combined_data", "1"),
    new_link("versicolor_data", "combined_data", "2"),
    new_link("virginica_data", "combined_data", "3"),
    new_link("combined_data", "result_summary", "data")
  )
)
