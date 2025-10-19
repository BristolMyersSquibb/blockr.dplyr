# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Generated board code demonstrating new_bind_rows_block
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Create first dataset: iris setosa
      setosa_data = new_filter_expr_block(
        expressions = list(Species = "Species == 'setosa'")
      ),

      # Create second dataset: iris versicolor
      versicolor_data = new_filter_expr_block(
        expressions = list(Species = "Species == 'versicolor'")
      ),

      # Create third dataset: iris virginica
      virginica_data = new_filter_expr_block(
        expressions = list(Species = "Species == 'virginica'")
      ),

      # Bind all three datasets together using new_bind_rows_block
      combined_data = new_bind_rows_block(),

      # Add a count to show the result
      result_summary = new_summarize_block(
        string = list(
          count = "n()",
          avg_sepal_length = "mean(Sepal.Length)",
          avg_sepal_width = "mean(Sepal.Width)"
        ),
        by = "Species",
        unpack = FALSE
      ),

      # Source data block
      iris_data = new_dataset_block(
        selected_dataset = "iris"
      )
    ),
    links = c(
      # Connect source to filters
      new_link("iris_data", "setosa_data", "data"),
      new_link("iris_data", "versicolor_data", "data"),
      new_link("iris_data", "virginica_data", "data"),

      # Connect filtered data to bind_rows block
      new_link("setosa_data", "combined_data", "1"),
      new_link("versicolor_data", "combined_data", "2"),
      new_link("virginica_data", "combined_data", "3"),

      # Connect combined data to summary
      new_link("combined_data", "result_summary", "data")
    ),
    document = c(
      "## Bind Rows Demo\n\n",
      "This workflow demonstrates the `new_bind_rows_block()` which combines ",
      "multiple datasets vertically.\n\n",
      "The workflow:\n",
      "1. Takes the iris dataset\n",
      "2. Filters it into three separate datasets (one per species)\n",
      "3. Combines them back together using `new_bind_rows_block()`\n",
      "4. Summarizes the result\n\n",
      "### Combined Data Summary\n\n",
      "![](blockr://result_summary)\n\n"
    )
  )
)
