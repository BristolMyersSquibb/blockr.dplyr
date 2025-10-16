# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo with three datasets
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Three head blocks to create smaller datasets
      head1 = new_head_block(n = 5),
      head2 = new_head_block(n = 5),
      head3 = new_head_block(n = 5),

      # Bind columns together
      combined_cols = new_bind_cols_block(),

      # Source data blocks
      iris_data = new_dataset_block(dataset = "iris"),
      mtcars_data = new_dataset_block(dataset = "mtcars"),
      airquality_data = new_dataset_block(dataset = "airquality")
    ),
    links = c(
      # Connect sources to head blocks
      new_link("iris_data", "head1", "data"),
      new_link("mtcars_data", "head2", "data"),
      new_link("airquality_data", "head3", "data"),

      # Connect head outputs to bind_cols
      new_link("head1", "combined_cols", "1"),
      new_link("head2", "combined_cols", "2"),
      new_link("head3", "combined_cols", "3")
    ),
    document = c(
      "## Bind Columns Demo\n\n",
      "This workflow demonstrates the `new_bind_cols_block()` which combines ",
      "three datasets horizontally (side-by-side).\n\n",
      "The workflow:\n\n",

      "1. Takes three datasets (iris, mtcars, airquality)\n",
      "2. Takes the first 5 rows of each using `head` blocks\n",
      "3. Combines them side-by-side using `bind_cols`\n\n",

      "Note: All inputs must have the same number of rows for `bind_cols` to work.\n",

      "## Combined Data\n\n",

      "![](blockr://combined_cols)\n\n"
    )
  )
)
