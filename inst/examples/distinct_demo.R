# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow for distinct block - removing duplicate observations
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Source dataset with duplicates
      data = new_dataset_block(dataset = "iris"),

      # Show distinct species and their sepal characteristics
      species_distinct = new_distinct_block(
        columns = c("Species", "Sepal.Length", "Sepal.Width")
      ),

      # Show all distinct combinations (all columns)
      fully_distinct = new_distinct_block(
        columns = character()
      )
    ),
    links = c(
      # Connect data flow
      new_link("data", "species_distinct", "data"),
      new_link("data", "fully_distinct", "data")
    ),
    document = c(
      "## Distinct Block Demo\n\n",
      "This workflow demonstrates the `new_distinct_block()` for getting unique combinations.\n\n",
      "The workflow:\n\n",
      "1. Loads the iris dataset (150 rows)\n",
      "2. **First distinct block**: Shows unique combinations of Species, Sepal.Length, and Sepal.Width\n",
      "   - Returns only the selected columns with distinct combinations\n",
      "   - Useful for finding unique patterns in specific variables\n",
      "3. **Second distinct block**: Shows completely unique rows across all columns\n",
      "   - No columns selected = remove duplicate rows\n",
      "   - Returns all columns from unique rows\n\n",

      "### Why use distinct?\n\n",
      "- Remove duplicate observations\n",
      "- Find unique combinations of variables\n",
      "- Clean data before analysis\n",
      "- Reduce dataset size\n\n",

      "## Species-Sepal Distinct Combinations\n\n",
      "![](blockr://species_distinct)\n\n",

      "## Fully Distinct Rows\n\n",
      "![](blockr://fully_distinct)\n\n"
    )
  )
)
