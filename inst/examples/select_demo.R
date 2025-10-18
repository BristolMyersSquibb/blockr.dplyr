# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow for select block with focused analysis
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Source dataset
      data = new_dataset_block(dataset = "mtcars"),

      # Select specific columns for analysis with distinct combinations
      selected = new_select_block(
        columns = c("cyl", "gear"),
        distinct = TRUE
      ),

      # Count how many cars have each cyl/gear combination
      count_combos = new_summarize_block(
        exprs = list(
          count = "n()"
        ),
        by = c("cyl", "gear"),
        unpack = FALSE
      )
    ),
    links = c(
      # Connect data flow
      new_link("data", "selected", "data"),
      new_link("data", "count_combos", "data")
    ),
    document = c(
      "## Select Block Demo with Distinct\n\n",
      "This workflow demonstrates the `new_select_block()` with the `distinct` parameter.\n\n",
      "The workflow:\n\n",
      "1. Loads the mtcars dataset (32 rows, 11 columns)\n",
      "2. **Selects** columns and **keeps only distinct combinations**:\n",
      "   - `cyl` - Number of cylinders\n",
      "   - `gear` - Number of forward gears\n",
      "   - `distinct = TRUE` - Keeps only unique cyl/gear combinations\n",
      "3. **Counts** how many cars have each combination\n\n",

      "### Why use select with distinct?\n\n",
      "- **Streamlined workflow**: One block instead of two (`select` + `distinct`)\n",
      "- **Clear intent**: \"I want these columns, unique only\"\n",
      "- **Matches SQL**: Similar to `SELECT DISTINCT col1, col2`\n",
      "- **Common pattern**: Often you select columns to see unique combinations\n\n",

      "## Unique Combinations\n\n",
      "![](blockr://selected)\n\n",

      "## Count by Combination\n\n",
      "![](blockr://count_combos)\n\n"
    )
  )
)
