# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow comparing filter, value_filter, and summarize block styles
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Source dataset
      data = new_dataset_block(dataset = "mtcars"),

      # Expression filter block - filter by expression
      filtered = new_filter_expr_block(
        exprs = list(
          high_mpg = "mpg > 20"
        )
      ),

      # Value filter block - filter by specific values
      value_filtered = new_value_filter_block(
        conditions = list(
          list(
            column = "cyl",
            values = c(4, 6)
          )
        )
      ),

      # Summarize the filtered data
      summary = new_summarize_block(
        exprs = list(
          avg_mpg = "mean(mpg)",
          avg_hp = "mean(hp)",
          count = "n()"
        ),
        by = "cyl",
        unpack = FALSE
      )
    ),
    links = c(
      # Connect data flow
      new_link("data", "filtered", "data"),
      new_link("filtered", "value_filtered", "data"),
      new_link("value_filtered", "summary", "data")
    ),
    document = c(
      "## Filter Blocks Style Comparison\n\n",
      "This workflow demonstrates three different filtering/transformation blocks:\n\n",
      "1. **Expression Filter Block**: Filter rows using R expressions\n",
      "2. **Value Filter Block**: Filter by selecting specific column values\n",
      "3. **Summarize Block**: Aggregate data with grouping\n\n",

      "## Filtered Data\n\n",
      "![](blockr://value_filtered)\n\n",

      "## Summary Statistics\n\n",
      "![](blockr://summary)\n\n"
    )
  )
)
