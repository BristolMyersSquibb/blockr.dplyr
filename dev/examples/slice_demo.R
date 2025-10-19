# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow for slice block with multiple slice types
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Source dataset
      data = new_dataset_block(dataset = "mtcars"),

      # Slice variant 1: Get first 5 rows
      head_slice = new_slice_block(
        type = "head",
        n = 5
      ),

      # Slice variant 2: Get top 3 cars by MPG
      max_mpg = new_slice_block(
        type = "max",
        order_by = "mpg",
        n = 3,
        with_ties = FALSE
      ),

      # Slice variant 3: Get bottom 3 cars by MPG
      min_mpg = new_slice_block(
        type = "min",
        order_by = "mpg",
        n = 3,
        with_ties = FALSE
      ),

      # Summarize the max MPG cars
      max_summary = new_summarize_block(
        exprs = list(
          avg_mpg = "mean(mpg)",
          avg_hp = "mean(hp)",
          avg_wt = "mean(wt)",
          count = "n()"
        ),
        unpack = FALSE
      ),

      # Summarize the min MPG cars
      min_summary = new_summarize_block(
        exprs = list(
          avg_mpg = "mean(mpg)",
          avg_hp = "mean(hp)",
          avg_wt = "mean(wt)",
          count = "n()"
        ),
        unpack = FALSE
      )
    ),
    links = c(
      # Connect data flow
      new_link("data", "head_slice", "data"),
      new_link("data", "max_mpg", "data"),
      new_link("data", "min_mpg", "data"),
      new_link("max_mpg", "max_summary", "data"),
      new_link("min_mpg", "min_summary", "data")
    ),
    document = c(
      "## Slice Block Demo\n\n",
      "This workflow demonstrates the `new_slice_block()` with different slice types.\n\n",
      "The workflow creates three parallel slicing operations:\n\n",

      "### 1. Head Slice\n",
      "Get the first 5 rows of the dataset\n",
      "- **Type:** `head`\n",
      "- **Rows:** 5\n\n",
      "![](blockr://head_slice)\n\n",

      "### 2. Maximum MPG Slice\n",
      "Get the top 3 most fuel-efficient cars\n",
      "- **Type:** `max`\n",
      "- **Order by:** `mpg`\n",
      "- **Rows:** 3\n\n",
      "![](blockr://max_mpg)\n\n",

      "**Summary statistics for most efficient cars:**\n\n",
      "![](blockr://max_summary)\n\n",

      "### 3. Minimum MPG Slice\n",
      "Get the 3 least fuel-efficient cars\n",
      "- **Type:** `min`\n",
      "- **Order by:** `mpg`\n",
      "- **Rows:** 3\n\n",
      "![](blockr://min_mpg)\n\n",

      "**Summary statistics for least efficient cars:**\n\n",
      "![](blockr://min_summary)\n\n",

      "### Slice Types Available\n\n",
      "The slice block supports multiple operations:\n",
      "- **head/tail** - First/last N rows\n",
      "- **max/min** - Top/bottom N rows by value\n",
      "- **sample** - Random N rows (with optional replacement)\n",
      "- **custom** - Specify row positions with R expressions\n\n",

      "### Why use slice?\n\n",
      "- Extract specific rows by position or value\n",
      "- Get top-N or bottom-N results\n",
      "- Sample data for testing or visualization\n",
      "- Combine with `.by` parameter for group-wise operations\n"
    )
  )
)
