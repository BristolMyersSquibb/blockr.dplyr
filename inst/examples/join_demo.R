# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow for join block - simple two dataset example
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Create first dataset: BOD data
      bod_data1 = new_dataset_block(dataset = "BOD"),

      # Create second dataset: BOD data again
      bod_data2 = new_dataset_block(dataset = "BOD"),

      # Join the two datasets - explicit join on Time column
      join_result = new_join_block(
        type = "left_join",
        by = list("Time" = "Time")
      )
    ),
    links = c(
      # Connect the join
      new_link("bod_data1", "join_result", "x"),
      new_link("bod_data2", "join_result", "y")
    ),
    document = c(
      "## Join Block Demo\n\n",
      "This workflow demonstrates the `new_join_block()` joining two datasets.\n\n",
      "We use the BOD (Biochemical Oxygen Demand) dataset twice to demonstrate the join functionality.\n\n",

      "### Source Data\n\n",
      "**BOD Data (Left):**\n\n",
      "![](blockr://bod_data1)\n\n",

      "**BOD Data (Right):**\n\n",
      "![](blockr://bod_data2)\n\n",

      "### Join Result\n",
      "Joining on the `Time` column (Left Dataset ← Right Dataset)\n",
      "- **Type:** `left_join` (keeps all rows from left)\n",
      "- **Join key:** `Time = Time` (explicit mapping)\n",
      "- **Result:** Since both datasets are identical, all rows match\n\n",
      "![](blockr://join_result)\n\n",

      "### Join Types Available\n\n",
      "The join block supports these operations:\n",
      "- **left_join** - Keep all rows from left dataset, add matching from right\n",
      "- **right_join** - Keep all rows from right dataset, add matching from left\n",
      "- **inner_join** - Keep only rows that match in both datasets\n",
      "- **full_join** - Keep all rows from both datasets\n",
      "- **semi_join** - Keep left rows that have matches in right (no columns added)\n",
      "- **anti_join** - Keep left rows that have NO matches in right\n\n",

      "### Join Modes\n\n",
      "- **Natural join**: Automatically match columns with the same names\n",
      "- **Explicit join**: Manually map columns from left to right datasets\n"
    )
  )
)
