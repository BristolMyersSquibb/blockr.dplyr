# Demo script for unite block
library(blockr)
library(blockr.dag)
library(blockr.md)
library(blockr.dplyr)

pkgload::load_all()

# Example: Unite Time and demand columns
run_app(
  blocks = c(
    source_data = new_dataset_block(
      dataset = "BOD"
    ),
    unite_cols = new_unite_block(
      col = "time___demand",
      cols = c("Time", "demand"),
      sep = "___"
    )
  ),
  links = c(
    new_link("source_data", "unite_cols", "data")
  ),
  extensions = list(
    new_dag_extension(),
    new_md_extension(
      content = c(
        "## Unite Block Demo\n\n",
        "This example demonstrates the `new_unite_block()` which combines multiple columns into one.\n\n",
        "**What it does:**\n",
        "- Combines `Time` and `demand` columns\n",
        "- Uses three underscores (`___`) as separator\n",
        "- Creates a new `time___demand` column\n",
        "- Removes the original columns (default behavior)\n\n",
        "### Source Data\n\n",
        "![](blockr://source_data)\n\n",
        "### After Unite\n\n",
        "![](blockr://unite_cols)\n\n"
      )
    )
  )
)
