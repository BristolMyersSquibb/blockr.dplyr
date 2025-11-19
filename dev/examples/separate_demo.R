# Demo script for separate block
library(blockr)
library(blockr.dag)
library(blockr.md)
library(blockr.dplyr)

pkgload::load_all()

# Example: First unite, then separate to demonstrate the inverse operation
run_app(
  blocks = c(
    source_data = new_dataset_block(
      dataset = "BOD"
    ),
    # First unite Time and demand
    unite_cols = new_unite_block(
      col = "time___demand",
      cols = c("Time", "demand"),
      sep = "___"
    ),
    # Then separate it back
    separate_cols = new_separate_block(
      col = "time___demand",
      into = c("Time", "demand"),
      sep = "___"
    )
  ),
  links = c(
    new_link("source_data", "unite_cols", "data"),
    new_link("unite_cols", "separate_cols", "data")
  ),
  extensions = list(
    new_dag_extension(),
    new_md_extension(
      content = c(
        "## Separate Block Demo\n\n",
        "This example demonstrates the `new_separate_block()` which splits one column into multiple columns.\n\n",
        "**Workflow:**\n",
        "1. Start with BOD dataset (Time and demand columns)\n",
        "2. Unite them into `time___demand` column\n",
        "3. Separate it back into `Time` and `demand` columns\n\n",
        "This shows that separate is the inverse operation of unite.\n\n",
        "### Source Data\n\n",
        "![](blockr://source_data)\n\n",
        "### After Unite\n\n",
        "![](blockr://unite_cols)\n\n",
        "### After Separate (back to original)\n\n",
        "![](blockr://separate_cols)\n\n"
      )
    )
  )
)
