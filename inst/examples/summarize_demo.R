# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
library(blockr.ggplot)
pkgload::load_all()

# Demo workflow for summarize block with visualization
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Source dataset
      data = new_dataset_block(dataset = "mtcars"),

      # Add a categorical grouping variable (cylinder count)
      prep = new_mutate_block(
        string = list(
          cyl_group = "as.factor(cyl)"
        )
      ),

      # Summarize by cylinder groups
      summary = new_summarize_block(
        string = list(
          avg_mpg = "mean(mpg)",
          avg_hp = "mean(hp)",
          avg_wt = "mean(wt)",
          count = "n()"
        ),
        by = "cyl_group",
        unpack = FALSE
      ),

      # Visualize the summary with ggplot
      viz = new_ggplot_block(
        type = "bar",
        x = "cyl_group",
        y = "avg_mpg",
        fill = "cyl_group"
      )
    ),
    links = c(
      # Connect data flow
      new_link("data", "prep", "data"),
      new_link("prep", "summary", "data"),
      new_link("summary", "viz", "data")
    ),
    document = c(
      "## Summarize Block Demo\n\n",
      "This workflow demonstrates the `new_summarize_block()` with visualization.\n\n",
      "The workflow:\n\n",
      "1. Loads the mtcars dataset\n",
      "2. Creates a categorical grouping variable (cylinder count)\n",
      "3. Summarizes data by groups (average MPG, HP, weight, and count)\n",
      "4. Visualizes the summary using a ggplot bar chart\n\n",

      "## Summary Statistics\n\n",
      "![](blockr://summary)\n\n",

      "## Visualization\n\n",
      "![](blockr://viz)\n\n"
    )
  )
)
