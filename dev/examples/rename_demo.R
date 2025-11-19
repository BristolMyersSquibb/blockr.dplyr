# Load required libraries
library(blockr)
library(blockr.dag)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow for rename block with visualization
run_app(
  blocks = c(
  
      # Source dataset
      data = new_dataset_block(dataset = "mtcars"),

      # Rename columns to more descriptive names
      renamed = new_rename_block(
        renames = list(
          miles_per_gallon = "mpg",
          cylinders = "cyl",
          horsepower = "hp",
          weight_1000lbs = "wt"
        )
      ),

      # Add a categorical grouping variable
      prep = new_mutate_block(
        string = list(
          cyl_group = "as.factor(cylinders)"
        )
      ),

      # Summarize by cylinder groups
      summary = new_summarize_block(
        string = list(
          avg_mpg = "mean(miles_per_gallon)",
          avg_hp = "mean(horsepower)",
          count = "n()"
        ),
        by = "cyl_group",
        unpack = FALSE
      )
    ),
    links = c(
      # Connect data flow
      new_link("data", "renamed", "data"),
      new_link("renamed", "prep", "data"),
      new_link("prep", "summary", "data")
    ),
  extensions = list(

    new_dag_extension(),

    new_md_extension(

      content = c(
      "## Rename & Summarize Block Demo\n\n",
      "This workflow demonstrates the `new_rename_block()` and `new_summarize_block()` side by side.\n\n",
      "The workflow:\n\n",
      "1. Loads the mtcars dataset\n",
      "2. **Renames** columns to more descriptive names:\n",
      "   - `mpg` → `miles_per_gallon`\n",
      "   - `cyl` → `cylinders`\n",
      "   - `hp` → `horsepower`\n",
      "   - `wt` → `weight_1000lbs`\n",
      "3. Creates a categorical grouping variable (cylinder count)\n",
      "4. **Summarizes** data by groups (average MPG, HP, and count)\n\n",

      "## Summary Statistics\n\n",
      "![](blockr://summary)\n\n"
      )
      )
  )
)
