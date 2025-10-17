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

      # Select specific columns for analysis
      selected = new_select_block(
        columns = c("mpg", "cyl", "hp", "wt")
      ),

      # Add calculated columns using selected data
      enhanced = new_mutate_block(
        exprs = list(
          power_to_weight = "hp / wt",
          efficiency_score = "mpg / cyl"
        )
      ),

      # Summarize by cylinder count
      summary = new_summarize_block(
        exprs = list(
          avg_mpg = "mean(mpg)",
          avg_hp = "mean(hp)",
          avg_power_to_weight = "mean(power_to_weight)",
          avg_efficiency = "mean(efficiency_score)",
          count = "n()"
        ),
        by = "cyl",
        unpack = FALSE
      )
    ),
    links = c(
      # Connect data flow
      new_link("data", "selected", "data"),
      new_link("selected", "enhanced", "data"),
      new_link("enhanced", "summary", "data")
    ),
    document = c(
      "## Select Block Demo\n\n",
      "This workflow demonstrates the `new_select_block()` for focused data analysis.\n\n",
      "The workflow:\n\n",
      "1. Loads the mtcars dataset (32 rows, 11 columns)\n",
      "2. **Selects** only the columns needed for analysis:\n",
      "   - `mpg` - Miles per gallon\n",
      "   - `cyl` - Number of cylinders\n",
      "   - `hp` - Horsepower\n",
      "   - `wt` - Weight (1000 lbs)\n",
      "3. **Mutates** to add calculated columns:\n",
      "   - `power_to_weight` - Power-to-weight ratio\n",
      "   - `efficiency_score` - Fuel efficiency score\n",
      "4. **Summarizes** by cylinder count\n\n",

      "### Why use select?\n\n",
      "- Focus on relevant variables\n",
      "- Simplify downstream analysis\n",
      "- Improve performance with large datasets\n",
      "- Make data pipelines more readable\n\n",

      "## Summary Statistics\n\n",
      "![](blockr://summary)\n\n"
    )
  )
)
