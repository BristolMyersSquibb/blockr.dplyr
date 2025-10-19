# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow showcasing the pivot_wider block
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Load ChickWeight dataset (long format - perfect for pivot_wider!)
      data = new_dataset_block(dataset = "ChickWeight"),

      # Filter to specific time points for clearer demonstration
      subset_times = new_filter_expr_block(
        expr = "Time %in% c(0, 10, 20)"
      ),

      # Select first 5 chicks for manageable output
      first_chicks = new_filter_expr_block(
        expr = "Chick %in% c(1, 2, 3, 4, 5)"
      ),

      # Pivot wider: Create columns for each time point
      wide_format = new_pivot_wider_block(
        names_from = "Time",
        values_from = "weight",
        names_prefix = "day_"
      ),

      # Calculate weight gain
      with_gains = new_mutate_block(
        exprs = list(
          gain_0_to_10 = "day_10 - day_0",
          gain_10_to_20 = "day_20 - day_10",
          total_gain = "day_20 - day_0"
        )
      ),

      # Arrange by total weight gain
      sorted = new_arrange_block(
        columns = list(
          list(column = "total_gain", direction = "desc")
        )
      )
    ),
    links = c(
      new_link("data", "subset_times", "data"),
      new_link("subset_times", "first_chicks", "data"),
      new_link("first_chicks", "wide_format", "data"),
      new_link("wide_format", "with_gains", "data"),
      new_link("with_gains", "sorted", "data")
    ),
    document = c(
      "## Pivot Wider Block Demo\n\n",
      "This workflow demonstrates the `new_pivot_wider_block()` for reshaping long data to wide format.\n\n",

      "### What is Pivot Wider?\n\n",
      "Pivot wider transforms long data (many rows) into wide data (more columns, fewer rows).\n",
      "This is useful for creating summary tables or matrices where row-column combinations become cells.\n\n",

      "### Dataset: ChickWeight\n\n",
      "The `ChickWeight` dataset tracks weight of chicks over time on different diets.\n",
      "It's in long format: Each row is one chick at one time point.\n\n",

      "### What this workflow does:\n\n",
      "1. Loads the `ChickWeight` dataset (578 rows in long format)\n",
      "2. **Filters** to specific time points (Day 0, 10, 20)\n",
      "3. **Filters** to first 5 chicks for manageable output\n",
      "4. **Pivots** to wide format:\n",
      "   - `names_from = 'Time'` - Creates columns for each time point\n",
      "   - `values_from = 'weight'` - Fills cells with weight values\n",
      "   - `names_prefix = 'day_'` - Adds 'day_' prefix to column names\n",
      "   - Result: One row per chick, with day_0, day_10, day_20 columns\n",
      "5. **Calculates** weight gain between time periods\n",
      "6. **Arranges** by total weight gain (highest first)\n\n",

      "### Why Pivot This Data?\n\n",
      "- **Time comparison**: Easy to see all time points for one chick in a single row\n",
      "- **Calculate changes**: Simpler to calculate differences between time points\n",
      "- **Summary tables**: Creates a more readable summary format\n",
      "- **Matrix operations**: Wide format works better for certain calculations\n\n",

      "### Common Use Cases:\n\n",
      "- **Crosstabs**: Creating frequency tables (rows × columns)\n",
      "- **Time series**: Converting long format to one column per time period\n",
      "- **Pivot tables**: Creating Excel-style pivot table layouts\n",
      "- **Before/after comparisons**: Side-by-side comparison of measurements\n",
      "- **Correlation analysis**: Wide format needed for correlation matrices\n\n",

      "### Try these features:\n\n",
      "- **Change time points**: Filter to different days (0, 5, 10, 15, 20, 21)\n",
      "- **Change names/values**: Try pivoting by `Diet` instead of `Time`\n",
      "- **Fill missing values**: Use `values_fill` to replace NAs (e.g., '0' or 'NA')\n",
      "- **Remove prefix**: Clear `names_prefix` to get columns named '0', '10', '20'\n",
      "- **Specify ID columns**: Use `id_cols` to control which columns identify rows\n\n",

      "## Wide Format (One Row per Chick)\n\n",
      "![](blockr://wide_format)\n\n",

      "## Weight Gains (Sorted by Total Gain)\n\n",
      "![](blockr://sorted)\n\n"
    )
  )
)
