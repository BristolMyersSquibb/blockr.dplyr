# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow showcasing the pivot_longer block
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Use mtcars - a classic wide format dataset
      data = new_dataset_block(dataset = "mtcars"),

      # Select a subset for clearer demonstration
      subset_data = new_select_block(
        columns = c("mpg", "cyl", "hp", "wt", "qsec")
      ),

      # Pivot longer: Convert performance metrics to rows
      long_format = new_pivot_longer_block(
        cols = c("mpg", "hp", "wt", "qsec")
      ),

      # Calculate summary statistics by metric
      metric_summary = new_summarize_block(
        exprs = list(
          mean_value = "mean(value)",
          min_value = "min(value)",
          max_value = "max(value)"
        ),
        by = "metric"
      ),

      # Arrange by mean to see which metrics have highest values
      sorted = new_arrange_block(
        columns = list(
          list(column = "mean_value", direction = "desc")
        )
      )
    ),
    links = c(
      new_link("data", "subset_data", "data"),
      new_link("subset_data", "long_format", "data"),
      new_link("long_format", "metric_summary", "data"),
      new_link("metric_summary", "sorted", "data")
    ),
    document = c(
      "## Pivot Longer Block Demo\n\n",
      "This workflow demonstrates the `new_pivot_longer_block()` for reshaping wide data to long format.\n\n",

      "### What is Pivot Longer?\n\n",
      "Pivot longer transforms wide data (many columns) into long data (more rows, fewer columns).\n",
      "This is useful when column names represent values of a variable rather than variables themselves.\n\n",

      "### Dataset: mtcars\n\n",
      "The `mtcars` dataset contains performance metrics for 32 cars.\n",
      "Each metric (mpg, hp, wt, qsec) is a separate column - this is 'wide format' data.\n\n",

      "### What this workflow does:\n\n",
      "1. Loads the `mtcars` dataset (32 cars × 11 columns)\n",
      "2. **Selects** key columns: mpg, cyl, hp, wt, qsec\n",
      "3. **Pivots** the performance metric columns into rows:\n",
      "   - Selected columns: `mpg`, `hp`, `wt`, `qsec`\n",
      "   - Creates `metric` column from the original column names\n",
      "   - Creates `value` column from the metric values\n",
      "   - Keeps `cyl` column as an identifier\n",
      "4. **Summarizes** to calculate mean, min, and max for each metric\n",
      "5. **Arranges** by mean value (highest first)\n\n",

      "### Why Pivot This Data?\n\n",
      "- **Compare metrics**: All metrics in one column makes cross-metric analysis easier\n",
      "- **Visualization**: Long format is required for faceted plots or grouped visualizations\n",
      "- **Statistical analysis**: Many analysis functions expect long format\n",
      "- **Calculation**: Easier to calculate summary statistics across all metrics\n\n",

      "### Common Use Cases:\n\n",
      "- **Time series data**: When each time period is a column (Q1_2020, Q2_2020, etc.)\n",
      "- **Survey data**: When each question is a column\n",
      "- **Experimental data**: When each measurement type is a column\n",
      "- **Preparing for visualization**: Many plotting functions expect long format data\n\n",

      "### Try these features:\n\n",
      "- **Change column selection**: Add or remove metric columns to pivot (try adding `disp`, `drat`)\n",
      "- **Rename output columns**: Change `names_to` and `values_to` parameters\n",
      "- **Drop NAs**: Check the 'Drop rows with NA values' option to remove missing data\n",
      "- **Remove prefixes**: Use `names_prefix` to clean column names\n\n",

      "## Long Format Data (First 20 rows)\n\n",
      "![](blockr://long_format)\n\n",

      "## Summary Statistics by Metric\n\n",
      "![](blockr://sorted)\n\n"
    )
  )
)
