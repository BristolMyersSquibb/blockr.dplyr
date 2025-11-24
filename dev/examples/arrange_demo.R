# Load required libraries
library(blockr)
library(blockr.dag)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow for arrange block with multi-column sorting
run_app(
  blocks = c(
    # Source dataset
    data = new_dataset_block(dataset = "mtcars"),

    # Add a categorical variable for better grouping
    categorized = new_mutate_block(
      exprs = list(
        car_type = "dplyr::case_when(cyl <= 4 ~ 'Economy', cyl <= 6 ~ 'Standard', TRUE ~ 'Performance')"
      )
    ),

    # Sort by multiple columns: car type (asc), then mpg (desc), then hp (desc)
    sorted = new_arrange_block(
      columns = list(
        list(column = "car_type", direction = "asc"),
        list(column = "mpg", direction = "desc"),
        list(column = "hp", direction = "desc")
      )
    ),

    # Get the top 10 most efficient cars in each category
    top_performers = new_slice_block(
      type = "head",
      n = 10
    ),

    # Summarize the sorted and filtered data
    summary = new_summarize_block(
      exprs = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        max_mpg = "max(mpg)",
        min_hp = "min(hp)",
        count = "n()"
      ),
      by = "car_type",
      unpack = FALSE
    )
  ),
  links = c(
    # Connect data flow
    new_link("data", "categorized", "data"),
    new_link("categorized", "sorted", "data"),
    new_link("sorted", "top_performers", "data"),
    new_link("top_performers", "summary", "data")
  ),
  extensions = list(
    new_dag_extension(),
    new_md_extension(
      content = c(
        "## Arrange Block Demo\n\n",
        "This workflow demonstrates the `new_arrange_block()` for multi-column sorting.\n\n",
        "The workflow:\n\n",
        "1. Loads the mtcars dataset\n",
        "2. **Categorizes** cars by cylinder count:\n",
        "   - Economy: ≤4 cylinders\n",
        "   - Standard: 5-6 cylinders\n",
        "   - Performance: ≥7 cylinders\n",
        "3. **Arranges** by multiple columns:\n",
        "   - Primary: `car_type` (ascending) - group similar cars\n",
        "   - Secondary: `mpg` (descending) - most efficient first\n",
        "   - Tertiary: `hp` (descending) - most powerful first\n",
        "4. **Slices** to get top 10 results\n",
        "5. **Summarizes** by car type\n\n",

        "### Why use arrange?\n\n",
        "- Sort by multiple columns with individual sort directions\n",
        "- Prepare data for top-N analysis (used with slice)\n",
        "- Create meaningful orderings for reports\n",
        "- Find extreme values (min/max) within groups\n\n",

        "## Top Performers Data\n\n",
        "![](blockr://top_performers)\n\n",

        "## Summary by Car Type\n\n",
        "![](blockr://summary)\n\n"
      )
    )
  )
)
