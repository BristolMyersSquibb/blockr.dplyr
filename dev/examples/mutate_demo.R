# Load required libraries
library(blockr.core)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow showcasing the mutate block with documentation links
blockr.core::serve(
  blockr.md::new_md_board(
    blocks = c(
      # Source dataset
      data = new_dataset_block(dataset = "mtcars"),

      # Mutate: Create new columns and modify existing ones
      enhanced = new_mutate_block(
        exprs = list(
          # Calculate efficiency ratio
          efficiency = "mpg / wt",
          # Categorize cars by cylinder count
          engine_size = "dplyr::case_when(cyl <= 4 ~ 'Small', cyl == 6 ~ 'Medium', TRUE ~ 'Large')",
          # Create a power-to-weight ratio
          power_ratio = "hp / wt * 0.01",
          # Round mpg for readability
          mpg_rounded = "round(mpg, 1)"
        ),
        by = "cyl"
      ),

      # Select to show the results
      results = new_select_block(
        columns = c(
          "mpg",
          "mpg_rounded",
          "wt",
          "hp",
          "efficiency",
          "power_ratio",
          "engine_size"
        )
      ),

      # Arrange to see most efficient cars first
      sorted = new_arrange_block(
        columns = list(
          list(column = "efficiency", direction = "desc")
        )
      )
    ),
    links = c(
      new_link("data", "enhanced", "data"),
      new_link("enhanced", "results", "data"),
      new_link("results", "sorted", "data")
    ),
    document = c(
      "## Mutate Block Demo\n\n",
      "This workflow demonstrates the `new_mutate_block()` with documentation links.\n\n",

      "### Click the links in the Mutate Block!\n\n",
      "- **Documentation** link - Takes you to the showcase article\n",
      "- **Expression helpers guide** link (lightbulb icon) - Shows common functions like `case_when()`, `if_else()`, `lag()`, etc.\n\n",

      "### What this workflow does:\n\n",
      "1. Loads the mtcars dataset\n",
      "2. **Creates new columns** using mutate:\n",
      "   - `efficiency`: MPG per unit weight (mpg / wt)\n",
      "   - `engine_size`: Categorical variable based on cylinders\n",
      "   - `power_ratio`: Horsepower per unit weight\n",
      "   - `mpg_rounded`: Rounded miles per gallon\n",
      "3. **Selects** relevant columns to display\n",
      "4. **Arranges** by efficiency (most efficient first)\n\n",

      "### Try these features:\n\n",
      "- **Edit expressions**: Click into any expression field\n",
      "- **Autocomplete**: Press Ctrl+Space to see available columns and functions\n",
      "- **Add columns**: Use the + button to add more mutations\n",
      "- **Remove columns**: Use the - button to remove expressions\n",
      "- **Click the documentation links** at the top of the block!\n\n",

      "## Results\n\n",
      "![](blockr://sorted)\n\n"
    )
  )
)
