# Load required libraries
library(blockr)
library(blockr.dag)
library(blockr.md)
library(blockr.dplyr)
pkgload::load_all()
library(tidyverse)

# Demo workflow for testing value filter with NAs and empty strings
run_app(
  blocks = c(
  
      # Source dataset - iris
      data = new_dataset_block(dataset = "iris"),

      # Mutate block to introduce NAs and empty strings
      mutate_test = new_mutate_block(
        exprs = list(
          # Change some numeric values to NA
          Sepal.Length = "ifelse(row_number() %% 10 == 0, NA_real_, Sepal.Length)",
          # Convert Species to character and add NAs and empty strings
          Species = "case_when(
            row_number() %% 15 == 0 ~ NA_character_,
            row_number() %% 12 == 0 ~ '',
            TRUE ~ as.character(Species)
          )"
        )
      ),

      # Filter to test how it handles NAs and empty strings
      test_filter = new_value_filter_block(
        conditions = list(
          list(
            column = "Species",
            values = c("setosa", "versicolor"),
            mode = "include"
          )
        )
      )
    ),
    links = c(
      # Connect mutate block
      new_link("data", "mutate_test", "data"),

      # Connect filter
      new_link("mutate_test", "test_filter", "data")
    ),
  extensions = list(

    new_dag_extension(),

    new_md_extension(

      content = c(
      "## Value Filter with NAs and Empty Strings Demo\n\n",
      "This workflow tests how the value filter block handles NA values and empty strings.\n\n",

      "### Original Data\n\n",
      "![](blockr://data)\n\n",

      "### Data with NAs and Empty Strings\n",
      "The mutate block introduces:\n",
      "- **Sepal.Length:** NA values every 10th row\n",
      "- **Species:** Converted to character with:\n",
      "  - NA values every 15th row\n",
      "  - Empty strings ('') every 12th row\n\n",
      "![](blockr://mutate_test)\n\n",

      "### Filtered Data\n",
      "Filter to include only 'setosa' and 'versicolor' species.\n",
      "This tests how the filter handles:\n",
      "- NA values in the Species column\n",
      "- Empty strings in the Species column\n",
      "- Regular values\n\n",
      "![](blockr://test_filter)\n\n"
      )
      )
  )
)
