
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Data transformation for `blockr.core`

<!-- badges: start -->

[![check](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml)
<!-- badges: end -->

Extending `blockr.core` with blocks for data wrangling, `blockr.dplyr`
wraps several `dplyr` verbs.

## Installation

You can install the development version of blockr.dplyr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.dplyr")
```

## Example

A `dplyr::select()` block can be spun up as

``` r
library(blockr.dplyr)
blockr.core::serve(new_select_block(c("mpg", "cyl")), data = list(data = mtcars))
```

A `dplyr::mutate()` block can be used to add new variables:

``` r
library(blockr.dplyr)
blockr.core::serve(new_mutate_block(), data = list(data = mtcars))
```

And a two-table verb such as `dplyr::join()` is available as

``` r
library(blockr.dplyr)
blockr.core::serve(
  new_join_block(by = "name"),
  data = list(x = dplyr::band_members, y = dplyr::band_instruments)
)
```

## Summarize Example

The `dplyr::summarize()` block supports both simple aggregations and
grouped summaries using the built-in `by` parameter:

### Standalone usage

``` r
library(blockr.dplyr)

# Basic summarization with grouping
blockr.core::serve(
  new_summarize_block(
    string = list(
      mean_mpg = "mean(mpg)",
      max_hp = "max(hp)",
      count = "n()"
    ),
    by = c("cyl", "am")
  ),
  data = list(data = mtcars)
)
```

### Full dashboard example

Create a complete data analysis workflow by connecting multiple blocks:

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard with connected blocks
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    filter_block = new_filter_block("mpg > 20"),
    summary_block = new_summarize_block(
      string = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)", 
        count = "n()"
      ),
      by = c("cyl")
    )
  ),
  links = links(
    from = c("data_block", "filter_block"),
    to = c("filter_block", "summary_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

This creates a visual pipeline where: 1. Data is loaded from the mtcars
dataset 2. Rows are filtered to only include cars with mpg \> 20 3.
Results are summarized by cylinder count, calculating average mpg,
average horsepower, and row count

## Mutate Example with Multiple Expressions

The `dplyr::mutate()` block now supports multiple expressions that can
be added/removed dynamically in the UI:

### Full dashboard with mutate block

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard with mutate block showing multiple expressions
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    mutate_block = new_mutate_block(
      string = list(
        mpg_double = "mpg * 2",
        hp_per_100 = "hp / 100", 
        power_to_weight = "hp / wt",
        efficiency_score = "mpg / hp * 100"
      )
    )
  ),
  links = links(
    from = c("data_block"),
    to = c("mutate_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

This creates a focused demonstration of the mutate block’s new
capabilities: - **Data loading**: Load the mtcars dataset - **Multiple
mutations**: Create 4 new columns simultaneously in one block

The mutate block UI allows you to: - Add new expressions with the "Add
Expression" button - Remove expressions (keeping at least one) - Edit
column names and expressions with syntax highlighting - See real-time
results as you modify the expressions

## Enhanced Filter Block with Multi-Condition Support

The `dplyr::filter()` block now supports multiple conditions with visual AND/OR logic builders:

### Simple single condition (classic mode)

``` r
library(blockr.dplyr)
# Single condition interface (backward compatible)
blockr.core::serve(
  new_filter_block("mpg > 20", multi_condition = FALSE), 
  data = list(data = mtcars)
)
```

### Multi-condition interface (new default)

``` r
library(blockr.dplyr)
# Multi-condition interface with visual builder
blockr.core::serve(
  new_filter_block("mpg > 20"), 
  data = list(data = mtcars)
)
```

### Complete data pipeline with enhanced filter

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard demonstrating the enhanced filter capabilities
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    filter_block = new_filter_block("mpg > 20"),  # Will use multi-condition UI
    mutate_block = new_mutate_block(
      string = list(
        efficiency = "mpg / hp",
        power_ratio = "hp / wt"
      )
    ),
    summary_block = new_summarize_block(
      string = list(
        avg_efficiency = "mean(efficiency)",
        max_power_ratio = "max(power_ratio)",
        count = "n()"
      ),
      by = c("cyl")
    )
  ),
  links = links(
    from = c("data_block", "filter_block", "mutate_block"),
    to = c("filter_block", "mutate_block", "summary_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

The enhanced filter block features:
- **Multi-condition interface**: Add/remove filter conditions with "Add Condition" button
- **AND/OR logic**: Choose between AND (&) or OR (|) operators between conditions
- **Full autocompletion**: Column names and dplyr functions suggested as you type
- **Real-time validation**: Immediate feedback on filter syntax and results
- **Backward compatibility**: Use `multi_condition = FALSE` for single-condition mode

### Testing multi-condition functionality

To quickly test the multi-condition features:

``` r
library(blockr.dplyr)

# Test the multi-filter module directly
run_multi_filter_example()

# Test in context with sample data
blockr.core::serve(new_filter_block(), data = list(data = mtcars))
```

In the UI, you can:
1. Start with one condition (e.g., "mpg > 20")
2. Click "Add Condition" to add more (e.g., "cyl == 4")
3. Choose AND/OR between conditions using the dropdown
4. Use autocompletion for column names and functions
5. Remove conditions with the trash icon (keeping at least one)
