
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
