# Summarize expression block constructor

This block allows to add new variables by summarizing over groups using
R expressions (see
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)).
Changes are applied after clicking the submit button.

## Usage

``` r
new_summarize_expr_block(
  exprs = list(count = "dplyr::n()"),
  by = character(),
  unpack = FALSE,
  ...
)
```

## Arguments

- exprs:

  Reactive expression returning character vector of expressions

- by:

  Columns to define grouping

- unpack:

  Logical flag to unpack data frame columns from helper functions. When
  `TRUE`, expressions that return data frames will have their columns
  unpacked into separate columns. When `FALSE`, data frames are kept as
  nested list-columns. Default is `FALSE`.

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for summarize operations

## Details

For no-code summarization using dropdowns, see
[`new_summarize_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_summarize_block.md).

## Unpacking Helper Function Results

When `unpack = TRUE`, helper functions that return data frames will have
their columns unpacked into separate columns in the result. This is
useful for helper functions like `stat_label()` that return multiple
statistics as columns.

    # Without unpacking (default)
    new_summarize_expr_block(
      exprs = list(stats = "helper_func(...)"),
      unpack = FALSE
    )
    # Result: Creates nested list-column "stats" containing the data frame

    # With unpacking
    new_summarize_expr_block(
      exprs = list(stats = "helper_func(...)"),
      unpack = TRUE
    )
    # Result: Columns from helper_func() are unpacked into separate columns

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Examples

``` r
# Create a summarize expression block
new_summarize_expr_block()
#> <summarize_expr_block<transform_block<block>>>
#> Name: "Summarize expr"
#> Data inputs: "data"
#> Initial block state:
#>  $ exprs :List of 1
#>   ..$ count: chr "dplyr::n()"
#>  $ by    : chr(0)
#>  $ unpack: logi FALSE
#> Constructor: blockr.dplyr::new_summarize_expr_block()

if (interactive()) {
  # Basic usage with mtcars dataset
  library(blockr.core)
  serve(new_summarize_expr_block(), list(data = mtcars))

  # With a custom dataset
  df <- data.frame(x = 1:5, y = letters[1:5])
  serve(new_summarize_expr_block(), list(data = df))

  # Using unpack to expand helper function results
  # Define the helper in your environment first
  calc_stats <- function(df) {
    data.frame(
      mean_x = mean(df$x),
      mean_y = mean(df$y),
      sum_x = sum(df$x),
      sum_y = sum(df$y)
    )
  }

  # With unpacking enabled
  serve(
    new_summarize_expr_block(
      exprs = list(stats = "calc_stats(pick(everything()))"),
      by = "group",
      unpack = TRUE
    ),
    list(data = data.frame(x = 1:6, y = 10:15, group = rep(c("A", "B"), 3)))
  )
  # Result: group, mean_x, mean_y, sum_x, sum_y (columns unpacked)
}
```
