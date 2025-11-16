# Slice block constructor

This block allows row selection using various dplyr slice functions (see
[`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_head()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_max()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)).
Features reactive UI with immediate updates and comprehensive grouping
support.

## Usage

``` r
new_slice_block(
  type = "head",
  n = 5,
  prop = NULL,
  order_by = character(),
  with_ties = TRUE,
  weight_by = character(),
  replace = FALSE,
  rows = "1:5",
  by = character(),
  ...
)
```

## Arguments

- type:

  Character string specifying slice type: "head", "tail", "min", "max",
  "sample", or "custom"

- n:

  Number of rows to select (default: 5). Mutually exclusive with prop.

- prop:

  Proportion of rows to select (0 to 1, default: NULL). When specified,
  n is ignored.

- order_by:

  Column name to order by (for slice_min/slice_max)

- with_ties:

  Logical, whether to include ties (for slice_min/slice_max)

- weight_by:

  Column name for weighted sampling (for slice_sample)

- replace:

  Logical, whether to sample with replacement (for slice_sample)

- rows:

  Custom row positions (for slice)

- by:

  Character vector of column names for grouping

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for slice operations

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Examples

``` r
# Create a slice block
new_slice_block(type = "head", n = 5)
#> <slice_block<transform_block<block>>>
#> Name: "Slice"
#> Data inputs: "data"
#> Initial block state:
#>  $ type     : chr "head"
#>  $ n        : num 5
#>  $ prop     : NULL
#>  $ order_by : chr(0)
#>  $ with_ties: logi TRUE
#>  $ weight_by: chr(0)
#>  $ replace  : logi FALSE
#>  $ rows     : chr "1:5"
#>  $ by       : chr(0)
#> Constructor: blockr.dplyr::new_slice_block()

if (interactive()) {
  # Basic usage
  library(blockr.core)
  serve(new_slice_block(), list(data = mtcars))

  # Select first 5 rows
  serve(new_slice_block(type = "head", n = 5), list(data = mtcars))

  # Select rows with highest mpg values
  serve(new_slice_block(type = "max", order_by = "mpg", n = 3), list(data = mtcars))

  # Random sampling
  serve(new_slice_block(type = "sample", n = 10, replace = FALSE), list(data = mtcars))
}
```
