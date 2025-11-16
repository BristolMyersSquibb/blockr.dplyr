# Pivot Wider block constructor

This block reshapes data from long to wide format by pivoting column
values into new columns (see
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)).
This is the inverse operation of pivot_longer.

## Usage

``` r
new_pivot_wider_block(
  names_from = character(),
  values_from = character(),
  id_cols = character(),
  values_fill = "",
  names_sep = "_",
  names_prefix = "",
  ...
)
```

## Arguments

- names_from:

  Character vector specifying which column(s) to use for new column
  names. Can be a single column or multiple columns.

- values_from:

  Character vector specifying which column(s) to use for cell values.
  Can be a single column or multiple columns.

- id_cols:

  Character vector of columns that uniquely identify each row. If empty
  (default), uses all columns not specified in names_from or
  values_from.

- values_fill:

  Optional value to use for missing combinations. Can be a single value
  like "0" or "NA". Leave empty to keep missing values as NA.

- names_sep:

  Separator to use when names_from specifies multiple columns. Default
  is "\_".

- names_prefix:

  Optional prefix to add to all new column names.

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Value

A block object for pivot_wider operations

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html),
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)

## Examples

``` r
# Create a pivot wider block
new_pivot_wider_block()
#> <pivot_wider_block<transform_block<block>>>
#> Name: "Pivot wider"
#> Data inputs: "data"
#> Initial block state:
#>  $ names_from  : chr(0)
#>  $ values_from : chr(0)
#>  $ id_cols     : chr(0)
#>  $ values_fill : chr ""
#>  $ names_sep   : chr "_"
#>  $ names_prefix: chr ""
#> Constructor: blockr.dplyr::new_pivot_wider_block()

if (interactive()) {
  # Basic usage with long format data
  library(blockr.core)
  long_data <- data.frame(
    id = rep(1:3, each = 3),
    measurement_type = rep(c("a", "b", "c"), 3),
    value = c(10, 15, 12, 20, 25, 22, 30, 35, 32)
  )
  serve(
    new_pivot_wider_block(
      names_from = "measurement_type",
      values_from = "value"
    ),
    data = list(data = long_data)
  )

  # With values_fill to replace NAs
  serve(
    new_pivot_wider_block(
      names_from = "measurement_type",
      values_from = "value",
      values_fill = "0"
    ),
    data = list(data = long_data)
  )

  # With custom names_prefix
  serve(
    new_pivot_wider_block(
      names_from = "measurement_type",
      values_from = "value",
      names_prefix = "measure_"
    ),
    data = list(data = long_data)
  )
}
```
