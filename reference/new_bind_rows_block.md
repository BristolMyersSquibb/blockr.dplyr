# Bind Rows Block Constructor

This block allows for row-wise combination of two or more data frames
using
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
It stacks data frames vertically, matching columns by name and filling
missing columns with NA values.

## Usage

``` r
new_bind_rows_block(id_name = "", ...)
```

## Arguments

- id_name:

  Character string, name for the ID column. If non-empty, adds a column
  identifying source data frames. Default "" (disabled).

- ...:

  Forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for bind_rows operations

## Examples

``` r
# Create a bind rows block
new_bind_rows_block()
#> <bind_rows_block<transform_block<block>>>
#> Name: "Bind rows"
#> Indefinite arity
#> Initial block state:
#>  $ id_name: chr ""
#> Constructor: blockr.dplyr::new_bind_rows_block()

if (interactive()) {
  library(blockr.core)
  library(blockr.dplyr)

  # Basic usage - stack filtered datasets
  serve(
    new_board(
      blocks = list(
        iris_data = new_dataset_block(dataset = "iris"),
        setosa = new_filter_expr_block(exprs = list("Species == 'setosa'")),
        versicolor = new_filter_expr_block(exprs = list("Species == 'versicolor'")),
        combined = new_bind_rows_block()
      ),
      links = links(
        from = c("iris_data", "iris_data", "setosa", "versicolor"),
        to = c("setosa", "versicolor", "combined", "combined"),
        input = c("data", "data", "1", "2")
      )
    )
  )

  # With ID column to track source
  serve(
    new_board(
      blocks = list(
        iris_data = new_dataset_block(dataset = "iris"),
        setosa = new_filter_expr_block(exprs = list("Species == 'setosa'")),
        versicolor = new_filter_expr_block(exprs = list("Species == 'versicolor'")),
        combined = new_bind_rows_block(id_name = "source")
      ),
      links = links(
        from = c("iris_data", "iris_data", "setosa", "versicolor"),
        to = c("setosa", "versicolor", "combined", "combined"),
        input = c("data", "data", "1", "2")
      )
    )
  )
}
```
