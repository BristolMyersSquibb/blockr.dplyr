# Bind Columns Block Constructor

This block allows for column-wise combination of two or more data frames
using
[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html).
It combines data frames side-by-side, requiring them to have the same
number of rows. Duplicate column names are automatically handled by
dplyr.

## Usage

``` r
new_bind_cols_block(...)
```

## Arguments

- ...:

  Forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for bind_cols operations

## Examples

``` r
# Create a bind cols block
new_bind_cols_block()
#> <bind_cols_block<transform_block<block>>>
#> Name: "Bind cols"
#> Indefinite arity
#> Stateless block
#> Constructor: blockr.dplyr::new_bind_cols_block()

if (interactive()) {
  library(blockr.core)
  library(blockr.dplyr)

  # Basic usage - combine different datasets horizontally
  serve(
    new_board(
      blocks = list(
        iris_data = new_dataset_block(dataset = "iris"),
        mtcars_data = new_dataset_block(dataset = "mtcars"),
        head1 = new_slice_block(type = "head", n = 5),
        head2 = new_slice_block(type = "head", n = 5),
        combined = new_bind_cols_block()
      ),
      links = links(
        from = c("iris_data", "mtcars_data", "head1", "head2"),
        to = c("head1", "head2", "combined", "combined"),
        input = c("data", "data", "1", "2")
      )
    )
  )

  # Combine selected columns from same dataset
  serve(
    new_board(
      blocks = list(
        mtcars_data = new_dataset_block(dataset = "mtcars"),
        engine_cols = new_select_block(columns = c("mpg", "cyl", "hp")),
        weight_cols = new_select_block(columns = c("wt", "qsec")),
        combined = new_bind_cols_block()
      ),
      links = links(
        from = c("mtcars_data", "mtcars_data", "engine_cols", "weight_cols"),
        to = c("engine_cols", "weight_cols", "combined", "combined"),
        input = c("data", "data", "1", "2")
      )
    )
  )
}
```
