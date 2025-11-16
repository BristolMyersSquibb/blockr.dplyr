# Expression filter block constructor

This block allows filtering rows in a data frame based on R expressions
(see
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)).
Supports multiple conditions with AND/OR logic. Changes are applied
after clicking the submit button.

## Usage

``` r
new_filter_expr_block(exprs = "TRUE", ...)
```

## Arguments

- exprs:

  Reactive expression returning character vector of filter conditions
  (default: "TRUE" for no filtering)

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for expression-based filter operations

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Examples

``` r
# Create a filter block
new_filter_expr_block("mpg > 20")
#> <filter_expr_block<transform_block<block>>>
#> Name: "Filter expr"
#> Data inputs: "data"
#> Initial block state:
#>  $ exprs: chr "mpg > 20"
#> Constructor: blockr.dplyr::new_filter_expr_block()

if (interactive()) {
  # Basic usage with mtcars dataset
  library(blockr.core)
  serve(new_filter_expr_block(), list(data = mtcars))

  # With custom initial condition
  serve(new_filter_expr_block("mpg > 20"), list(data = mtcars))

  # Connected blocks example
  serve(
    new_board(
      blocks = list(
        a = new_dataset_block(),
        b = new_filter_expr_block()
      ),
      links = links(
        from = c("a"),
        to = c("b")
      )
    )
  )
}
```
