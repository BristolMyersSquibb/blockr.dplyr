# Filter block constructor

This block allows filtering rows in a data frame by selecting specific
values from columns (see
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)).
Provides a visual interface where users can select columns and choose
which values to include or exclude without writing R expressions.
Supports multiple conditions with AND/OR logic.

## Usage

``` r
new_filter_block(conditions = list(), preserve_order = FALSE, ...)
```

## Arguments

- conditions:

  List of filter conditions. Each condition should be a list with
  elements: column (character), values (vector), mode ("include" or
  "exclude"), and optionally operator ("&" or "\|") specifying how this
  condition connects to the previous one

- preserve_order:

  Logical. If TRUE, preserves the order of selected values in the
  filtered output (default: FALSE)

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for value-based filter operations

## Details

For expression-based filtering, see
[`new_filter_expr_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_filter_expr_block.md).

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Examples

``` r
# Create a filter block
new_filter_block()
#> <filter_block<transform_block<block>>>
#> Name: "Filter"
#> Data inputs: "data"
#> Initial block state:
#>  $ conditions    : list()
#>  $ preserve_order: logi FALSE
#> Constructor: blockr.dplyr::new_filter_block()

if (interactive()) {
  # Basic usage with mtcars dataset
  library(blockr.core)
  serve(new_filter_block(), list(data = mtcars))

  # With initial condition
  serve(new_filter_block(
    conditions = list(
      list(column = "cyl", values = c(4, 6), mode = "include")
    )
  ), list(data = mtcars))

  # Connected blocks example
  serve(
    new_board(
      blocks = list(
        a = new_dataset_block(),
        b = new_filter_block()
      ),
      links = links(
        from = c("a"),
        to = c("b")
      )
    )
  )
}
```
