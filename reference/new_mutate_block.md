# Mutate block constructor

This block allows to add new variables and preserve existing ones (see
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)).
Changes are applied after clicking the submit button.

## Usage

``` r
new_mutate_block(exprs = list(new_col = "1"), by = character(), ...)
```

## Arguments

- exprs:

  Reactive expression returning character vector of expressions

- by:

  Character vector of column names for grouping. Default is empty.

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for mutate operations

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Examples

``` r
# Create a mutate block
new_mutate_block(exprs = list(mpg_squared = "mpg^2"))
#> <mutate_block<transform_block<block>>>
#> Name: "Mutate"
#> Data inputs: "data"
#> Initial block state:
#>  $ exprs:List of 1
#>   ..$ mpg_squared: chr "mpg^2"
#>  $ by   : chr(0)
#> Constructor: blockr.dplyr::new_mutate_block()

if (interactive()) {
  # Basic usage with mtcars datase
  library(blockr.core)
  serve(new_mutate_block(), data = list(data = mtcars))

  # With a custom dataset
  df <- data.frame(x = 1:5, check.names = FALSE)
  df$`2025 Sales` <- letters[1:5]
  serve(new_mutate_block(), data = list(data = df))
}
```
