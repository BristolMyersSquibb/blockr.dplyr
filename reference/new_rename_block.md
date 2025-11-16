# Rename block constructor

This block allows renaming columns in a data frame using the visual
interface (see
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)).
Changes are applied after clicking the submit button. Uses new_name =
old_name syntax where new_name is what you want to call the column and
old_name is the current column name.

## Usage

``` r
new_rename_block(renames = list(new_col = character()), ...)
```

## Arguments

- renames:

  Named list or vector of renames in new_name = old_name format

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for rename operations

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Examples

``` r
# Create a rename block
new_rename_block(list(miles_per_gallon = "mpg", cylinders = "cyl"))
#> <rename_block<transform_block<block>>>
#> Name: "Rename"
#> Data inputs: "data"
#> Initial block state:
#>  $ renames:List of 2
#>   ..$ miles_per_gallon: chr "mpg"
#>   ..$ cylinders       : chr "cyl"
#> Constructor: blockr.dplyr::new_rename_block()

if (interactive()) {
  # Basic usage with mtcars dataset
  library(blockr.core)
  serve(new_rename_block(), data = list(data = mtcars))

  # With predefined renames
  serve(
    new_rename_block(list(miles_per_gallon = "mpg", cylinders = "cyl")),
    data = list(data = mtcars)
  )

  # Connected blocks example
  serve(
    new_board(
      blocks = list(
        a = new_dataset_block(),
        b = new_rename_block(list(horsepower = "hp"))
      ),
      links = links(
        from = c("a"),
        to = c("b")
      )
    )
  )
}
```
