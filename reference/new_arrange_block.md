# Arrange block constructor

This block allows allows you to order the rows of a data frame by the
values of selected columns (see
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)).

## Usage

``` r
new_arrange_block(columns = character(), ...)
```

## Arguments

- columns:

  Columns to arrange by. Can be a character vector (ascending order) or
  a list of specifications with column and direction.

- ...:

  Forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Examples

``` r
# Create an arrange block
new_arrange_block(columns = "mpg")
#> <arrange_block<transform_block<block>>>
#> Name: "Arrange"
#> Data inputs: "data"
#> Initial block state:
#>  $ columns: chr "mpg"
#> Constructor: blockr.dplyr::new_arrange_block()

if (interactive()) {
  library(blockr.core)
  library(blockr.dplyr)

  # Basic usage - single column ascending
  serve(new_arrange_block(columns = "mpg"), list(data = mtcars))

  # Multiple columns with custom directions
  serve(
    new_arrange_block(
      columns = list(
        list(column = "cyl", direction = "asc"),
        list(column = "mpg", direction = "desc")
      )
    ),
    list(data = mtcars)
  )

  # Connected blocks - sort after categorizing
  serve(
    new_board(
      blocks = list(
        data = new_dataset_block(dataset = "mtcars"),
        categorized = new_mutate_block(
          exprs = list(
            car_type = paste0(
              "dplyr::case_when(cyl <= 4 ~ 'Economy', ",
              "cyl <= 6 ~ 'Standard', TRUE ~ 'Performance')"
            )
          )
        ),
        sorted = new_arrange_block(
          columns = list(
            list(column = "car_type", direction = "asc"),
            list(column = "mpg", direction = "desc"),
            list(column = "hp", direction = "desc")
          )
        )
      ),
      links = links(
        from = c("data", "categorized"),
        to = c("categorized", "sorted")
      )
    )
  )
}
```
