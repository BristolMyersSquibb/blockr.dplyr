# Join block constructor

This block allows for joining of two `data.frame` objects with advanced
multi-column support including same-name and different-name joins (see
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
etc.).

## Usage

``` r
new_join_block(type = character(), by = character(), ...)
```

## Arguments

- type:

  Join type (left_join, inner_join, right_join, full_join, semi_join,
  anti_join)

- by:

  Column(s) to join on - can be character vector for same-name joins or
  named list for different-name joins

- ...:

  Forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Examples

``` r
# Create a join block
new_join_block(type = "left_join")
#> <join_block<transform_block<block>>>
#> Name: "Join"
#> Data inputs: "x" and "y"
#> Initial block state:
#>  $ type: chr "left_join"
#>  $ by  : chr(0)
#> Constructor: blockr.dplyr::new_join_block()

if (interactive()) {
  library(blockr.core)
  library(blockr.dplyr)

  # Basic left join - automatically detects common columns
  serve(
    new_board(
      blocks = list(
        data1 = new_dataset_block(dataset = "band_members"),
        data2 = new_dataset_block(dataset = "band_instruments"),
        joined = new_join_block(type = "left_join")
      ),
      links = links(
        from = c("data1", "data2"),
        to = c("joined", "joined"),
        input = c("x", "y")
      )
    )
  )

  # Inner join with explicit join column
  serve(
    new_board(
      blocks = list(
        data1 = new_dataset_block(dataset = "band_members"),
        data2 = new_dataset_block(dataset = "band_instruments"),
        joined = new_join_block(type = "inner_join", by = "name")
      ),
      links = links(
        from = c("data1", "data2"),
        to = c("joined", "joined"),
        input = c("x", "y")
      )
    )
  )

  # Right join - keep all rows from right dataset
  serve(
    new_board(
      blocks = list(
        data1 = new_dataset_block(dataset = "band_members"),
        data2 = new_dataset_block(dataset = "band_instruments"),
        joined = new_join_block(type = "right_join")
      ),
      links = links(
        from = c("data1", "data2"),
        to = c("joined", "joined"),
        input = c("x", "y")
      )
    )
  )

  # Anti join - find rows in left without matches in right
  serve(
    new_board(
      blocks = list(
        data1 = new_dataset_block(dataset = "band_members"),
        data2 = new_dataset_block(dataset = "band_instruments"),
        joined = new_join_block(type = "anti_join")
      ),
      links = links(
        from = c("data1", "data2"),
        to = c("joined", "joined"),
        input = c("x", "y")
      )
    )
  )
}
```
