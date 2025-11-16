# Summarize no-code block constructor

This block provides a no-code interface for summarizing data (see
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)).
Instead of writing expressions, users select summary functions from
dropdowns (mean, median, sum, etc.), choose columns to summarize, and
specify new column names.

## Usage

``` r
new_summarize_nocode_block(
  summaries = list(count = list(func = "dplyr::n", col = "")),
  by = character(),
  ...
)
```

## Arguments

- summaries:

  Named list where each element is a list with 'func' and 'col'
  elements. For example: list(avg_mpg = list(func = "mean", col =
  "mpg"))

- by:

  Columns to define grouping

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A block object for no-code summarize operations

## Extending available functions

The list of available summary functions can be extended using the
`blockr.dplyr.summary_functions` option. Set this option to a named
character vector where names are display labels and values are function
calls:

    options(
      blockr.dplyr.summary_functions = c(
        "extract parentheses (paren_num)" = "blockr.topline::paren_num"
      )
    )

If a description is not provided (empty name), the function name will be
used as the display label.

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html),
[`new_summarize_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_summarize_block.md)

## Examples

``` r
# Create a summarize no-code block
new_summarize_nocode_block()
#> <summarize_nocode_block<transform_block<block>>>
#> Name: "Summarize nocode"
#> Data inputs: "data"
#> Initial block state:
#>  $ summaries:List of 1
#>   ..$ count:List of 2
#>   .. ..$ func: chr "dplyr::n"
#>   .. ..$ col : chr ""
#>  $ by       : chr(0)
#> Constructor: blockr.dplyr::new_summarize_nocode_block()

if (interactive()) {
  # Basic usage with mtcars dataset
  library(blockr.core)
  serve(new_summarize_nocode_block(), data = list(data = mtcars))

  # With predefined summaries
  serve(
    new_summarize_nocode_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg"),
        max_hp = list(func = "max", col = "hp")
      )
    ),
    data = list(data = mtcars)
  )

  # With grouping
  serve(
    new_summarize_nocode_block(
      summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
      by = "cyl"
    ),
    data = list(data = mtcars)
  )
}
```
