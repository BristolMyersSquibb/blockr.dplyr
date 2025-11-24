# Unite block constructor

This block combines multiple columns into a single column by pasting
their values together (see
[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html)).
This is useful for creating composite identifiers or labels from
multiple fields.

## Usage

``` r
new_unite_block(
  col = "united",
  cols = character(),
  sep = "_",
  remove = TRUE,
  na.rm = FALSE,
  ...
)
```

## Arguments

- col:

  Name for the new united column. Default is "united".

- cols:

  Character vector of column names to unite together. If empty
  (default), all columns will be available for selection.

- sep:

  Separator to use between values. Default is "\_".

- remove:

  If TRUE (default), remove input columns from output data frame.

- na.rm:

  If TRUE, missing values will be removed prior to uniting each row.
  Default is FALSE.

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Value

A block object for unite operations

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html),
[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html)

## Examples

``` r
# Create a unite block
new_unite_block()
#> <unite_block<transform_block<block>>>
#> Name: "Unite"
#> Data inputs: "data"
#> Initial block state:
#>  $ col   : chr "united"
#>  $ cols  : chr(0)
#>  $ sep   : chr "_"
#>  $ remove: logi TRUE
#>  $ na.rm : logi FALSE
#> Constructor: blockr.dplyr::new_unite_block()

if (interactive()) {
  # Basic usage - combine first and last name
  library(blockr.core)
  people_data <- data.frame(
    first_name = c("John", "Jane", "Bob"),
    last_name = c("Doe", "Smith", "Johnson"),
    age = c(30, 25, 35)
  )
  serve(
    new_unite_block(
      col = "full_name",
      cols = c("first_name", "last_name"),
      sep = " "
    ),
    data = list(data = people_data)
  )

  # With custom separator
  serve(
    new_unite_block(
      col = "id",
      cols = c("first_name", "last_name"),
      sep = "-",
      remove = TRUE
    ),
    data = list(data = people_data)
  )

  # With NA removal
  data_with_na <- data.frame(
    prefix = c("Dr.", NA, "Prof."),
    first = c("John", "Jane", "Bob"),
    last = c("Doe", "Smith", "Johnson")
  )
  serve(
    new_unite_block(
      col = "full_name",
      cols = c("prefix", "first", "last"),
      sep = " ",
      na.rm = TRUE
    ),
    data = list(data = data_with_na)
  )
}
```
