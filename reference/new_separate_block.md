# Separate block constructor

This block separates a single character column into multiple columns by
splitting on a separator pattern (see
[`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html)).
This is the inverse operation of unite.

## Usage

``` r
new_separate_block(
  col = character(),
  into = c("col1", "col2"),
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
  ...
)
```

## Arguments

- col:

  Character string specifying which column to separate. If empty
  (default), all columns will be available for selection.

- into:

  Character vector of names for the new columns. Can be specified as a
  character vector or a comma-separated string (e.g., "col1, col2,
  col3"). Default is c("col1", "col2").

- sep:

  Separator between columns. Can be a regular expression or numeric
  positions. Default is `"[^[:alnum:]]+"` (any non-alphanumeric
  character).

- remove:

  If TRUE (default), remove input column from output data frame.

- convert:

  If TRUE, will run type.convert() with as.is = TRUE on new columns.
  Default is FALSE.

- extra:

  How to handle extra pieces when there are too many: "warn" (default),
  "drop", or "merge".

- fill:

  How to handle missing pieces when there are too few: "warn" (default),
  "right", or "left".

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Value

A block object for separate operations

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html),
[`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html)

## Examples

``` r
# Create a separate block
new_separate_block()
#> <separate_block<transform_block<block>>>
#> Name: "Separate"
#> Data inputs: "data"
#> Initial block state:
#>  $ col    : chr(0)
#>  $ into   : chr [1:2] "col1" "col2"
#>  $ sep    : chr "[^[:alnum:]]+"
#>  $ remove : logi TRUE
#>  $ convert: logi FALSE
#>  $ extra  : chr "warn"
#>  $ fill   : chr "warn"
#> Constructor: blockr.dplyr::new_separate_block()

if (interactive()) {
  # Basic usage - separate full name into first and last
  library(blockr.core)
  people_data <- data.frame(
    full_name = c("John Doe", "Jane Smith", "Bob Johnson"),
    age = c(30, 25, 35)
  )
  serve(
    new_separate_block(
      col = "full_name",
      into = c("first_name", "last_name"),
      sep = " "
    ),
    data = list(data = people_data)
  )

  # Separate date components
  date_data <- data.frame(
    date_string = c("2024-01-15", "2024-02-20", "2024-03-25")
  )
  serve(
    new_separate_block(
      col = "date_string",
      into = c("year", "month", "day"),
      sep = "-",
      convert = TRUE
    ),
    data = list(data = date_data)
  )

  # Using regex separator
  mixed_data <- data.frame(
    mixed_col = c("a-b", "c_d", "e.f")
  )
  serve(
    new_separate_block(
      col = "mixed_col",
      into = c("col1", "col2"),
      sep = "[-_.]"
    ),
    data = list(data = mixed_data)
  )
}
```
