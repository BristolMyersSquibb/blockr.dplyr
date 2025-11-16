# Pivot Longer block constructor

This block reshapes data from wide to long format by pivoting multiple
columns into two columns: one containing the original column names and
another containing the values (see
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)).

## Usage

``` r
new_pivot_longer_block(
  cols = character(),
  names_to = "name",
  values_to = "value",
  values_drop_na = FALSE,
  names_prefix = "",
  ...
)
```

## Arguments

- cols:

  Character vector of column names to pivot into longer format. If
  empty, all columns will be available for selection.

- names_to:

  Name of the new column to create from the column names. Default is
  "name".

- values_to:

  Name of the new column to create from the values. Default is "value".

- values_drop_na:

  If TRUE, rows with NA values will be dropped. Default is FALSE.

- names_prefix:

  Optional prefix to remove from column names before storing in the
  names_to column. For example, "col\_" would remove that prefix from
  column names like "col_a", "col_b".

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Value

A block object for pivot_longer operations

## See also

[`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html),
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)

## Examples

``` r
# Create a pivot longer block
new_pivot_longer_block()
#> <pivot_longer_block<transform_block<block>>>
#> Name: "Pivot longer"
#> Data inputs: "data"
#> Initial block state:
#>  $ cols          : chr(0)
#>  $ names_to      : chr "name"
#>  $ values_to     : chr "value"
#>  $ values_drop_na: logi FALSE
#>  $ names_prefix  : chr ""
#> Constructor: blockr.dplyr::new_pivot_longer_block()

if (interactive()) {
  # Basic usage with wide format data
  library(blockr.core)
  wide_data <- data.frame(
    id = 1:3,
    measurement_a = c(10, 20, 30),
    measurement_b = c(15, 25, 35),
    measurement_c = c(12, 22, 32)
  )
  serve(
    new_pivot_longer_block(
      cols = c("measurement_a", "measurement_b", "measurement_c"),
      names_to = "measurement_type",
      values_to = "value"
    ),
    data = list(data = wide_data)
  )

  # With names_prefix to clean column names
  serve(
    new_pivot_longer_block(
      cols = c("measurement_a", "measurement_b", "measurement_c"),
      names_to = "type",
      values_to = "measurement",
      names_prefix = "measurement_"
    ),
    data = list(data = wide_data)
  )
}
```
