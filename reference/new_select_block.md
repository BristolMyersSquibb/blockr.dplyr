# Select block constructor

This block allows performing column subsetting on `data.frame` objects
(see
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)).
Columns can be selected and reordered by dragging, and an exclude mode
allows for negative selection using dplyr's minus syntax. Optionally,
distinct rows can be kept after selection.

## Usage

``` r
new_select_block(columns = character(), exclude = FALSE, distinct = FALSE, ...)
```

## Arguments

- columns:

  Selected columns (character vector). If empty, selects all columns.

- exclude:

  Logical. If TRUE, uses exclude mode (dplyr minus syntax:
  `-c(col1, col2)`). If FALSE (default), uses include mode (selects
  specified columns).

- distinct:

  Logical. If TRUE, keeps only distinct/unique rows after selecting
  columns. If FALSE (default), returns all rows. This replaces the old
  `new_distinct_block()` functionality.

- ...:

  Forwarded to
  [`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Details

**Note**: This block replaces the deprecated `new_distinct_block()`. Use
the `distinct` parameter to get unique rows after column selection.

The select block provides a sortable multi-select interface where
columns can be:

- Selected/deselected by clicking

- Reordered by dragging (order is preserved in output)

- Removed individually using the × button

**Include mode (exclude = FALSE, default):**

- Selected columns are included in output

- Empty selection = select all (`select(data, dplyr::everything())`)

**Exclude mode (exclude = TRUE):**

- Selected columns are excluded from output using minus syntax

- Empty selection = select all (`select(data)`)

- Efficient for large datasets when you want to remove just a few
  columns

**Distinct mode (distinct = TRUE):**

- Keeps only distinct rows after column selection

- Equivalent to piping `select()` output to `distinct()`

- Useful for finding unique combinations of selected columns

## Examples

``` r
# Create a select block
new_select_block(columns = c("mpg", "cyl", "hp"))
#> <select_block<transform_block<block>>>
#> Name: "Select"
#> Data inputs: "data"
#> Initial block state:
#>  $ columns : chr [1:3] "mpg" "cyl" "hp"
#>  $ exclude : logi FALSE
#>  $ distinct: logi FALSE
#> Constructor: blockr.dplyr::new_select_block()

if (interactive()) {
  # Basic usage with mtcars dataset
  library(blockr.core)
  serve(new_select_block(), list(data = mtcars))

  # With initial column selection
  serve(new_select_block(columns = c("mpg", "cyl", "hp")), list(data = mtcars))

  # Exclude mode (select all except specified columns)
  serve(new_select_block(columns = c("gear", "carb"), exclude = TRUE), list(data = mtcars))

  # Select with distinct (unique combinations)
  serve(new_select_block(columns = c("cyl", "gear"), distinct = TRUE), list(data = mtcars))

  # Full deduplication (distinct on all columns)
  serve(new_select_block(distinct = TRUE), list(data = mtcars))

  # Connected blocks example
  serve(
    new_board(
      blocks = list(
        a = new_dataset_block(),
        b = new_select_block()
      ),
      links = links(
        from = c("a"),
        to = c("b")
      )
    )
  )
}
```
