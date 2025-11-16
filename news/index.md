# Changelog

## blockr.dplyr 0.1.0

Initial CRAN release.

### Features

#### Core Data Transformation Blocks

- [`new_select_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_select_block.md):
  Select columns with distinct option and drag-to-reorder
- [`new_filter_expr_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_filter_expr_block.md):
  Filter rows using R expressions
- `new_filter_value_block()`: Filter rows by selecting values from
  dropdowns
- [`new_arrange_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_arrange_block.md):
  Sort rows by multiple columns with ascending/descending options
- [`new_slice_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_slice_block.md):
  Select rows by position or value criteria
- [`new_mutate_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_mutate_block.md):
  Create or modify columns using expressions
- [`new_rename_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_rename_block.md):
  Rename columns interactively
- [`new_summarize_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_summarize_block.md):
  Calculate summary statistics with optional grouping

#### Data Combination Blocks

- [`new_join_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_join_block.md):
  Join two datasets (left, right, inner, full, semi, anti)
- [`new_bind_rows_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_bind_rows_block.md):
  Stack datasets vertically
- [`new_bind_cols_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_bind_cols_block.md):
  Combine datasets horizontally

#### Data Reshaping Blocks

- [`new_pivot_longer_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_pivot_longer_block.md):
  Transform wide data to long format
- [`new_pivot_wider_block()`](https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_pivot_wider_block.md):
  Transform long data to wide format

### Documentation

- Two comprehensive vignettes:
  - “blockr.dplyr Showcase: Data Wrangling Blocks”
  - “Working with Expressions: Helper Functions for Advanced Data
    Manipulation”
- Full documentation for all exported functions
- Package website with examples and screenshots
