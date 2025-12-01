# blockr.dplyr 0.1.0

Initial CRAN release.

## Features

### Core Data Transformation Blocks
- `new_select_block()`: Select columns with distinct option and drag-to-reorder
- `new_filter_block()`: Filter rows by selecting values from dropdowns
- `new_filter_expr_block()`: Filter rows using R expressions
- `new_arrange_block()`: Sort rows by multiple columns with ascending/descending options
- `new_slice_block()`: Select rows by position or value criteria
- `new_mutate_expr_block()`: Create or modify columns using expressions
- `new_rename_block()`: Rename columns interactively
- `new_summarize_block()`: Calculate summary statistics with optional grouping (no-code interface)
- `new_summarize_expr_block()`: Calculate summary statistics using R expressions

### Data Combination Blocks
- `new_join_block()`: Join two datasets (left, right, inner, full, semi, anti)
- `new_bind_rows_block()`: Stack datasets vertically
- `new_bind_cols_block()`: Combine datasets horizontally

### Data Reshaping Blocks
- `new_pivot_longer_block()`: Transform wide data to long format
- `new_pivot_wider_block()`: Transform long data to wide format
- `new_separate_block()`: Split a column into multiple columns
- `new_unite_block()`: Combine multiple columns into one

## Documentation
- Two comprehensive vignettes:
  - "blockr.dplyr Showcase: Data Wrangling Blocks"
  - "Working with Expressions: Helper Functions for Advanced Data Manipulation"
- Full documentation for all exported functions
- Package website with examples and screenshots
