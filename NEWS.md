# blockr.dplyr 0.2.0

## Breaking changes

- All block UIs rewritten as JS-driven components for instant feedback.
- `new_filter_expr_block()`, `new_mutate_expr_block()` and
  `new_summarize_expr_block()` are removed: filter, mutate and summarize
  now each unify no-code and expression modes in a single UI. Boards
  saved with old versions still restore (state is migrated on load);
  code calling the removed constructors must switch to the unified
  blocks with an `expr`-type entry in `state`.
- An unconfigured filter or summarize block now passes data through
  unchanged. Previously an empty filter emitted `dplyr::filter(., TRUE)`
  and an empty summarize collapsed the data to a single row.

## New features

- Filter block supports value, numeric, and expression conditions, plus
  a `preserveOrder` option. Column values load lazily on dropdown-open,
  so large data arrives instantly.
- Summarize block extensible via the `blockr.dplyr.summary_functions`
  option.
- Filter conditions carry the column type, so values picked from
  character columns that look numeric (subject IDs like `"007"`) filter
  correctly instead of silently matching nothing.

## Bug fixes and internals

- Join expressions are built as language objects; column names or
  suffixes containing quotes no longer break the join (previously this
  silently fell back to a plain `left_join()`).
- Factor columns keep their level order in filter value dropdowns.
- Shared R and JS factories (`new_js_transform_block()`,
  `Blockr.registerBlock()`) replace the per-block boilerplate; removed
  blocks no longer leak document-level event listeners.
- The JS layer is type-checked (JSDoc + TypeScript, no build step);
  `inst/js/types.d.ts` documents the R/JS protocol.

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
