# blockr.dplyr (development version)

## Bug fixes

- **Restored blocks came back pointing at the wrong columns**, or at none.
  A block read its column back out of the `Blockr.Select` widget after
  refreshing the widget's options, and stored whatever the widget said. A
  column picker has no `allowEmpty`, so an option list that does not carry the
  block's column slides the widget onto the first one — and during a restore
  that list arrives routinely, because an upstream that has not restored yet
  still reports the old frame's columns. The authored column was gone before
  the real one arrived, and nothing brought it back: `setState()` rebuilds from
  R's message, which does not repeat. Multi-column pickers lost their columns
  outright, since `setOptions()` filters the selection against the new list.
  Every block with a column picker was affected: `rename` renamed the wrong
  column, `filter` kept its values while re-pointing at another column,
  `join` joined on the wrong keys, `select`, `unite`, `pivot_longer`,
  `pivot_wider`, `slice` and `summarize` came back empty, and `arrange` and
  `separate` collapsed onto the first column. A block now keeps its own
  column: one the board restored or the user picked survives a list that does
  not offer it and lands back on its option when the real columns arrive,
  while one the select auto-picked still follows the data.
- Following from that, a column picker no longer drops a deliberate column
  just because the frame in front of it does not carry one by that name. This
  is what makes a restore survive an upstream that is still settling, and it
  cuts the other way too: when an upstream really has removed the column, the
  block holds on to it and fails loudly instead of quietly transforming a
  different one. Only a column the select auto-picked still follows the data.
- `summarize` and `join` auto-submit once they know their columns, and
  `summarize` and `mutate` did so again from `setState()`. On a restore that
  wrote the client's reading of the block over the state R had just sent it —
  `mutate`'s fired before the group-by had been restored, so a restored
  `mutate` lost its `.by` for good. These now speak only when they have
  something to say that R did not already tell them.
- R pushes a block's state before its columns, so a block that auto-submits on
  columns can no longer answer with a fabricated state before its restore has
  arrived.

- Every block came up **blank** — one empty row, no column choices — when its
  dock panel was not on the startup view, while its expression and everything
  downstream stayed correct. The block's server runs at boot (blockr.core
  builds it whenever a visible block downstream needs it) and pushes its state
  and columns before the panel, and with it this package's JavaScript, has been
  delivered; Shiny drops custom messages that have no handler yet, and the
  replay queue in `blockr-core.js` cannot catch a message dropped before its
  own script loaded. The binding now announces itself on bind when nothing was
  queued for it, and R re-sends state, columns, and `summarize`'s function
  list. Affected all 13 blocks. The core-level fix this stands in for is
  BristolMyersSquibb/blockr.core#317.
- `slice` with type `min`/`max` and no `order_by` raised
  `` `order_by` is absent but must be supplied `` instead of passing the data
  through. An unconfigured block is inert, never a red banner.
- `slice`'s `Order by` and `Weight by` pickers displayed the first column while
  the block's value was still `""`, so the block looked configured and was not.
  Both now pass `allowEmpty = TRUE` to `Blockr.Select`.
- Required-empty fields now render the trailing `*` on their label, which the
  design system has specified all along and the CSS never implemented.

- Clearing the `Separator` field in `unite`, `separate` or `pivot_wider` sent
  an empty string rather than the default the placeholder promised: `unite`
  pasted values with no separator, `separate` split between every character,
  and `pivot_wider` joined names with nothing. A cleared field now applies the
  default, matching the placeholder and the behaviour of the other text fields
  in those blocks (#0).

## Design-system alignment

- Placeholders now name the empty state on optional fields and prompt on
  required ones, and the trailing `…` marks the difference: `ID columns
  (optional)` reads `All other columns` (what tidyr actually does), `Group by:`
  reads `None`, `Weight by` reads `Unweighted`, while required pickers keep
  `Select columns…`.
- Removed the `title` tooltips that only restated their checkbox label
  (`Exclude selected`, `Remove source column`, `Remove source columns`).

# blockr.dplyr 0.2.0

## Design-system alignment

- Gear settings now expand a full-width, in-flow settings band between the
  gear header and the block content (pivot_wider, pivot_longer, join,
  slice) instead of a floating popover: content pushes down, the gear is
  the only toggle, and there is no outside-click dismissal. Band assets
  are vendored verbatim from blockr.viz (the canonical source until the
  shared layer moves to blockr.ui).
- On/off options are checkboxes instead of self-labeling toggle pills
  (design-system rule: values cycle as pills, data options are
  checkboxes): select exclude/distinct, separate remove/convert, unite
  remove/na_rm, filter "Keep pick order", pivot_longer drop-NA, slice
  keep-ties / sample-with-replacement. Cycling pills (AND/OR, asc/desc,
  n/%, join type, operators) are unchanged.
- Text and number inputs commit on Enter or blur with an "Enter ↵"
  confirm chip (armed → faded check; Escape reverts) instead of
  auto-submitting on a 300ms debounce; discrete controls (selects, pills,
  checkboxes) now submit immediately.
- Required-but-empty fields show a soft amber cue
  (`.blockr-field--required-empty`): pivot_longer columns, pivot_wider
  names/values-from, separate column/into, unite columns, slice
  order-by (min/max) and custom row positions.
- Token unification: one focus ring (blue-600 `--blockr-focus-ring`),
  danger fallback `#dc2626`, mono font behind `--blockr-font-mono`;
  the summarize block's CSS prefix is now `smb-` (collision with the
  select block's `sb-`); the orphaned `filter-unified.css` is removed.

## Breaking changes

- All block UIs rewritten as JS-driven components for instant feedback.
- Block constructors now take flat, per-field arguments instead of a single
  `state = list(...)` argument (e.g.
  `new_filter_block(conditions = ..., operator = ...)` rather than
  `new_filter_block(state = list(conditions = ..., operator = ...))`). This
  surfaces every field directly to the assistant and to external control.
  Boards saved with the old single-`state` format still restore; code
  passing `state = list(...)` must switch to the individual arguments.
  The registry assistant metadata (`arguments` descriptions + `examples`)
  was flattened to match: each field is now keyed by its constructor
  argument name instead of nested under `state`, so the per-block AI sees
  accurate parameter docs and canonical example shapes.
- `new_filter_expr_block()`, `new_mutate_expr_block()` and
  `new_summarize_expr_block()` are removed: filter, mutate and summarize
  now each unify no-code and expression modes in a single UI. Boards
  saved with old versions still restore (state is migrated on load);
  code calling the removed constructors must switch to the unified
  blocks with an `expr`-type entry in `conditions`/`mutations`/`summaries`.
- An unconfigured filter or summarize block now passes data through
  unchanged. Previously an empty filter emitted `dplyr::filter(., TRUE)`
  and an empty summarize collapsed the data to a single row.

## New features

- Filter block supports value, numeric, and expression conditions, plus
  a `preserve_order` option. Column values load lazily on dropdown-open,
  so large data arrives instantly.
- High-cardinality columns use server-side value search: the dropdown
  shows the first page plus a total count, and typing queries R for
  matching values instead of shipping the full unique-value list to the
  browser. Threshold via `options(blockr.dplyr.max_filter_values = )`
  (default 1000).
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
