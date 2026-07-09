# Cross-package UI showcase — the design-system conventions on one board.
#
# Every block touched by the 2026-07 consistency round (blockr.dplyr,
# blockr.io, blockr.ggplot, plus blockr.viz as the reference generation),
# served with plain blockr.core::serve() and NO dock/ai/dag dependencies —
# so this also demonstrates the standalone case where all styling runs on
# the var(--blockr-*, fallback) values.
#
# Run from your local blockr checkout, either
#   - from the blockr.dplyr package dir:  source("dev/preview-design-system.R")
#   - or from the workspace root:         source("blockr.dplyr/dev/preview-design-system.R")
#
# Things to try:
#   * Gears (slice, pivots, join, read/write/download, viz/ggplot blocks):
#     each opens the same full-width in-flow settings band with a beak —
#     content pushes down, the gear is the only toggle, no outside-click
#     dismissal.
#   * Amber required-empty cues: pivot_longer Columns, pivot_wider
#     Names/Values from, separate, unite, slice Order by (type = min),
#     read file path — clear as soon as a value is picked.
#   * "Enter ↵" commit chips: type into pivot_longer Names to, grid
#     Title/Subtitle/Caption (shared engine), or the read path — the chip
#     arms, Enter/blur commits (fades to ✓), Escape reverts. Rename /
#     slice n / filter numeric use the compact bare-↵ variant.
#   * Checkboxes vs cycling pills: select Exclude/Distinct and filter
#     "Keep pick order" are checkboxes; arrange asc/desc, filter AND/OR
#     and slice n/% stay pills (label = value).
#   * Type pickers: ggplot's 9-type icon tile grid vs facet's 2-type
#     segmented strip.
#
# NOTE: pkgload::load_all() serves the INSTALLED inst/js, not source —
# after editing any inst/ asset, reinstall the package first.

pkg <- if (file.exists("DESCRIPTION")) "." else "blockr.dplyr"
pkgload::load_all(pkg)

library(blockr.core)
library(blockr.ggplot)
library(blockr.viz)
library(blockr.io)

options(shiny.port = 3838)

serve(
  new_board(
    blocks = c(
      # --- data ------------------------------------------------------------
      data = new_dataset_block(dataset = "mtcars", package = "datasets"),
      data2 = new_dataset_block(dataset = "mtcars", package = "datasets"),

      # --- blockr.io: band + amber path + commit chip ------------------------
      read = new_read_block(),
      write = new_write_block(),
      download = new_download_block(),

      # --- blockr.dplyr: settings bands (gear) -------------------------------
      slice = new_slice_block(type = "min"),   # gear visible; order_by amber
      pivot_wider = new_pivot_wider_block(),   # band text chips + amber
      pivot_longer = new_pivot_longer_block(), # band checkbox + amber
      join = new_join_block(),                 # band suffix chips

      # --- blockr.dplyr: checkboxes + chips on the card ----------------------
      select = new_select_block(),             # exclude/distinct checkboxes
      filter = new_filter_block(),             # keep-pick-order checkbox
      separate = new_separate_block(),         # checkboxes + amber col/into
      unite = new_unite_block(),               # checkboxes + amber columns
      rename = new_rename_block(),             # compact chip in rows
      mutate = new_mutate_block(),             # name joins the expr chip
      summarize = new_summarize_block(),       # smb- prefix, compact chip
      arrange = new_arrange_block(),           # cycling asc/desc pill stays
      bind_rows = new_bind_rows_block(),       # .id commit chip

      # --- blockr.ggplot: tile/strip type pickers + engine chips -------------
      gg = new_ggplot_block(type = "point", x = "wt", y = "mpg"),
      facet = new_facet_block(),
      theme = new_theme_block(),
      grid = new_grid_block(),                 # title/subtitle/caption chips

      # --- blockr.viz: the reference generation ------------------------------
      chart = new_chart_block(),
      summary_table = new_summary_table_block(),
      tile = new_tile_block()
    ),
    links = c(
      new_link("data", "slice", "data"),
      new_link("data", "pivot_wider", "data"),
      new_link("data", "pivot_longer", "data"),
      new_link("data", "join", "x"),
      new_link("data2", "join", "y"),
      new_link("data", "select", "data"),
      new_link("data", "filter", "data"),
      new_link("data", "separate", "data"),
      new_link("data", "unite", "data"),
      new_link("data", "rename", "data"),
      new_link("data", "mutate", "data"),
      new_link("data", "summarize", "data"),
      new_link("data", "arrange", "data"),
      new_link("data", "bind_rows", "x"),
      new_link("data2", "bind_rows", "y"),
      new_link("data", "write", "data"),
      new_link("data", "download", "data"),
      new_link("data", "gg", "data"),
      new_link("gg", "facet", "data"),
      new_link("facet", "theme", "data"),
      new_link("gg", "grid", "1"),
      new_link("data", "chart", "data"),
      new_link("data", "summary_table", "data"),
      new_link("data", "tile", "data")
    )
  )
)
