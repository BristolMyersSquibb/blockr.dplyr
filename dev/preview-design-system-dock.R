# Cross-package UI showcase — dock + DAG edition.
#
# The same board as dev/preview-design-system.R (every block from the
# 2026-07 design-system consistency round), but served in the real product
# chrome: blockr.dock views + the blockr.dag pipeline editor. Because
# blockr.dock ships the token :root, this app renders with the design
# tokens DEFINED — the plain sibling script covers the fallback-only case.
#
# Run from the workspace root:
#   Rscript blockr.dplyr/dev/preview-design-system-dock.R
# (serves on port 3838; the DAG view is the landing view)
#
# See the sibling script's header for the walk-through list (bands, amber
# cues, Enter-chip commits, checkbox-vs-pill rule, type pickers).
#
# NOTE: load_all() ALL of them, never a mix. Packages build their
# htmlDependencies on each other's assets via
# system.file("css", package = "blockr.dplyr"); pkgload swaps in a shim that
# maps that onto the source inst/ — but only inside namespaces it loaded.
# A load_all()'d blockr.dplyr behind an *installed* blockr.viz therefore
# yields src = '' and crashes addResourcePath on page render.

# Works from the workspace root or from the package dir.
root <- if (file.exists("blockr.dplyr/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.dplyr", "blockr.ggplot", "blockr.viz",
            "blockr.io", "blockr.dock")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}
library(blockr.dag)

options(
  shiny.port = 3838,
  "g6R.preserve_elements_position" = TRUE
)

serve(
  new_dock_board(
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
    links = list(
      list(from = "data", to = "slice", input = "data"),
      list(from = "data", to = "pivot_wider", input = "data"),
      list(from = "data", to = "pivot_longer", input = "data"),
      list(from = "data", to = "join", input = "x"),
      list(from = "data2", to = "join", input = "y"),
      list(from = "data", to = "select", input = "data"),
      list(from = "data", to = "filter", input = "data"),
      list(from = "data", to = "separate", input = "data"),
      list(from = "data", to = "unite", input = "data"),
      list(from = "data", to = "rename", input = "data"),
      list(from = "data", to = "mutate", input = "data"),
      list(from = "data", to = "summarize", input = "data"),
      list(from = "data", to = "arrange", input = "data"),
      list(from = "data", to = "bind_rows", input = "x"),
      list(from = "data2", to = "bind_rows", input = "y"),
      list(from = "data", to = "write", input = "data"),
      list(from = "data", to = "download", input = "data"),
      list(from = "data", to = "gg", input = "data"),
      list(from = "gg", to = "facet", input = "data"),
      list(from = "facet", to = "theme", input = "data"),
      list(from = "gg", to = "grid", input = "1"),
      list(from = "data", to = "chart", input = "data"),
      list(from = "data", to = "summary_table", input = "data"),
      list(from = "data", to = "tile", input = "data")
    ),
    extensions = new_dag_extension(),
    # Current dock API: named PLAIN list of `grids =` (the old `layouts =` is
    # swallowed by ... and silently ignored). Views are derived from the
    # grids. A grid child is either a bare panel id or panels(...) for a
    # tabbed group; "dag_extension" is the DAG panel's extension_id().
    # View names must be safe identifiers (letters, digits, . - _).
    grids = list(
      Pipeline = dock_grid("dag_extension"),
      `Data-IO` = dock_grid(
        panels("data", "data2"),
        panels("read", "write", "download")
      ),
      Bands = dock_grid(
        panels("slice", "pivot_wider"),
        panels("pivot_longer", "join")
      ),
      Cards = dock_grid(
        panels("select", "filter", "separate", "unite", "rename"),
        panels("mutate", "summarize", "arrange", "bind_rows")
      ),
      Plots = dock_grid(panels("gg", "facet"), panels("theme", "grid")),
      Renderers = dock_grid("chart", panels("summary_table", "tile"))
    ),
    active = "Pipeline"
  )
)
