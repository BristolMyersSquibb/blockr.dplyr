# Run the DuckDB-lazy board against LOCAL source checkouts (your latest
# uncommitted changes to any blockr package). This is the pkgload::load_all()
# counterpart of the shipped, library()-based inst/examples/duckdb-lazy.R: it
# just flips the loader and sources it, so the two can never drift.
#
# Run from an R session at the workspace root:
#   source("blockr.dplyr/dev/duckdb-lazy.R")
#
# For a lighter local run (skip generating the full 100M-row parquet):
#   n_rows <- 5e6; source("blockr.dplyr/dev/duckdb-lazy.R")
#
# (End users without the source checkouts run the shipped copy instead:
#   source(system.file("examples/duckdb-lazy.R", package = "blockr.dplyr")))

options(shiny.port = 3838, shiny.host = "0.0.0.0")

dev_local <- TRUE
source("blockr.dplyr/inst/examples/duckdb-lazy.R")
