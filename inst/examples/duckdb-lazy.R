# Larger-than-memory parquet via DuckDB, transformed lazily with blockr.dplyr.
# A 100,000,000-row parquet file is opened as a lazy DuckDB tbl and run through a
# chain of blockr.dplyr blocks. Every verb pushes to SQL and the shared HTML
# table preview pages through 100M rows without ever materializing the full
# table; the filter value-picker probes distinct levels with a bounded query.
#
#   [fn(...): DuckDB read_parquet -> lazy tbl]          <- variadic = arity-0 SOURCE
#       -> select -> filter (id < 1000) -> mutate -> arrange -> rename  (stay lazy)
#                                                       \-> summarize    (group agg)
#       -> [fn: collect()]   (filter cut 100M -> ~1000 in DuckDB, THEN materialize)
#
# Run with:
#   source(system.file("examples/duckdb-lazy.R", package = "blockr.dplyr"))
#
# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). Set it to TRUE -- or source this file from the
# dev/duckdb-lazy.R wrapper -- to load every blockr package from its LOCAL
# source checkout with pkgload::load_all(). One board, two loaders, no drift.
if (!exists("dev_local")) dev_local <- FALSE

options(blockr.html_table_preview = TRUE)  # swap DT -> shared lazy-aware HTML preview

blockr_pkgs <- c(
  "blockr.core",
  "blockr.ui",
  "blockr.dplyr",
  "blockr.dock",
  "blockr.dag",
  "blockr.extra"   # new_function_var_block (variadic source) + new_function_block
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

# DuckDB pushdown backend (CRAN).
requireNamespace("duckdb", quietly = TRUE)   # embedded OLAP engine, reads parquet
requireNamespace("DBI", quietly = TRUE)      # database connection layer
requireNamespace("dbplyr", quietly = TRUE)   # translates dplyr verbs to SQL
stopifnot(
  requireNamespace("duckdb", quietly = TRUE),
  requireNamespace("DBI", quietly = TRUE),
  requireNamespace("dbplyr", quietly = TRUE)
)

# ---- Data: 100M-row parquet (mounted on deploy, self-generated elsewhere) ---
# On blockr.cloud the file is generated once on the host and mounted read-only
# at /data (fast container start, shared across sessions; see
# blockr.deploy/shinyproxy-hetzner/scripts/gen-duckdb-demo-parquet.R). Anywhere
# else it is generated once into a fixed cache dir on first run and reused.
# DuckDB streams it; it is never held in an R data.frame. Override the size
# before sourcing for a lighter local run: `n_rows <- 5e6; source(...)`.
if (!exists("n_rows")) n_rows <- 100e6

pq <- "/data/blockr-demo-100M.parquet"                # deploy: mounted read-only
if (!file.exists(pq)) {
  cache <- "/tmp/blockr-duckdb-lazy"                  # fixed path -> reused across runs
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  pq <- file.path(cache, "blockr-demo-100M.parquet")
  if (!file.exists(pq)) {
    message(sprintf("generating %s-row parquet via DuckDB (one-time)...",
                    format(n_rows, big.mark = ",")))
    con0 <- DBI::dbConnect(duckdb::duckdb())
    DBI::dbExecute(con0, "SET memory_limit='2GB'")    # bounded: don't disturb neighbours
    DBI::dbExecute(con0, "SET threads TO 2")
    DBI::dbExecute(con0, sprintf("
      COPY (SELECT i AS id,
                   chr(97 + (i %% 5)::INT) AS grp,
                   round((random() * 2 - 1)::DOUBLE, 3) AS x,
                   round((random() * 100)::DOUBLE, 1) AS y
            FROM range(%.0f) t(i)) TO '%s' (FORMAT parquet)", n_rows, pq))
    DBI::dbDisconnect(con0, shutdown = TRUE)
  }
}
stopifnot(file.exists(pq))
message(sprintf("parquet: %s (%.2f GB on disk, %s rows)",
                pq, file.size(pq) / 1e9, format(n_rows, big.mark = ",")))

# --- escape hatch 1: variadic SOURCE -- open DuckDB, return a LAZY tbl --------
# `function(...)` takes no input, so the block is a root (no upstream needed).
# Self-qualified (DBI::/duckdb::/dplyr::) because a block expr runs sandboxed.
read_remote_fn <- sprintf(
  "function(..., path = \"%s\") {
  con <- DBI::dbConnect(duckdb::duckdb())
  dplyr::tbl(con, sprintf(\"read_parquet('%%s')\", path))
}",
  pq
)

# --- escape hatch 2: materialize the (now small) filtered result -------------
# Safe because `filt` below cuts 100M rows down to ~1000 IN DuckDB first; only
# the filtered result is pulled into R. (collect() on the full 100M would OOM --
# the lesson: filter on the remote table, THEN collect the small result.)
collect_fn <- "function(data) {
  dplyr::collect(data)
}"

board <- new_dock_board(
  blocks = c(
    remote = new_function_var_block(fn = read_remote_fn),  # arity-0 lazy source

    # --- as many blockr.dplyr blocks as stay lazy on a remote backend --------
    cols   = new_select_block(
      columns = list("id", "grp", "x", "y"),
      exclude = FALSE,
      distinct = FALSE
    ),
    # cut 100M rows down to ~1000 IN DuckDB, so the collect downstream is safe
    filt   = new_filter_block(
      conditions = list(
        list(type = "numeric", column = "id", op = "<", value = 1000)
      ),
      operator = "&"
    ),
    mutated = new_mutate_block(),
    sorted = new_arrange_block(),
    renamed = new_rename_block(),
    agg    = new_summarize_block(),

    # --- materialize ----------------------------------------------------------
    collected = new_function_block(fn = collect_fn)
  ),
  links = links(
    from = c("remote", "cols", "filt", "mutated", "sorted", "renamed", "renamed"),
    to   = c("cols", "filt", "mutated", "sorted", "renamed", "agg", "collected")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

serve(board, "duckdb-lazy")
