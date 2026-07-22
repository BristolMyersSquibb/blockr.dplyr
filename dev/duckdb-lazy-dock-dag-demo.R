# Larger-than-memory parquet via DuckDB, transformed lazily with blockr.dplyr
# ---------------------------------------------------------------------------
# Issue BristolMyersSquibb/blockr#480: read >RAM data as a lazy dataframe in
# blockr. A 100,000,000-row parquet file is opened as a lazy DuckDB tbl and
# run through a chain of blockr.dplyr blocks. Every verb pushes to SQL and the
# shared HTML table preview (blockr.ui) pages through 100M rows without ever
# materializing the full table; the filter value-picker probes distinct levels
# with a bounded query.
#
#   [fn(...): DuckDB read_parquet -> lazy tbl]          <- variadic = arity-0 SOURCE
#       -> select -> filter (id < 1000) -> mutate -> arrange -> rename  (stay lazy)
#                                                       \-> summarize    (group agg)
#       -> [fn: collect()]   (filter cut 100M -> ~1000 in DuckDB, THEN materialize)
#
# The reader is a VARIADIC function block (`function(...)`): it takes no input,
# so it is a true source -- no dummy upstream block needed. Two function blocks
# remain, both candidates for dedicated blockr.dplyr blocks:
#   * the reader  -> a `new_duckdb_block()` / lazy read source (arity-0)
#   * the collect -> a `new_collect_block()` (dplyr::collect / compute)
# Everything in between is a real blockr.dplyr block on the remote table.

options(
  blockr.tabular_display = blockr.ui::html_table_display,   # swap DT -> shared lazy-aware HTML preview
  shiny.port = 3845,
  # shiny.host = "0.0.0.0",
  shiny.launch.browser = T
)

library(shiny)
library(blockr.core)
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
library(blockr.dock)
library(blockr.dag)
pkgload::load_all("blockr.extra")   # new_function_var_block (variadic source) + new_function_block

stopifnot(
  requireNamespace("duckdb", quietly = TRUE),
  requireNamespace("DBI", quietly = TRUE),
  requireNamespace("dbplyr", quietly = TRUE)
)

# --- a 100M-row parquet on disk (stand-in for >RAM data) ---------------------
# Generated once, streamed by DuckDB itself (never held in an R data.frame).
pq <- "/tmp/blockr-demo-100M.parquet"
if (!file.exists(pq)) {
  message("generating 100,000,000-row parquet via DuckDB (one-time)...")
  con0 <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con0, sprintf("
    COPY (SELECT i AS id,
                 chr(97 + (i %% 5)::INT) AS grp,
                 round((random() * 2 - 1)::DOUBLE, 3) AS x,
                 round((random() * 100)::DOUBLE, 1) AS y
          FROM range(100000000) t(i)) TO '%s' (FORMAT parquet)", pq))
  DBI::dbDisconnect(con0, shutdown = TRUE)
}
message(sprintf("parquet: %s (%.2f GB on disk, 100,000,000 rows)",
                pq, file.size(pq) / 1e9))

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
# the filtered result is pulled into R. (collect() on the full 100M would OOM —
# the lesson: filter on the remote table, THEN collect the small result.)
collect_fn <- "function(data) {
  dplyr::collect(data)
}"

board <- new_dock_board(
  blocks = c(
    remote = new_function_var_block(fn = read_remote_fn),  # arity-0 lazy source

    # --- as many blockr.dplyr blocks as stay lazy on a remote backend --------
    cols   = new_select_block(
      state = list(
        columns = list("id", "grp", "x", "y"),
        exclude = FALSE,
        distinct = FALSE
      )
    ),
    # cut 100M rows down to ~1000 IN DuckDB, so the collect downstream is safe
    filt   = new_filter_block(
      state = list(
        conditions = list(
          list(type = "numeric", column = "id", op = "<", value = 1000)
        ),
        operator = "&"
      )
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
