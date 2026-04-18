# =============================================================================
# blockr.dplyr filter-block startup-time reprex for David
# =============================================================================
#
# What this does
# --------------
# Benchmarks two installs of blockr.dplyr against your real dataset and your
# real board layout, on the same machine and the same blockr.core install,
# so the only thing that changes between runs is the version of blockr.dplyr.
#
# The point: rule out any apples-to-oranges (different blockr.core, different
# dock/DAG extensions, different data) so we can pinpoint where the residual
# slowdown is coming from.
#
# How to use
# ----------
#
# 1. Install both versions side-by-side in distinct lib trees:
#
#      mkdir -p ~/blockr-bench/lib_cran ~/blockr-bench/lib_fix
#
#      Rscript -e 'install.packages("blockr.dplyr",
#                                   lib = "~/blockr-bench/lib_cran",
#                                   repos = "https://cloud.r-project.org")'
#
#      # Clone and install the fix branch
#      git clone --branch fix/filter-lazy-values \
#        https://github.com/BristolMyersSquibb/blockr.dplyr.git \
#        /tmp/blockr.dplyr-fix
#      Rscript -e 'install.packages("/tmp/blockr.dplyr-fix",
#                                   lib = "~/blockr-bench/lib_fix",
#                                   repos = NULL, type = "source")'
#
# 2. Drop your real data into the YOUR_DATA section below (or leave the
#    synthetic 50K-row default). If your data lives in an .rds, load it.
#
# 3. Optionally edit YOUR_BOARD to mirror your actual app's blocks/links
#    (the default = a single static_block + filter_block, which is the
#    minimum reproducer; adding the dock/DAG extensions you actually use
#    will surface any costs that come from those, not from blockr.dplyr).
#
# 4. Run it and send back the output:
#
#      Rscript dev/perf-reprex.R
#
# Output
# ------
# - "R-side cost per data flow" for each lib path (R compute + JSON serialize
#   times — anything > 100 ms is suspicious for the cran lib; anything > 100 ms
#   on the fix lib means the lazy-load fix isn't taking effect).
# - "Time to filter-columns msg" via chromote (real browser) — this is the
#   user-felt time before the filter UI is ready.
# - Library-load + board-construction phase costs.
# =============================================================================

# -- 1. Pick your lib paths -----------------------------------------------------

LIB_CRAN <- normalizePath("~/blockr-bench/lib_cran", mustWork = FALSE)
LIB_FIX  <- normalizePath("~/blockr-bench/lib_fix",  mustWork = FALSE)

stopifnot(
  "lib_cran missing — install blockr.dplyr from CRAN there first" =
    file.exists(file.path(LIB_CRAN, "blockr.dplyr", "DESCRIPTION")),
  "lib_fix missing — install blockr.dplyr fix-branch there first" =
    file.exists(file.path(LIB_FIX, "blockr.dplyr", "DESCRIPTION"))
)

# -- 2. YOUR_DATA: replace this with the data your real app loads --------------

make_data <- function() {
  # Default: 50K-row synthetic dataset with one high-cardinality column.
  # Replace with: readRDS("~/path/to/your.rds")  — or anything else.
  set.seed(42)
  n <- 50000L
  data.frame(
    id       = seq_len(n),
    category = sample(LETTERS, n, replace = TRUE),
    group    = sample(paste0("Group_", 1:100), n, replace = TRUE),
    value    = rnorm(n),
    score    = sample(1:1000, n, replace = TRUE),
    label    = sample(paste0("item_", 1:5000), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# -- 3. YOUR_BOARD: replace with your real board layout ------------------------

# IMPORTANT: this needs to use the CURRENTLY loaded blockr.dplyr namespace.
# `lib_path` is set externally before sourcing this file, so just call the
# blockr functions directly.
make_board <- function() {
  blockr.core::new_board(
    blocks = list(
      data   = blockr.core::new_static_block(make_data()),
      filter = blockr.dplyr::new_filter_block()
    ),
    links = blockr.core::links(from = "data", to = "filter")
  )
}

# =============================================================================
# Benchmark machinery — generally don't need to edit below this line.
# =============================================================================

# -- A. R-side micro: build_column_meta / summary cost --------------------------
# Each lib runs in its OWN R subprocess so the namespace is loaded fresh.
# Without this, R caches the first-loaded namespace and ignores subsequent
# `lib.loc =` arguments — making the second comparison meaningless.

run_micro <- function(lib_path, label) {
  cat("\n=== ", label, " (lib = ", lib_path, ") ===\n", sep = "")
  this_script <- tempfile(fileext = ".R")
  writeLines(sprintf('
    suppressMessages(library(blockr.dplyr, lib.loc = %s))
    cat("  blockr.dplyr version:",
        as.character(packageVersion("blockr.dplyr")), "\\n")
    set.seed(42); n <- 50000L
    df <- data.frame(
      id = seq_len(n), category = sample(LETTERS, n, replace = TRUE),
      group = sample(paste0("Group_", 1:100), n, replace = TRUE),
      value = rnorm(n), score = sample(1:1000, n, replace = TRUE),
      label = sample(paste0("item_", 1:5000), n, replace = TRUE),
      stringsAsFactors = FALSE
    )
    ns <- asNamespace("blockr.dplyr")
    if (exists("build_column_meta", envir = ns)) {
      fn <- get("build_column_meta", envir = ns)
      t  <- system.time(replicate(3, fn(df)))
      t2 <- system.time(replicate(3, shiny:::toJSON(fn(df))))
      cat(sprintf("  build_column_meta  R compute    : %%5.0f ms / call\\n",
                  t["elapsed"]  * 1000 / 3))
      cat(sprintf("  build_column_meta  + JSON serial: %%5.0f ms / call\\n",
                  t2["elapsed"] * 1000 / 3))
    }
    if (exists("build_column_summary", envir = ns)) {
      fn <- get("build_column_summary", envir = ns)
      t  <- system.time(replicate(20, fn(df)))
      t2 <- system.time(replicate(20, shiny:::toJSON(fn(df))))
      cat(sprintf("  build_column_summary R compute    : %%5.1f ms / call\\n",
                  t["elapsed"]  * 1000 / 20))
      cat(sprintf("  build_column_summary + JSON serial: %%5.1f ms / call\\n",
                  t2["elapsed"] * 1000 / 20))
    }
    if (!exists("build_column_meta", envir = ns) &&
        !exists("build_column_summary", envir = ns)) {
      cat("  (CRAN-era code path — uses lazy mod_value_filter, no eager meta)\\n")
    }
  ', deparse(lib_path)), this_script)
  system2(file.path(R.home("bin"), "Rscript"), this_script,
          stdout = "", stderr = "")
}

# -- B. Startup: library load + block + board construction ---------------------

run_startup <- function(lib_path, label) {
  cat("\n=== ", label, " startup phases ===\n", sep = "")
  Rscript <- file.path(R.home("bin"), "Rscript")
  # Run in a fresh process so library load is timed honestly.
  this_script <- tempfile(fileext = ".R")
  writeLines(sprintf('
    t1 <- system.time({
      library(blockr.core)
      library(blockr.dplyr, lib.loc = %s)
    })
    cat(sprintf("  library load:       %%6.0f ms\\n", t1["elapsed"] * 1000))
    t2 <- system.time(b <- new_filter_block())
    cat(sprintf("  new_filter_block(): %%6.0f ms\\n", t2["elapsed"] * 1000))
    t3 <- system.time({
      brd <- new_board(
        blocks = list(data = new_static_block(mtcars), filter = b),
        links = links(from = "data", to = "filter")
      )
    })
    cat(sprintf("  new_board():        %%6.0f ms\\n", t3["elapsed"] * 1000))
    t4 <- system.time(app <- serve(brd))
    cat(sprintf("  serve():            %%6.0f ms\\n", t4["elapsed"] * 1000))
  ', deparse(lib_path)), this_script)
  system2(Rscript, this_script, stdout = "", stderr = "")
}

# -- C. Browser-side: time-to-filter-columns (chromote) ------------------------

# This is the user-facing measure: how long after page navigation before the
# `filter-columns` WebSocket message arrives at the JS widget.
#
# Requires: chromote + a Chromium binary. If you don't have chromote handy,
# skip this step; the R-side micro is usually enough to confirm the regression.

run_browser <- function(lib_path, label, port) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    cat("\n[skip] chromote not installed; install with install.packages('chromote')\n")
    return(invisible())
  }

  cat("\n=== ", label, " browser timing (port ", port, ") ===\n", sep = "")

  # Ensure the port is free; if something is listening, abort with a hint.
  port_in_use <- tryCatch({
    con <- suppressWarnings(socketConnection(
      host = "127.0.0.1", port = port, blocking = TRUE,
      open = "r+", timeout = 1))
    close(con); TRUE
  }, error = function(e) FALSE, warning = function(e) FALSE)
  if (port_in_use) {
    cat("  [skip] port ", port, " is already in use; chromote would attach to ",
        "the wrong app.\n  Free the port (e.g. kill the listener) or change ",
        "the port arg.\n", sep = "")
    return(invisible())
  }

  # Boot app in background, in a fresh R process pointed at lib_path.
  app_script <- tempfile(fileext = ".R")
  writeLines(sprintf('
    library(blockr.core)
    library(blockr.dplyr, lib.loc = %s)
    set.seed(42); n <- 50000
    df <- data.frame(
      id = seq_len(n), category = sample(LETTERS, n, replace = TRUE),
      group = sample(paste0("Group_", 1:100), n, replace = TRUE),
      value = rnorm(n), score = sample(1:1000, n, replace = TRUE),
      label = sample(paste0("item_", 1:5000), n, replace = TRUE),
      stringsAsFactors = FALSE
    )
    df$value[sample(n, 500)] <- NA
    app <- serve(new_board(
      blocks = list(data = new_static_block(df), filter = new_filter_block()),
      links = links(from = "data", to = "filter")
    ))
    shiny::runApp(app, port = %d, host = "127.0.0.1", launch.browser = FALSE)
  ', deparse(lib_path), port), app_script)

  log_file <- tempfile(fileext = ".log")
  pid <- sys::exec_background(file.path(R.home("bin"), "Rscript"),
                              args = app_script,
                              std_out = log_file, std_err = log_file)
  on.exit(tools::pskill(pid), add = TRUE)

  # Wait for "Listening on"
  deadline <- Sys.time() + 30
  while (Sys.time() < deadline) {
    if (file.exists(log_file)) {
      l <- readLines(log_file, warn = FALSE)
      if (any(grepl("Listening on", l))) break
    }
    Sys.sleep(0.05)
  }
  Sys.sleep(0.5)

  b <- chromote::ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Runtime$enable(); b$Page$enable()

  # Hook every Shiny custom message handler before page scripts run.
  hook <- '
    window.__bench = { events: [], t0: performance.now() };
    const log = (n, sz) => window.__bench.events.push(
      { n: n, t: Math.round(performance.now() - window.__bench.t0), sz: sz });
    const wait = setInterval(() => {
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        clearInterval(wait);
        const orig = Shiny.addCustomMessageHandler;
        Shiny.addCustomMessageHandler = function(type, h) {
          orig.call(Shiny, type, function(msg) {
            try { log("msg:" + type, JSON.stringify(msg).length); }
            catch(e) { log("msg:" + type, -1); }
            return h(msg);
          });
        };
      }
    }, 10);
  '
  b$Page$addScriptToEvaluateOnNewDocument(source = hook)
  b$Page$navigate(sprintf("http://127.0.0.1:%d", port))

  # Wait until message stream stabilises (no new events for 3s) or 30s.
  last <- 0; stable <- 0; deadline <- Sys.time() + 30
  while (Sys.time() < deadline) {
    Sys.sleep(0.1)
    r <- tryCatch(b$Runtime$evaluate("window.__bench && window.__bench.events.length"),
                  error = function(e) NULL)
    cur <- if (is.null(r) || is.null(r$result$value)) 0 else r$result$value
    if (cur == last) {
      stable <- stable + 1
      if (stable >= 30 && cur > 3) break
    } else { stable <- 0; last <- cur }
  }

  ev <- jsonlite::fromJSON(
    b$Runtime$evaluate("JSON.stringify(window.__bench.events)")$result$value,
    simplifyDataFrame = TRUE
  )
  if (nrow(ev) == 0) { cat("  no messages captured\n"); return(invisible()) }
  for (i in seq_len(nrow(ev))) {
    cat(sprintf("  %5d ms  %-32s  %s\n",
                ev$t[i], ev$n[i],
                if (is.na(ev$sz[i])) "" else paste0(ev$sz[i], "B")))
  }
}

# =============================================================================
# Run the suite
# =============================================================================

cat("===== R-side micro-benchmarks =====\n")
run_micro(LIB_CRAN, "cran")
run_micro(LIB_FIX,  "fix")

cat("\n===== Startup phase costs =====\n")
run_startup(LIB_CRAN, "cran")
run_startup(LIB_FIX,  "fix")

cat("\n===== Browser-side: time-to-filter-columns =====\n")
run_browser(LIB_CRAN, "cran", 7860L)
Sys.sleep(2)
run_browser(LIB_FIX,  "fix",  7861L)

cat("\n=====\nDONE — please send the full output back.\n")
