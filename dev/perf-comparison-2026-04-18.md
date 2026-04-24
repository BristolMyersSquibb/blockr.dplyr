# blockr.dplyr Filter Block — Startup Performance Comparison

Reproducible benchmark comparing three versions of `blockr.dplyr` on the same
50,000-row dataset and the same minimal board layout.

## TL;DR

David's report of "massively slower startup" was real and reproducible. The
post-JS-rewrite `main` branch takes **~3.5 s** before the filter UI is usable
on a 50K-row dataset; CRAN takes **~0.6 s**. The lazy-loading fix on
`fix/filter-lazy-values` brings it back to **~0.55 s** — back to CRAN parity
while keeping the new JS-driven UI.

| Version | Time before filter is usable¹ | vs CRAN |
| --- | ---: | --- |
| CRAN (`v0.1.0`)               |  ~0.62 s | (baseline) |
| `main` (eager metadata)        | **3.45 s** | 5.6× slower |
| `fix/filter-lazy-values`      |  0.55 s | parity (~10 % faster) |

¹ Time from page navigation until the filter widget has its column metadata
loaded and is ready to be used.

## Setup

- Branch / install layout
  - `/tmp/lib_cran` — CRAN release `0.1.0`
  - `/tmp/lib_main` — `main` after the feat/js merge (`0.1.0.9001`, eager
    `build_column_meta`)
  - `/tmp/lib_fix`  — `fix/filter-lazy-values` (`0.1.0.9001`, lazy
    `build_column_summary` + on-demand `build_column_values`)
- Test board: a single `new_static_block(df)` connected to a
  `new_filter_block()`.
- Test data: 50,000 rows × 6 columns (`id` 50K-unique, `category` LETTERS,
  `group` 100 levels, `value` numeric with NAs, `score` 1K-unique numeric,
  `label` 5K-unique strings, `category` with NAs).
- Chromium / chromote driver navigates to the app, hooks every Shiny custom
  message handler before page scripts run, and timestamps the exact moment
  each `filter-*` message is processed by the JS widget.

All scripts and raw outputs live in `/tmp/blockr-bench/`.

## Measurements

### 1) R-side: per-call cost of building the column metadata

Times from `bench_r_time.R` on the 50K dataset, measuring what runs each
time `data()` fires inside the filter block.

| Function | Version | R compute | JSON serialise | **Total / call** |
| --- | --- | ---: | ---: | ---: |
| `build_column_meta`   (eager all columns) | `main` | 14 ms | 2 748 ms | **2 762 ms** |
| `build_column_meta`   (still kept on fix) | `fix`  | 15 ms | 2 763 ms | **2 778 ms** |
| `build_column_summary` (lightweight)      | `fix`  | **0.2 ms** | **0.4 ms** | **0.6 ms** |

CRAN never enters this code path — it uses the pre-rewrite
`mod_value_filter_server` which fetches unique values lazily, per column,
when the user opens a dropdown.

The dominant cost is **JSON serialisation, not R compute**: the `id` column
alone (50 000 unique numbers) takes ~1 s to serialise, and the full payload
~2.7 s. This is what the R thread blocks on while the user waits.

### 2) Browser-side: when does the filter widget become usable?

Driver is `drive_inspect.R` — Chromium hooks `Shiny.addCustomMessageHandler`
before page scripts execute, then timestamps every `filter-*` message that
arrives over the WebSocket.

| Version | `shiny_ready` | `filter-columns` arrives | Filter usable | Comment |
| --- | ---: | ---: | ---: | --- |
| CRAN | 53 ms | n/a (no msg) | **~620 ms** | renderUI delivers the column dropdown |
| main | 54 ms | **3 453 ms** | **~3 450 ms** | UI present but value pickers blocked on the message |
| fix  | 50 ms | **553 ms** | **~555 ms** | summary-only payload; values on-demand |

For `fix`, the auto-loaded values for the first selected column arrive
~150 ms after the user opens the dropdown (`filter-column-values` at
1 678 ms in the trace).

### 3) Server-side: first-GET HTML

| Version | Time to "Listening on" | First GET (server-rendered HTML) | HTML size |
| --- | ---: | ---: | ---: |
| CRAN | 630 ms | 391 ms | 52 765 chars |
| main | 628 ms | 380 ms | 46 094 chars |
| fix  | 581 ms | 373 ms | 46 094 chars |

Static-HTML pre-WebSocket times are equivalent across versions — the
regression only manifests once `data()` flows in over the WebSocket.

### 4) Pre-app construction (library load + block + board + serve)

| Version | Library load | `new_filter_block` | `new_board` | TOTAL pre-serve |
| --- | ---: | ---: | ---: | ---: |
| CRAN | 339 ms | 2 ms | 131 ms | 474 ms |
| main | 330 ms | 4 ms | 122 ms | 456 ms |
| fix  | 336 ms | 4 ms | 123 ms | 463 ms |

No regression here; the difference is entirely in the data-flow path.

## Root cause

The feat/js merge (PR #71) replaced `mod_value_filter_server` (lazy,
per-column `updateSelectizeInput`) with a JS-driven widget. To populate the
JS widget, `filter_block.R` started doing this on every `data()` change:

```r
observeEvent(data(), {
  meta <- build_column_meta(data())
  session$sendCustomMessage(
    "filter-columns",
    list(id = ns("filter_input"), columns = meta)
  )
})
```

`build_column_meta(df)` ran `sort(unique(...))` for **every column**, then
`shiny:::toJSON` serialised the whole blob. On a 50K-row dataset the
serialisation alone takes 2–3 seconds, blocking the R thread. That is what
David felt.

## Fix (`fix/filter-lazy-values`)

The data-flow path now sends only column **summaries** (name + type +
`hasNA`):

```r
build_column_summary(df)   # 0.2 ms instead of 14 ms
```

Unique values are fetched on demand via a new message:

```r
observeEvent(input$filter_input_request_values, {
  col <- input$filter_input_request_values
  if (col %in% colnames(data())) {
    session$sendCustomMessage(
      "filter-column-values",
      list(id = ns("filter_input"),
           column = build_column_values(data(), col))
    )
  }
})
```

The JS widget shows a "Loading…" placeholder while waiting for values for
the picked column; subsequent picks of the same column are cached.

This restores CRAN-class startup latency while keeping the new JS-driven
filter UI.

## Reproduce

```bash
# Install three lib trees
mkdir -p /tmp/lib_cran /tmp/lib_main /tmp/lib_fix
# (lib_cran from CRAN; lib_main from main@HEAD; lib_fix from fix-branch@HEAD)

# 1. R-side micro
for ver in cran main fix; do
  Rscript /tmp/bench_r_time.R /tmp/lib_$ver $ver
done

# 2. Startup phases
for ver in cran main fix; do
  Rscript /tmp/bench_startup.R /tmp/lib_$ver $ver
done

# 3. Server-side first GET
/tmp/measure_first_load.sh /tmp/lib_cran cran 7860
/tmp/measure_first_load.sh /tmp/lib_main main 7861
/tmp/measure_first_load.sh /tmp/lib_fix  fix  7862

# 4. Browser-side (chromote + Chromium)
# Boots app on each port and inspects WebSocket message timing:
# /tmp/blockr-bench/drive_inspect.R
```

## Files

| Path | Purpose |
| --- | --- |
| `/tmp/bench_r_time.R`              | R-side per-call timing |
| `/tmp/bench_startup.R`             | Library-load → board → serve phases |
| `/tmp/bench_app.R`                 | Standalone Shiny app (50K rows) |
| `/tmp/measure_first_load.sh`       | Wall-clock to "Listening" + first GET |
| `/tmp/measure_full.sh`             | + asset URLs and total bytes |
| `/tmp/blockr-bench/drive_inspect.R`| Chromium driver, hooks Shiny custom msg handlers |
| `/tmp/blockr-bench/results/`       | Captured outputs from this run |

Raw outputs from this run are committed to
`/tmp/blockr-bench/results/{r_time,startup,first_load,inspect3}.txt`.
