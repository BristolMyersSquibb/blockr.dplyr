library(shinytest2)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Push state into a custom JS input binding via run_js().
#' shinytest2's set_inputs() can't find blockr custom bindings,
#' so we drive setState() + _submit() on the JS block instance directly.
set_block_state <- function(app, block_id, input_suffix, state) {
  input_id <- paste0("board-block_", block_id, "-expr-", input_suffix)
  state_json <- jsonlite::toJSON(state, auto_unbox = TRUE, null = "null")

  app$run_js(sprintf(
    "var el = document.getElementById('%s');
     el._block.setState(%s);
     el._block._submit();",
    input_id, state_json
  ))
  app$wait_for_idle()
}

#' Read a specific block's result data frame from the exported test values.
get_block_result <- function(app, block_id) {
  vals <- app$get_values()
  vals$export$result[[block_id]]
}

# ---------------------------------------------------------------------------
# Shared app instance (test fixture)
# ---------------------------------------------------------------------------

app <- AppDriver$new(
  test_path("apps", "dplyr-e2e"),
  name = "dplyr-e2e",
  seed = 42,
  load_timeout = 60 * 1000,
  timeout = 15 * 1000
)
app$wait_for_idle()

withr::defer(app$stop(), testthat::teardown_env())

# ===========================================================================
# FILTER (6 tests)
# ===========================================================================

test_that("filter: values include", {
  set_block_state(app, "filter", "filter_input", list(
    conditions = list(
      list(type = "values", column = "cyl", values = list("4"), mode = "include")
    ),
    operator = "&"
  ))
  res <- get_block_result(app, "filter")
  expect_equal(nrow(res), 11)
  expect_true(all(res$cyl == 4))
})

test_that("filter: values exclude", {
  set_block_state(app, "filter", "filter_input", list(
    conditions = list(
      list(type = "values", column = "cyl", values = list("4"), mode = "exclude")
    ),
    operator = "&"
  ))
  res <- get_block_result(app, "filter")
  expect_equal(nrow(res), 21)
  expect_true(all(res$cyl != 4))
})

test_that("filter: numeric condition", {
  set_block_state(app, "filter", "filter_input", list(
    conditions = list(
      list(type = "numeric", column = "mpg", op = ">", value = 25)
    ),
    operator = "&"
  ))
  res <- get_block_result(app, "filter")
  expect_true(all(res$mpg > 25))
  expect_true(nrow(res) > 0)
})

test_that("filter: expression condition", {
  set_block_state(app, "filter", "filter_input", list(
    conditions = list(
      list(type = "expr", expr = "mpg > 20 & cyl == 4")
    ),
    operator = "&"
  ))
  res <- get_block_result(app, "filter")
  expect_true(all(res$mpg > 20))
  expect_true(all(res$cyl == 4))
})

test_that("filter: two conditions with AND", {
  set_block_state(app, "filter", "filter_input", list(
    conditions = list(
      list(type = "values", column = "cyl",
           values = list("4", "6"), mode = "include"),
      list(type = "numeric", column = "mpg", op = ">=", value = 20)
    ),
    operator = "&"
  ))
  res <- get_block_result(app, "filter")
  expect_true(all(res$cyl %in% c(4, 6)))
  expect_true(all(res$mpg >= 20))
})

test_that("filter: three conditions with OR (values + numeric + expr)", {
  set_block_state(app, "filter", "filter_input", list(
    conditions = list(
      list(type = "values", column = "cyl", values = list("4"), mode = "include"),
      list(type = "numeric", column = "hp", op = ">", value = 200),
      list(type = "expr", expr = "gear == 5")
    ),
    operator = "|"
  ))
  res <- get_block_result(app, "filter")
  expect_true(all(res$cyl == 4 | res$hp > 200 | res$gear == 5))
  expect_true(nrow(res) > 0)
})

# Regression: on board restore the server fires both `filter-block-update`
# (state) and `filter-columns` (metadata) custom messages. If metadata
# arrives after state, `updateColumns` must not clobber restored values.
# See filter-block.js `_onColumnChange` / `updateColumns`.
test_that("filter: values survive a subsequent updateColumns (restore race)", {
  set_block_state(app, "filter", "filter_input", list(
    conditions = list(
      list(type = "values", column = "cyl",
           values = list("4", "6"), mode = "include"),
      list(type = "numeric", column = "mpg", op = ">=", value = 20)
    ),
    operator = "&"
  ))
  baseline <- get_block_result(app, "filter")

  fid <- "board-block_filter-expr-filter_input"
  app$run_js(sprintf(
    "(function(){
       var el = document.getElementById('%s');
       el._block.updateColumns(Object.values(el._block.columnMeta));
       el._block._submit();
     })();", fid))
  app$wait_for_idle()

  after_json <- app$get_js(sprintf(
    "JSON.stringify(document.getElementById('%s')._block.conditions
       .map(function(c){
         return {values: c.values, numValue: c.numValue, op: c.op};
       }))", fid))
  after <- jsonlite::fromJSON(after_json, simplifyVector = FALSE)
  expect_equal(after[[1]]$values, list("4", "6"))
  expect_equal(after[[1]]$op, "is")
  expect_equal(after[[2]]$numValue, 20)
  expect_equal(after[[2]]$op, ">=")

  res <- get_block_result(app, "filter")
  expect_equal(res, baseline)
})

# ===========================================================================
# SELECT (3 tests)
# ===========================================================================

test_that("select: include columns", {
  set_block_state(app, "select", "select_input", list(
    columns = list("mpg", "cyl", "hp"),
    exclude = FALSE,
    distinct = FALSE
  ))
  res <- get_block_result(app, "select")
  expect_equal(ncol(res), 3)
  expect_equal(names(res), c("mpg", "cyl", "hp"))
  expect_equal(nrow(res), 32)
})

test_that("select: exclude mode", {
  set_block_state(app, "select", "select_input", list(
    columns = list("mpg", "cyl"),
    exclude = TRUE,
    distinct = FALSE
  ))
  res <- get_block_result(app, "select")
  expect_false("mpg" %in% names(res))
  expect_false("cyl" %in% names(res))
  expect_equal(ncol(res), 9)
})

test_that("select: distinct rows", {
  set_block_state(app, "select", "select_input", list(
    columns = list("cyl", "gear"),
    exclude = FALSE,
    distinct = TRUE
  ))
  res <- get_block_result(app, "select")
  expect_equal(ncol(res), 2)
  expect_lt(nrow(res), 32)
  expect_equal(nrow(res), nrow(unique(mtcars[, c("cyl", "gear")])))
})

# ===========================================================================
# MUTATE (3 tests)
# ===========================================================================

test_that("mutate: single expression", {
  set_block_state(app, "mutate", "mutate_input", list(
    mutations = list(
      list(name = "kpl", expr = "mpg * 0.425")
    ),
    by = list()
  ))
  res <- get_block_result(app, "mutate")
  expect_true("kpl" %in% names(res))
  expect_equal(res$kpl, mtcars$mpg * 0.425, tolerance = 1e-10)
})

test_that("mutate: two expressions", {
  set_block_state(app, "mutate", "mutate_input", list(
    mutations = list(
      list(name = "kpl", expr = "mpg * 0.425"),
      list(name = "hp_per_cyl", expr = "hp / cyl")
    ),
    by = list()
  ))
  res <- get_block_result(app, "mutate")
  expect_true("kpl" %in% names(res))
  expect_true("hp_per_cyl" %in% names(res))
  expect_equal(res$hp_per_cyl, mtcars$hp / mtcars$cyl, tolerance = 1e-10)
})

test_that("mutate: three expressions with group-by", {
  set_block_state(app, "mutate", "mutate_input", list(
    mutations = list(
      list(name = "kpl", expr = "mpg * 0.425"),
      list(name = "hp_per_cyl", expr = "hp / cyl"),
      list(name = "mpg_centered", expr = "mpg - mean(mpg)")
    ),
    by = list("cyl")
  ))
  res <- get_block_result(app, "mutate")
  expect_true("mpg_centered" %in% names(res))
  # Grouped centering: within each cyl group, mean of centered values ~ 0
  for (g in unique(res$cyl)) {
    centered <- res$mpg_centered[res$cyl == g]
    expect_equal(mean(centered), 0, tolerance = 1e-10)
  }
})

# ===========================================================================
# SUMMARIZE (3 tests)
# ===========================================================================

test_that("summarize: simple function + group by", {
  set_block_state(app, "summarize", "summarize_input", list(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg")
    ),
    by = list("cyl")
  ))
  res <- get_block_result(app, "summarize")
  expect_equal(nrow(res), 3)
  expect_true("avg_mpg" %in% names(res))
  expect_true("cyl" %in% names(res))
})

test_that("summarize: two summaries (mean + n)", {
  set_block_state(app, "summarize", "summarize_input", list(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg"),
      list(type = "simple", name = "n_cars", func = "n", col = "")
    ),
    by = list("cyl")
  ))
  res <- get_block_result(app, "summarize")
  expect_true("avg_mpg" %in% names(res))
  expect_true("n_cars" %in% names(res))
  expect_equal(sum(res$n_cars), 32)
})

test_that("summarize: three summaries (simple + simple + expr) with multi group-by", {
  set_block_state(app, "summarize", "summarize_input", list(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg"),
      list(type = "simple", name = "max_hp", func = "max", col = "hp"),
      list(type = "expr", name = "hp_range", expr = "max(hp) - min(hp)")
    ),
    by = list("cyl", "gear")
  ))
  res <- get_block_result(app, "summarize")
  expect_true(all(c("avg_mpg", "max_hp", "hp_range") %in% names(res)))
  expect_true(all(c("cyl", "gear") %in% names(res)))
  expect_true(all(res$hp_range >= 0))
})

# Regression: updateColumns with unchanged metadata must not reset a
# summary row's col to the first column (breaks func = "n" which has no col).
test_that("summarize: col='' survives updateColumns (restore race)", {
  set_block_state(app, "summarize", "summarize_input", list(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg"),
      list(type = "simple", name = "n_cars", func = "n", col = "")
    ),
    by = list("cyl")
  ))
  baseline <- get_block_result(app, "summarize")

  fid <- "board-block_summarize-expr-summarize_input"
  app$run_js(sprintf(
    "(function(){var el=document.getElementById('%s');
       el._block.updateColumns(Object.values(el._block.columnMeta));
       el._block._submit();})();", fid))
  app$wait_for_idle()

  cols <- jsonlite::fromJSON(app$get_js(sprintf(
    "JSON.stringify(document.getElementById('%s')._block.summaries
       .map(function(s){return s.col;}))", fid)), simplifyVector = FALSE)
  expect_equal(cols[[1]], "mpg")
  expect_equal(cols[[2]], "")

  res <- get_block_result(app, "summarize")
  expect_equal(res, baseline)
})

# ===========================================================================
# ARRANGE (3 tests)
# ===========================================================================

test_that("arrange: single column desc", {
  set_block_state(app, "arrange", "arrange_input", list(
    columns = list(list(column = "mpg", direction = "desc"))
  ))
  res <- get_block_result(app, "arrange")
  expect_equal(nrow(res), 32)
  expect_equal(res$mpg[1], max(mtcars$mpg))
  expect_true(all(diff(res$mpg) <= 0))
})

test_that("arrange: two columns, mixed directions", {
  set_block_state(app, "arrange", "arrange_input", list(
    columns = list(
      list(column = "cyl", direction = "asc"),
      list(column = "mpg", direction = "desc")
    )
  ))
  res <- get_block_result(app, "arrange")
  expect_true(all(diff(res$cyl) >= 0))
  for (g in unique(res$cyl)) {
    mpg_group <- res$mpg[res$cyl == g]
    expect_true(all(diff(mpg_group) <= 0))
  }
})

test_that("arrange: three columns", {
  set_block_state(app, "arrange", "arrange_input", list(
    columns = list(
      list(column = "cyl", direction = "asc"),
      list(column = "gear", direction = "desc"),
      list(column = "mpg", direction = "asc")
    )
  ))
  res <- get_block_result(app, "arrange")
  expect_true(all(diff(res$cyl) >= 0))
})

# ===========================================================================
# RENAME (3 tests)
# ===========================================================================

test_that("rename: single rename", {
  set_block_state(app, "rename", "rename_input", list(
    renames = list(miles_per_gallon = "mpg")
  ))
  res <- get_block_result(app, "rename")
  expect_true("miles_per_gallon" %in% names(res))
  expect_false("mpg" %in% names(res))
})

test_that("rename: two renames", {
  set_block_state(app, "rename", "rename_input", list(
    renames = list(miles_per_gallon = "mpg", cylinders = "cyl")
  ))
  res <- get_block_result(app, "rename")
  expect_true("miles_per_gallon" %in% names(res))
  expect_true("cylinders" %in% names(res))
  expect_false("mpg" %in% names(res))
  expect_false("cyl" %in% names(res))
})

test_that("rename: three renames", {
  set_block_state(app, "rename", "rename_input", list(
    renames = list(
      miles_per_gallon = "mpg",
      cylinders = "cyl",
      horsepower = "hp"
    )
  ))
  res <- get_block_result(app, "rename")
  expect_true(all(c("miles_per_gallon", "cylinders", "horsepower") %in% names(res)))
  expect_false(any(c("mpg", "cyl", "hp") %in% names(res)))
})

# ===========================================================================
# SLICE (4 tests)
# ===========================================================================

test_that("slice: head n rows", {
  set_block_state(app, "slice", "slice_input", list(
    type = "head", n = 5
  ))
  res <- get_block_result(app, "slice")
  expect_equal(nrow(res), 5)
})

test_that("slice: tail with proportion", {
  set_block_state(app, "slice", "slice_input", list(
    type = "tail", prop = 0.1
  ))
  res <- get_block_result(app, "slice")
  expect_equal(nrow(res), 3)
})

test_that("slice: min with order_by", {
  set_block_state(app, "slice", "slice_input", list(
    type = "min", n = 3, order_by = "mpg", with_ties = FALSE
  ))
  res <- get_block_result(app, "slice")
  expect_equal(nrow(res), 3)
  expect_true(all(res$mpg <= sort(mtcars$mpg)[4]))
})

test_that("slice: sample with group-by", {
  set_block_state(app, "slice", "slice_input", list(
    type = "sample", n = 2, by = list("cyl")
  ))
  res <- get_block_result(app, "slice")
  expect_equal(nrow(res), 6)
  for (g in unique(res$cyl)) {
    expect_equal(sum(res$cyl == g), 2)
  }
})

# Regression: updateColumns with unchanged metadata must not default
# weight_by / order_by to the first column when the user left them unset.
test_that("slice: empty weight_by survives updateColumns (restore race)", {
  set_block_state(app, "slice", "slice_input", list(
    type = "min", n = 3L, order_by = "mpg", with_ties = FALSE,
    by = list("cyl")
  ))
  baseline <- get_block_result(app, "slice")

  fid <- "board-block_slice-expr-slice_input"
  app$run_js(sprintf(
    "(function(){var el=document.getElementById('%s');
       el._block.updateColumns(Object.values(el._block.columnMeta));
       el._block._submit();})();", fid))
  app$wait_for_idle()

  st <- jsonlite::fromJSON(app$get_js(sprintf(
    "JSON.stringify({order_by:document.getElementById('%s')._block.order_by,
       weight_by:document.getElementById('%s')._block.weight_by,
       by:document.getElementById('%s')._block.by})", fid, fid, fid)),
    simplifyVector = FALSE)
  expect_equal(st$order_by, "mpg")
  expect_equal(st$weight_by, "")
  expect_equal(st$by, list("cyl"))

  res <- get_block_result(app, "slice")
  expect_equal(res, baseline)
})

# ===========================================================================
# JOIN (3 tests)
# ===========================================================================

test_that("join: left join with key", {
  set_block_state(app, "join", "join_input", list(
    type = "left_join",
    keys = list(list(xCol = "cyl", op = "==", yCol = "Petal.Length")),
    exprs = list(),
    suffix_x = ".x", suffix_y = ".y"
  ))
  res <- get_block_result(app, "join")
  expect_gte(nrow(res), 0)
  expect_true("mpg" %in% names(res))
})

test_that("join: inner join with two keys", {
  set_block_state(app, "join", "join_input", list(
    type = "inner_join",
    keys = list(
      list(xCol = "cyl", op = ">=", yCol = "Petal.Length"),
      list(xCol = "gear", op = "<=", yCol = "Petal.Width")
    ),
    exprs = list(),
    suffix_x = ".x", suffix_y = ".y"
  ))
  res <- get_block_result(app, "join")
  expect_true(is.data.frame(res))
})

test_that("join: anti join", {
  set_block_state(app, "join", "join_input", list(
    type = "anti_join",
    keys = list(list(xCol = "cyl", op = "==", yCol = "Petal.Length")),
    exprs = list(),
    suffix_x = ".x", suffix_y = ".y"
  ))
  res <- get_block_result(app, "join")
  expect_true("mpg" %in% names(res))
  expect_false("Species" %in% names(res))
})

# ===========================================================================
# PIVOT_LONGER (2 tests)
# ===========================================================================

test_that("pivot_longer: basic", {
  set_block_state(app, "pivot_longer", "pivot_longer_input", list(
    cols = list("Sepal.Length", "Sepal.Width"),
    names_to = "measurement",
    values_to = "value",
    values_drop_na = FALSE,
    names_prefix = ""
  ))
  res <- get_block_result(app, "pivot_longer")
  expect_true("measurement" %in% names(res))
  expect_true("value" %in% names(res))
  expect_equal(nrow(res), 300)
})

test_that("pivot_longer: four columns with custom names", {
  set_block_state(app, "pivot_longer", "pivot_longer_input", list(
    cols = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    names_to = "part_dim",
    values_to = "cm",
    values_drop_na = TRUE,
    names_prefix = ""
  ))
  res <- get_block_result(app, "pivot_longer")
  expect_true("part_dim" %in% names(res))
  expect_true("cm" %in% names(res))
  expect_equal(nrow(res), 600)
})

# ===========================================================================
# PIVOT_WIDER (3 tests)
# ===========================================================================

test_that("pivot_wider: basic", {
  # pivot_data: person/measure/value/unit — pivot measure into columns
  set_block_state(app, "pivot_wider", "pivot_wider_input", list(
    names_from = list("measure"),
    values_from = list("value"),
    id_cols = list("person"),
    values_fill = NULL,
    names_sep = "_",
    names_prefix = ""
  ))
  res <- get_block_result(app, "pivot_wider")
  expect_true("height" %in% names(res))
  expect_true("weight" %in% names(res))
  expect_equal(nrow(res), 3)
})

test_that("pivot_wider: with prefix and multiple values_from", {
  set_block_state(app, "pivot_wider", "pivot_wider_input", list(
    names_from = list("measure"),
    values_from = list("value", "unit"),
    id_cols = list(),
    values_fill = NULL,
    names_sep = "_",
    names_prefix = "m_"
  ))
  res <- get_block_result(app, "pivot_wider")
  prefixed_cols <- grep("m_", names(res), value = TRUE)
  expect_true(length(prefixed_cols) > 0)
})

test_that("pivot_wider: values_fn aggregates duplicates", {
  # pivot_data has person+unit as implicit IDs, giving 6 rows without id_cols.
  # With values_fn = "first", duplicates get resolved to scalars.
  set_block_state(app, "pivot_wider", "pivot_wider_input", list(
    names_from = list("measure"),
    values_from = list("value"),
    id_cols = list("person"),
    values_fill = NULL,
    names_sep = "_",
    names_prefix = "",
    values_fn = "first"
  ))
  res <- get_block_result(app, "pivot_wider")
  expect_true("height" %in% names(res))
  expect_true("weight" %in% names(res))
  expect_equal(nrow(res), 3)
  # Scalars, not list-cols
  expect_true(is.numeric(res$height))
})

# ===========================================================================
# UNITE (2 tests)
# ===========================================================================

test_that("unite: basic with remove", {
  set_block_state(app, "unite", "unite_input", list(
    col = "cyl_gear",
    cols = list("cyl", "gear"),
    sep = "_",
    remove = TRUE,
    na_rm = FALSE
  ))
  res <- get_block_result(app, "unite")
  expect_true("cyl_gear" %in% names(res))
  expect_false("cyl" %in% names(res))
  expect_false("gear" %in% names(res))
  expect_true(all(grepl("^\\d+_\\d+$", res$cyl_gear)))
})

test_that("unite: three columns, keep originals, custom separator", {
  set_block_state(app, "unite", "unite_input", list(
    col = "engine",
    cols = list("cyl", "vs", "am"),
    sep = "-",
    remove = FALSE,
    na_rm = FALSE
  ))
  res <- get_block_result(app, "unite")
  expect_true("engine" %in% names(res))
  expect_true(all(c("cyl", "vs", "am") %in% names(res)))
  expect_true(all(grepl("-", res$engine)))
})

# ===========================================================================
# SEPARATE (2 tests)
# ===========================================================================

test_that("separate: basic", {
  set_block_state(app, "separate", "separate_input", list(
    col = "Species",
    into = list("part1", "part2"),
    sep = "i",
    remove = TRUE,
    convert = FALSE
  ))
  res <- get_block_result(app, "separate")
  expect_true("part1" %in% names(res))
  expect_true("part2" %in% names(res))
  expect_false("Species" %in% names(res))
})

test_that("separate: keep original + convert", {
  set_block_state(app, "separate", "separate_input", list(
    col = "Species",
    into = list("a", "b"),
    sep = "e",
    remove = FALSE,
    convert = TRUE
  ))
  res <- get_block_result(app, "separate")
  expect_true("a" %in% names(res))
  expect_true("b" %in% names(res))
  expect_true("Species" %in% names(res))
})

# ===========================================================================
# BIND_ROWS (2 tests)
# ===========================================================================

test_that("bind_rows: without id column", {
  set_block_state(app, "bind_rows", "bind_rows_input", list(
    id_name = NULL
  ))
  res <- get_block_result(app, "bind_rows")
  expect_equal(nrow(res), 32 + 150)
})

test_that("bind_rows: with id column", {
  set_block_state(app, "bind_rows", "bind_rows_input", list(
    id_name = "source"
  ))
  res <- get_block_result(app, "bind_rows")
  expect_equal(nrow(res), 32 + 150)
  expect_true("source" %in% names(res))
})

# ===========================================================================
# BIND_COLS (1 test)
# ===========================================================================

test_that("bind_cols: default", {
  set_block_state(app, "bind_cols", "bind_cols_input", list())
  res <- get_block_result(app, "bind_cols")
  # Both inputs are mtcars (32 rows, 11 cols each) = 22 cols
  expect_equal(ncol(res), 22)
})

# ===========================================================================
# AUTO-UNBOX REGRESSION (2 tests)
# ===========================================================================

test_that("auto-unbox: single-column data has correct JS columnNames", {
  # When data has 1 column, colnames() is length-1 and auto_unbox would
  # turn it into a scalar string. JS must still see an array.
  app$wait_for_idle()
  col_names_json <- app$get_js(
    "(function() {
       var el = document.getElementById('board-block_select_single-expr-select_input');
       if (!el || !el._block) return null;
       return JSON.stringify(el._block.columnNames);
     })();"
  )
  skip_if(is.null(col_names_json), "select_single block not initialized")
  parsed <- jsonlite::fromJSON(col_names_json)
  expect_equal(parsed, "x")
  expect_length(parsed, 1)
})

test_that("auto-unbox: single unique value has correct JS filter meta", {
  # When a column has 1 unique value, the values array would be auto-unboxed
  # to a scalar string, causing the dropdown to show individual characters.
  app$wait_for_idle()
  vals_json <- app$get_js(
    "(function() {
       var el = document.getElementById('board-block_filter_single-expr-filter_input');
       if (!el || !el._block) return null;
       var meta = el._block.columnMeta['id'];
       return JSON.stringify(meta ? meta.values : null);
     })();"
  )
  skip_if(is.null(vals_json), "filter_single block not initialized")
  parsed <- jsonlite::fromJSON(vals_json)
  expect_equal(parsed, "ONLY")
  expect_length(parsed, 1)
})
