# Tests for R/legacy-deser.R — restoring boards saved by old blockr.dplyr
# versions. Payload fixtures mirror what jsonlite gives blockr.core when it
# reads an old board JSON: old per-parameter payloads (no `state` wrapper).

eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

# Serialized form of a block saved by an old version: `cls` is the class
# vector recorded in the JSON, `ctor` the (possibly no longer existing)
# constructor name, `payload` the old per-parameter fields.
legacy_json <- function(cls, ctor, payload) {
  list(
    object = c(cls, "transform_block", "block", "vctrs_vctr", "list"),
    payload = payload,
    constructor = list(
      object = "blockr_ctor",
      constructor = ctor,
      package = "blockr.dplyr",
      version = "0.1.0"
    )
  )
}

# Deserialize and return the migrated state (what the new ctor received).
restored_state <- function(data) {
  blk <- blockr.core::blockr_deser(data)
  blockr.core::blockr_ser(blk)$payload$state
}

test_that("legacy filter_block payload migrates to typed conditions", {
  data <- legacy_json(
    "filter_block", "new_filter_block",
    list(
      conditions = list(
        list(column = "Species", values = list("setosa"), mode = "include")
      ),
      operator = "&",
      preserve_order = FALSE
    )
  )
  state <- restored_state(data)
  expect_length(state$conditions, 1)
  cond <- state$conditions[[1]]
  expect_equal(cond$type, "values")
  expect_equal(cond$column, "Species")
  expect_equal(unlist(cond$values), "setosa")

  expr <- make_filter_expr(state$conditions, state$operator)
  expect_equal(nrow(eval_bquoted(expr, iris)), 50)
})

test_that("legacy filter_expr_block (removed ctor) migrates to expr condition", {
  data <- legacy_json(
    "filter_expr_block", "new_filter_expr_block",
    list(exprs = "Sepal.Length > 7")
  )
  blk <- blockr.core::blockr_deser(data)
  # Class is restored verbatim so blockr.core's round-trip check passes
  expect_s3_class(blk, "filter_expr_block")
  state <- blockr.core::blockr_ser(blk)$payload$state
  expect_equal(state$conditions[[1]]$type, "expr")
  expect_equal(state$conditions[[1]]$expr, "Sepal.Length > 7")
})

test_that("legacy mutate_expr_block migrates named exprs to mutations", {
  data <- legacy_json(
    "mutate_expr_block", "new_mutate_expr_block",
    list(exprs = list(ratio = "mpg / wt"), by = list())
  )
  state <- restored_state(data)
  expect_equal(state$mutations[[1]]$name, "ratio")
  expect_equal(state$mutations[[1]]$expr, "mpg / wt")
})

test_that("legacy summarize_block migrates named summaries", {
  data <- legacy_json(
    "summarize_block", "new_summarize_block",
    list(
      summaries = list(avg = list(func = "mean", col = "mpg")),
      by = list("cyl")
    )
  )
  state <- restored_state(data)
  s <- state$summaries[[1]]
  expect_equal(s$type, "simple")
  expect_equal(s$name, "avg")
  expect_equal(s$func, "mean")
  expect_equal(s$col, "mpg")
  expect_equal(unlist(state$by), "cyl")

  expr <- make_summarize_expr(state$summaries, unlist(state$by))
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), 3)
})

test_that("legacy summarize_expr_block migrates exprs", {
  data <- legacy_json(
    "summarize_expr_block", "new_summarize_expr_block",
    list(exprs = list(total = "sum(mpg)"), by = list())
  )
  state <- restored_state(data)
  expect_equal(state$summaries[[1]]$type, "expr")
  expect_equal(state$summaries[[1]]$expr, "sum(mpg)")
})

test_that("legacy select/arrange/rename payloads migrate", {
  sel <- restored_state(legacy_json(
    "select_block", "new_select_block",
    list(columns = list("mpg", "cyl"), exclude = FALSE, distinct = TRUE)
  ))
  expect_equal(unlist(sel$columns), c("mpg", "cyl"))
  expect_true(sel$distinct)

  arr <- restored_state(legacy_json(
    "arrange_block", "new_arrange_block",
    list(columns = list(list(column = "mpg", direction = "desc")))
  ))
  expect_equal(arr$columns[[1]]$direction, "desc")

  ren <- restored_state(legacy_json(
    "rename_block", "new_rename_block",
    list(renames = list(miles = "mpg"))
  ))
  expect_equal(ren$renames$miles, "mpg")
})

test_that("legacy slice_block payload migrates", {
  state <- restored_state(legacy_json(
    "slice_block", "new_slice_block",
    list(type = "max", n = 3L, order_by = "mpg", with_ties = FALSE)
  ))
  expect_equal(state$type, "max")
  expect_equal(state$order_by, "mpg")
  expect_false(state$with_ties)
})

test_that("legacy join_block by-columns migrate to equi keys", {
  state <- restored_state(legacy_json(
    "join_block", "new_join_block",
    list(type = "inner_join", by = list("id"))
  ))
  expect_equal(state$type, "inner_join")
  key <- state$keys[[1]]
  expect_equal(key$xCol, "id")
  expect_equal(key$yCol, "id")
  expect_equal(key$op, "==")
})

test_that("legacy bind_rows/pivot/unite/separate payloads migrate", {
  br <- restored_state(legacy_json(
    "bind_rows_block", "new_bind_rows_block",
    list(id_name = "src")
  ))
  expect_equal(br$id_name, "src")

  pl <- restored_state(legacy_json(
    "pivot_longer_block", "new_pivot_longer_block",
    list(cols = list("a", "b"), names_to = "k", values_to = "v")
  ))
  expect_equal(pl$names_to, "k")

  pw <- restored_state(legacy_json(
    "pivot_wider_block", "new_pivot_wider_block",
    list(names_from = list("k"), values_from = list("v"))
  ))
  expect_equal(unlist(pw$names_from), "k")

  un <- restored_state(legacy_json(
    "unite_block", "new_unite_block",
    list(col = "full", cols = list("a", "b"), sep = "-")
  ))
  expect_equal(un$col, "full")
  expect_equal(un$sep, "-")

  sp <- restored_state(legacy_json(
    "separate_block", "new_separate_block",
    list(col = "full", into = list("a", "b"))
  ))
  expect_equal(unlist(sp$into), c("a", "b"))
})

test_that("new-format payload (with state) passes through unchanged", {
  state_in <- list(
    conditions = list(list(
      type = "values", column = "cyl", values = list("4"),
      mode = "include", colType = "numeric"
    )),
    operator = "&"
  )
  data <- legacy_json(
    "filter_block", "new_filter_block",
    list(state = state_in)
  )
  state <- restored_state(data)
  expect_equal(state$conditions[[1]]$column, "cyl")
  expect_equal(state$conditions[[1]]$colType, "numeric")
})
