# Tests for summarize_block external_ctrl (AI control integration).
#
# These tests verify that:
# 1. ctrl_block sees reactiveVal objects in state
# 2. Setting ctrl reactiveVals updates the block expr and output
# 3. Stale form inputs don't clobber external updates

test_that("summarize_block state has reactiveVal objects", {
  block <- new_summarize_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by = "cyl"
  )

  holder <- new.env(parent = emptyenv())

  test_ctrl <- blockr.core::ctrl_block(
    server = function(id, x, vars, data, eval) {
      moduleServer(id, function(input, output, session) {
        holder$ctrl_names <- names(Filter(
          function(v) inherits(v, "reactiveVal"), vars
        ))
        holder$vars <- vars
        holder$expr <- eval
        reactive(TRUE)
      })
    },
    ui = function(id, x) tagList()
  )

  testServer(
    blockr.core::block_server,
    args = list(
      x = block,
      data = list(data = reactive(mtcars)),
      ctrl_block = test_ctrl
    ),
    {
      session$flushReact()
      Sys.sleep(0.5)
      session$flushReact()

      expect_true("summaries" %in% holder$ctrl_names)
      expect_true("by" %in% holder$ctrl_names)
      expect_true(inherits(holder$vars$summaries, "reactiveVal"))
      expect_true(inherits(holder$vars$by, "reactiveVal"))
    }
  )
})

test_that("setting ctrl reactiveVals updates expr", {
  block <- new_summarize_block(
    summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
    by = "cyl"
  )

  holder <- new.env(parent = emptyenv())

  test_ctrl <- blockr.core::ctrl_block(
    server = function(id, x, vars, data, eval) {
      moduleServer(id, function(input, output, session) {
        holder$vars <- vars
        holder$expr <- eval
        reactive(TRUE)
      })
    },
    ui = function(id, x) tagList()
  )

  testServer(
    blockr.core::block_server,
    args = list(
      x = block,
      data = list(data = reactive(mtcars)),
      ctrl_block = test_ctrl
    ),
    {
      session$flushReact()
      Sys.sleep(0.5)
      session$flushReact()

      # Set new values
      new_summaries <- list(
        mean_hp = list(func = "mean", col = "hp"),
        mean_wt = list(func = "mean", col = "wt")
      )
      holder$vars$summaries(new_summaries)
      holder$vars$by(c("cyl", "gear"))

      session$flushReact()

      expr_str <- paste(deparse(isolate(holder$expr())), collapse = "")
      expect_match(expr_str, "mean_hp")
      expect_match(expr_str, "mean_wt")
      expect_match(expr_str, "gear")
    }
  )
})

test_that("mod_multi_summarize_server reflects external updates", {
  ext_rv <- reactiveVal(list(
    avg_mpg = list(func = "mean", col = "mpg")
  ))

  testServer(
    mod_multi_summarize_server,
    args = list(
      get_value = function() ext_rv(),
      get_cols = function() colnames(mtcars),
      external_value = ext_rv
    ),
    {
      session$flushReact()
      Sys.sleep(0.3)
      session$flushReact()

      initial <- session$returned()
      expect_equal(names(initial), "avg_mpg")

      # Simulate AI setting new value
      new_summaries <- list(
        mean_hp = list(func = "mean", col = "hp"),
        mean_wt = list(func = "mean", col = "wt")
      )
      ext_rv(new_summaries)
      session$flushReact()
      Sys.sleep(0.3)
      session$flushReact()

      after <- session$returned()
      expect_equal(names(after), c("mean_hp", "mean_wt"))
      expect_equal(after$mean_hp$col, "hp")
    }
  )
})

test_that("stale form inputs don't clobber external updates", {
  ext_rv <- reactiveVal(list(
    avg_mpg = list(func = "mean", col = "mpg")
  ))

  testServer(
    mod_multi_summarize_server,
    args = list(
      get_value = function() ext_rv(),
      get_cols = function() colnames(mtcars),
      external_value = ext_rv
    ),
    {
      session$flushReact()
      Sys.sleep(0.3)
      session$flushReact()

      # Simulate stale form inputs (as in live app after initial render)
      session$setInputs(
        summary_1_new = "avg_mpg",
        summary_1_func = "mean",
        summary_1_col = "mpg"
      )
      session$flushReact()
      Sys.sleep(0.3)
      session$flushReact()

      # AI sets new value
      new_summaries <- list(
        mean_hp = list(func = "mean", col = "hp"),
        mean_wt = list(func = "mean", col = "wt")
      )
      ext_rv(new_summaries)
      session$flushReact()
      Sys.sleep(0.5)
      session$flushReact()
      Sys.sleep(0.3)
      session$flushReact()

      # ext_rv must NOT be clobbered by stale form values
      expect_equal(ext_rv(), new_summaries)
      # Module output must reflect new values
      expect_equal(names(session$returned()), c("mean_hp", "mean_wt"))
    }
  )
})
