# Tests for summarize_expr_block external_ctrl (AI control integration).

test_that("summarize_expr_block state has reactiveVal objects", {
  block <- new_summarize_expr_block(
    exprs = list(count = "dplyr::n()"),
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

      expect_true("exprs" %in% holder$ctrl_names)
      expect_true("by" %in% holder$ctrl_names)
      expect_true(inherits(holder$vars$exprs, "reactiveVal"))
    }
  )
})

test_that("setting ctrl reactiveVals updates expr", {
  block <- new_summarize_expr_block(
    exprs = list(count = "dplyr::n()"),
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

      # Set new values via ctrl reactiveVal
      holder$vars$exprs(list(mean_hp = "mean(hp)"))

      session$flushReact()
      Sys.sleep(0.5)
      session$flushReact()

      expr_str <- paste(deparse(isolate(holder$expr())), collapse = "")
      expect_match(expr_str, "mean_hp")
    }
  )
})
