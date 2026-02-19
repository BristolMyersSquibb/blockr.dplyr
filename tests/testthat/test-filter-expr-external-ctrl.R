# Tests for filter_expr_block external_ctrl (AI control integration).

test_that("filter_expr_block state has reactiveVal objects", {
  block <- new_filter_expr_block(
    exprs = "TRUE"
  )

  holder <- new.env(parent = emptyenv())

  test_ctrl <- blockr.core::ctrl_block(
    server = function(id, x, vars, dat, expr) {
      moduleServer(id, function(input, output, session) {
        holder$ctrl_names <- names(Filter(
          function(v) inherits(v, "reactiveVal"), vars
        ))
        holder$vars <- vars
        holder$expr <- expr
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
      expect_true(inherits(holder$vars$exprs, "reactiveVal"))
    }
  )
})

test_that("setting ctrl reactiveVals updates expr", {
  block <- new_filter_expr_block(
    exprs = "TRUE"
  )

  holder <- new.env(parent = emptyenv())

  test_ctrl <- blockr.core::ctrl_block(
    server = function(id, x, vars, dat, expr) {
      moduleServer(id, function(input, output, session) {
        holder$vars <- vars
        holder$expr <- expr
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
      holder$vars$exprs("mpg > 20")

      session$flushReact()
      Sys.sleep(0.5)
      session$flushReact()

      expr_str <- paste(deparse(isolate(holder$expr())), collapse = "")
      expect_match(expr_str, "mpg")
    }
  )
})
