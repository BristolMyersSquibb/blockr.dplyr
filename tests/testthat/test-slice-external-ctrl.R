# Tests for slice_block external_ctrl (AI control integration).

test_that("slice_block state has reactiveVal objects", {
  block <- new_slice_block(type = "head", n = 5)

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

      expect_true("type" %in% holder$ctrl_names)
      expect_true("n" %in% holder$ctrl_names)
      expect_true("prop" %in% holder$ctrl_names)
      expect_true("order_by" %in% holder$ctrl_names)
      expect_true(inherits(holder$vars$prop, "reactiveVal"))
    }
  )
})

test_that("setting prop via external_ctrl updates expr to use prop", {
  block <- new_slice_block(type = "max", order_by = "mpg", n = 5)

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

      # Simulate AI setting prop via external_ctrl
      holder$vars$prop(0.05)

      session$flushReact()
      Sys.sleep(0.5)
      session$flushReact()

      expr_str <- paste(deparse(isolate(holder$expr())), collapse = "")
      expect_match(expr_str, "prop")
      expect_match(expr_str, "0.05")
    }
  )
})
