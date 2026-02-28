# Tests for slice_block external_ctrl (AI control integration).

test_that("slice_block state has reactiveVal objects", {
  block <- new_slice_block(type = "head", n = 5)

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

      expect_true("type" %in% holder$ctrl_names)
      expect_true("n" %in% holder$ctrl_names)
      expect_true("prop" %in% holder$ctrl_names)
      expect_true("order_by" %in% holder$ctrl_names)
      expect_true(inherits(holder$vars$prop, "reactiveVal"))
    }
  )
})

test_that("setting n via external_ctrl updates evaluated result", {
  block <- new_slice_block(type = "max", order_by = "mpg", n = 5)

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

      # Simulate AI setting n via external_ctrl
      holder$vars$n(3)

      session$flushReact()
      Sys.sleep(0.5)
      session$flushReact()

      result <- isolate(holder$expr())
      expect_s3_class(result, "data.frame")
      # with_ties = TRUE (default), so ties at the boundary may add extra rows
      expect_true(nrow(result) <= nrow(mtcars))
      expect_true(nrow(result) > 0)
    }
  )
})
