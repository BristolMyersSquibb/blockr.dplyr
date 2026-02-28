# Tests for rename_block external_ctrl (AI control integration).

test_that("rename_block state has reactiveVal objects", {
  block <- new_rename_block(
    renames = list(new_col = "mpg")
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

      expect_true("renames" %in% holder$ctrl_names)
      expect_true(inherits(holder$vars$renames, "reactiveVal"))
    }
  )
})

test_that("setting ctrl reactiveVals updates expr", {
  block <- new_rename_block(
    renames = list(new_col = "mpg")
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
      holder$vars$renames(list(horsepower = "hp"))

      session$flushReact()
      Sys.sleep(0.5)
      session$flushReact()

      expr_str <- paste(deparse(isolate(holder$expr())), collapse = "")
      expect_match(expr_str, "horsepower")
    }
  )
})
