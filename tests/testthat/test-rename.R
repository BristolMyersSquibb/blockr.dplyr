# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("rename block: empty renames pass data through", {
  blk <- new_rename_block(
    state = list(renames = list())
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(result, mtcars)
  })
})

test_that("rename block: rename a single column", {
  blk <- new_rename_block(
    state = list(renames = list(miles_per_gallon = "mpg"))
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("miles_per_gallon" %in% colnames(result))
    expect_false("mpg" %in% colnames(result))
    expect_equal(result$miles_per_gallon, mtcars$mpg)
  })
})

test_that("rename block: rename multiple columns", {
  blk <- new_rename_block(
    state = list(renames = list(
      miles_per_gallon = "mpg",
      cylinders = "cyl"
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("miles_per_gallon" %in% colnames(result))
    expect_true("cylinders" %in% colnames(result))
    expect_false("mpg" %in% colnames(result))
    expect_false("cyl" %in% colnames(result))
  })
})

test_that("rename block: state change updates column names", {
  blk <- new_rename_block(
    state = list(renames = list(miles_per_gallon = "mpg"))
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("miles_per_gallon" %in% colnames(result1))

    # Change rename
    session$returned$state$state(list(renames = list(
      horse_power = "hp"
    )))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("horse_power" %in% colnames(result2))
    expect_false("hp" %in% colnames(result2))
    # mpg should be back to original name
    expect_true("mpg" %in% colnames(result2))
  })
})
