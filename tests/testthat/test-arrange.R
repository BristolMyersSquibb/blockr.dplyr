# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("arrange block: empty columns pass data through unchanged", {
  blk <- new_arrange_block(
    state = list(columns = list())
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(result, mtcars)
  })
})

test_that("arrange block: ascending sort orders correctly", {
  blk <- new_arrange_block(
    state = list(columns = list(
      list(column = "mpg", direction = "asc")
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true(all(diff(result$mpg) >= 0))
  })
})

test_that("arrange block: descending sort orders correctly", {
  blk <- new_arrange_block(
    state = list(columns = list(
      list(column = "mpg", direction = "desc")
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true(all(diff(result$mpg) <= 0))
  })
})

test_that("arrange block: multi-column sort", {
  blk <- new_arrange_block(
    state = list(columns = list(
      list(column = "cyl", direction = "asc"),
      list(column = "mpg", direction = "desc")
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    # cyl should be non-decreasing
    expect_true(all(diff(result$cyl) >= 0))
    # Within each cyl group, mpg should be non-increasing
    for (cy in unique(result$cyl)) {
      sub <- result$mpg[result$cyl == cy]
      expect_true(all(diff(sub) <= 0))
    }
  })
})

test_that("arrange block: state change from asc to desc", {
  blk <- new_arrange_block(
    state = list(columns = list(
      list(column = "mpg", direction = "asc")
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true(all(diff(result1$mpg) >= 0))

    # Change to descending
    session$returned$state$state(list(columns = list(
      list(column = "mpg", direction = "desc")
    )))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true(all(diff(result2$mpg) <= 0))
  })
})
