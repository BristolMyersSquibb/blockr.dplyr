# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("slice block: default head returns first n rows", {
  blk <- new_slice_block(
    state = list(
      type = "head", n = 5L, prop = NULL,
      order_by = "", with_ties = TRUE,
      weight_by = "", replace = FALSE, by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), 5)
    expect_equal(result, head(mtcars, 5))
  })
})

test_that("slice block: tail returns last n rows", {
  blk <- new_slice_block(
    state = list(
      type = "tail", n = 3L, prop = NULL,
      order_by = "", with_ties = TRUE,
      weight_by = "", replace = FALSE, by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), 3)
    expect_equal(result, tail(mtcars, 3), ignore_attr = TRUE)
  })
})

test_that("slice block: sample returns correct count", {
  blk <- new_slice_block(
    state = list(
      type = "sample", n = 10L, prop = NULL,
      order_by = "", with_ties = TRUE,
      weight_by = "", replace = FALSE, by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), 10)
    expect_equal(ncol(result), ncol(mtcars))
  })
})

test_that("slice block: min returns rows with smallest values", {
  blk <- new_slice_block(
    state = list(
      type = "min", n = 5L, prop = NULL,
      order_by = "mpg", with_ties = FALSE,
      weight_by = "", replace = FALSE, by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), 5)
    # All values should be <= the 5th smallest
    sorted_mpg <- sort(mtcars$mpg)
    expect_true(all(result$mpg <= sorted_mpg[5]))
  })
})

test_that("slice block: prop-based slicing", {
  blk <- new_slice_block(
    state = list(
      type = "head", n = 5L, prop = 0.25,
      order_by = "", with_ties = TRUE,
      weight_by = "", replace = FALSE, by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), floor(nrow(mtcars) * 0.25))
  })
})

test_that("slice block: state change from head to tail", {
  blk <- new_slice_block(
    state = list(
      type = "head", n = 5L, prop = NULL,
      order_by = "", with_ties = TRUE,
      weight_by = "", replace = FALSE, by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(result1, head(mtcars, 5))

    # Switch to tail
    session$returned$state$state(list(
      type = "tail", n = 3L, prop = NULL,
      order_by = "", with_ties = TRUE,
      weight_by = "", replace = FALSE, by = list()
    ))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result2), 3)
    expect_equal(result2, tail(mtcars, 3), ignore_attr = TRUE)
  })
})
