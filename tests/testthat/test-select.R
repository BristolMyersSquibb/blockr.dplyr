# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("select block: empty columns pass all data through", {
  blk <- new_select_block(
    state = list(columns = list(), exclude = FALSE, distinct = FALSE)
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(ncol(result), ncol(mtcars))
    expect_equal(colnames(result), colnames(mtcars))
  })
})

test_that("select block: select specific columns", {
  blk <- new_select_block(
    state = list(
      columns = list("mpg", "cyl"),
      exclude = FALSE, distinct = FALSE
    )
  )

  testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars)),
    {
      session$flushReact()
      result <- eval_bquoted(
        session$returned$expr(), mtcars
      )
      expect_equal(colnames(result), c("mpg", "cyl"))
      expect_equal(nrow(result), nrow(mtcars))
    }
  )
})

test_that("select block: exclude mode removes specified columns", {
  blk <- new_select_block(
    state = list(columns = list("mpg", "cyl"), exclude = TRUE, distinct = FALSE)
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_false("mpg" %in% colnames(result))
    expect_false("cyl" %in% colnames(result))
    expect_equal(ncol(result), ncol(mtcars) - 2)
  })
})

test_that("select block: distinct deduplicates rows", {
  df <- data.frame(a = c(1, 1, 2, 2), b = c("x", "x", "y", "y"))
  blk <- new_select_block(
    state = list(columns = list("a"), exclude = FALSE, distinct = TRUE)
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_equal(nrow(result), 2)
    expect_equal(colnames(result), "a")
    expect_equal(result$a, c(1, 2))
  })
})

test_that("select block: state change from include to exclude", {
  blk <- new_select_block(
    state = list(
      columns = list("mpg", "cyl"),
      exclude = FALSE, distinct = FALSE
    )
  )

  testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars)),
    {
      session$flushReact()
      result1 <- eval_bquoted(
        session$returned$expr(), mtcars
      )
      expect_equal(colnames(result1), c("mpg", "cyl"))

      # Switch to exclude mode
      session$returned$state$state(list(
        columns = list("mpg", "cyl"),
        exclude = TRUE, distinct = FALSE
      ))
      session$flushReact()
      result2 <- eval_bquoted(
        session$returned$expr(), mtcars
      )
      expect_false("mpg" %in% colnames(result2))
      expect_false("cyl" %in% colnames(result2))
    }
  )
})
