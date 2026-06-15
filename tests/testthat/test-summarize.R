# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("summarize block: empty summaries pass data through unchanged", {
  blk <- new_summarize_block(
    summaries = list(), by = list()
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    # An unconfigured block is a no-op: it must not collapse the data
    # to a 1-row summary and wreck everything downstream.
    expect_identical(result, mtcars)
  })
})

test_that("summarize block: simple mean produces correct value", {
  blk <- new_summarize_block(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg")
    ),
    by = list()
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), 1)
    expect_true("avg_mpg" %in% colnames(result))
    expect_equal(result$avg_mpg, mean(mtcars$mpg))
  })
})

test_that("summarize block: grouped summary produces correct groups", {
  blk <- new_summarize_block(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg")
    ),
    by = list("cyl")
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), length(unique(mtcars$cyl)))
    expect_true("cyl" %in% colnames(result))
    expect_true("avg_mpg" %in% colnames(result))
    # Verify values
    for (cy in unique(mtcars$cyl)) {
      expected <- mean(mtcars$mpg[mtcars$cyl == cy])
      actual <- result$avg_mpg[result$cyl == cy]
      expect_equal(actual, expected)
    }
  })
})

test_that("summarize block: expr mode works", {
  blk <- new_summarize_block(
    summaries = list(
      list(type = "expr", name = "mpg_range", expr = "max(mpg) - min(mpg)")
    ),
    by = list()
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), 1)
    expect_true("mpg_range" %in% colnames(result))
    expect_equal(result$mpg_range, max(mtcars$mpg) - min(mtcars$mpg))
  })
})

test_that("summarize block: multiple summaries", {
  blk <- new_summarize_block(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg"),
      list(type = "simple", name = "max_hp", func = "max", col = "hp"),
      list(type = "simple", name = "count", func = "n", col = "")
    ),
    by = list()
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result), 1)
    expect_equal(result$avg_mpg, mean(mtcars$mpg))
    expect_equal(result$max_hp, max(mtcars$hp))
    expect_equal(result$count, nrow(mtcars))
  })
})

test_that("summarize block: state change from ungrouped to grouped", {
  blk <- new_summarize_block(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg")
    ),
    by = list()
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result1), 1)

    # Add grouping
    session$returned$state$summaries(list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg")
    ))
    session$returned$state$by(list("cyl"))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(nrow(result2), length(unique(mtcars$cyl)))
  })
})
