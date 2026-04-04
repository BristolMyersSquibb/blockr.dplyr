# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("mutate block: empty mutations pass data through", {
  blk <- new_mutate_block(
    state = list(mutations = list(list(name = "", expr = "")), by = list())
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(result, mtcars)
  })
})

test_that("mutate block: add a new computed column", {
  blk <- new_mutate_block(
    state = list(
      mutations = list(
        list(name = "kpl", expr = "mpg * 0.425144")
      ),
      by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("kpl" %in% colnames(result))
    expect_equal(result$kpl, mtcars$mpg * 0.425144)
  })
})

test_that("mutate block: add multiple columns", {
  blk <- new_mutate_block(
    state = list(
      mutations = list(
        list(name = "hp_per_cyl", expr = "hp / cyl"),
        list(name = "wt_kg", expr = "wt * 453.592")
      ),
      by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("hp_per_cyl" %in% colnames(result))
    expect_true("wt_kg" %in% colnames(result))
    expect_equal(result$hp_per_cyl, mtcars$hp / mtcars$cyl)
    expect_equal(result$wt_kg, mtcars$wt * 453.592)
  })
})

test_that("mutate block: grouped mutate with .by", {
  blk <- new_mutate_block(
    state = list(
      mutations = list(
        list(name = "mean_mpg", expr = "mean(mpg)")
      ),
      by = list("cyl")
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("mean_mpg" %in% colnames(result))
    # Each row should have the group mean, not the overall mean
    for (cy in unique(result$cyl)) {
      group_mean <- mean(mtcars$mpg[mtcars$cyl == cy])
      expect_equal(
        unique(result$mean_mpg[result$cyl == cy]),
        group_mean
      )
    }
  })
})

test_that("mutate block: state change adds different column", {
  blk <- new_mutate_block(
    state = list(
      mutations = list(
        list(name = "col_a", expr = "mpg * 2")
      ),
      by = list()
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("col_a" %in% colnames(result1))

    # Change to different mutation
    session$returned$state$state(list(
      mutations = list(
        list(name = "col_b", expr = "hp + wt")
      ),
      by = list()
    ))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), mtcars)
    expect_true("col_b" %in% colnames(result2))
    expect_false("col_a" %in% colnames(result2))
    expect_equal(result2$col_b, mtcars$hp + mtcars$wt)
  })
})
