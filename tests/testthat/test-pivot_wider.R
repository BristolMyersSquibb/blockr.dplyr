# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("pivot_wider block: empty names_from/values_from pass data through", {
  blk <- new_pivot_wider_block(
    state = list(
      names_from = list(), values_from = list(),
      id_cols = list(), values_fill = NULL,
      names_sep = "_", names_prefix = ""
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(result, mtcars)
  })
})

test_that("pivot_wider block: spreads values to wide format", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    measure = c("height", "weight", "height", "weight"),
    value = c(170, 70, 180, 85),
    stringsAsFactors = FALSE
  )

  blk <- new_pivot_wider_block(
    state = list(
      names_from = "measure",
      values_from = "value",
      id_cols = "id",
      values_fill = NULL,
      names_sep = "_",
      names_prefix = ""
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_equal(nrow(result), 2)
    expect_true("height" %in% colnames(result))
    expect_true("weight" %in% colnames(result))
    expect_equal(result$height, c(170, 180))
    expect_equal(result$weight, c(70, 85))
  })
})

test_that("pivot_wider block: names_prefix adds prefix", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    key = c("a", "b", "a", "b"),
    val = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )

  blk <- new_pivot_wider_block(
    state = list(
      names_from = "key",
      values_from = "val",
      id_cols = "id",
      values_fill = NULL,
      names_sep = "_",
      names_prefix = "col_"
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_true("col_a" %in% colnames(result))
    expect_true("col_b" %in% colnames(result))
  })
})

test_that("pivot_wider block: state change updates pivot", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    measure = c("height", "weight", "height", "weight"),
    value = c(170, 70, 180, 85),
    stringsAsFactors = FALSE
  )

  blk <- new_pivot_wider_block(
    state = list(
      names_from = "measure",
      values_from = "value",
      id_cols = "id",
      values_fill = NULL,
      names_sep = "_",
      names_prefix = ""
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), df)
    expect_equal(ncol(result1), 3)  # id, height, weight

    # Add a prefix
    session$returned$state$state(list(
      names_from = "measure",
      values_from = "value",
      id_cols = "id",
      values_fill = NULL,
      names_sep = "_",
      names_prefix = "val_"
    ))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), df)
    expect_true("val_height" %in% colnames(result2))
    expect_true("val_weight" %in% colnames(result2))
  })
})
