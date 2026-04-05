# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("pivot_longer block: empty cols pass data through", {
  blk <- new_pivot_longer_block(
    state = list(
      cols = list(), names_to = "name",
      values_to = "value", values_drop_na = FALSE, names_prefix = ""
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(result, mtcars)
  })
})

test_that("pivot_longer block: pivots selected columns to long format", {
  df <- data.frame(
    id = 1:3,
    height = c(170, 180, 165),
    weight = c(70, 85, 60)
  )

  blk <- new_pivot_longer_block(
    state = list(
      cols = list("height", "weight"),
      names_to = "measurement",
      values_to = "value",
      values_drop_na = FALSE,
      names_prefix = ""
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_equal(nrow(result), 6)  # 3 rows * 2 columns
    expect_true("measurement" %in% colnames(result))
    expect_true("value" %in% colnames(result))
    expect_true("id" %in% colnames(result))
    expect_equal(sort(unique(result$measurement)), c("height", "weight"))
  })
})

test_that("pivot_longer block: values_drop_na removes NA rows", {
  df <- data.frame(
    id = 1:3,
    x = c(1, NA, 3),
    y = c(NA, 2, 4)
  )

  blk <- new_pivot_longer_block(
    state = list(
      cols = list("x", "y"),
      names_to = "var",
      values_to = "val",
      values_drop_na = TRUE,
      names_prefix = ""
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_false(any(is.na(result$val)))
    expect_equal(nrow(result), 4)  # 6 - 2 NAs
  })
})

test_that("pivot_longer block: state change updates pivot columns", {
  df <- data.frame(
    id = 1:2,
    a = c(10, 20),
    b = c(30, 40),
    c = c(50, 60)
  )

  blk <- new_pivot_longer_block(
    state = list(
      cols = list("a", "b"),
      names_to = "name",
      values_to = "value",
      values_drop_na = FALSE,
      names_prefix = ""
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), df)
    expect_equal(nrow(result1), 4)  # 2 rows * 2 cols

    # Add column c to pivot
    session$returned$state$state(list(
      cols = list("a", "b", "c"),
      names_to = "name",
      values_to = "value",
      values_drop_na = FALSE,
      names_prefix = ""
    ))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), df)
    expect_equal(nrow(result2), 6)  # 2 rows * 3 cols
  })
})
