# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("separate block: empty col passes data through", {
  blk <- new_separate_block(
    state = list(
      col = "", into = list(),
      sep = "[^[:alnum:]]+", remove = TRUE,
      convert = FALSE, extra = "warn", fill = "warn"
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(result, mtcars)
  })
})

test_that("separate block: splits column into multiple", {
  df <- data.frame(
    x = c("a-1", "b-2", "c-3"),
    stringsAsFactors = FALSE
  )

  blk <- new_separate_block(
    state = list(
      col = "x",
      into = c("letter", "number"),
      sep = "-",
      remove = TRUE,
      convert = FALSE,
      extra = "warn",
      fill = "warn"
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_true("letter" %in% colnames(result))
    expect_true("number" %in% colnames(result))
    expect_false("x" %in% colnames(result))
    expect_equal(result$letter, c("a", "b", "c"))
    expect_equal(result$number, c("1", "2", "3"))
  })
})

test_that("separate block: convert converts types", {
  df <- data.frame(
    x = c("a-1", "b-2", "c-3"),
    stringsAsFactors = FALSE
  )

  blk <- new_separate_block(
    state = list(
      col = "x",
      into = c("letter", "number"),
      sep = "-",
      remove = TRUE,
      convert = TRUE,
      extra = "warn",
      fill = "warn"
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_type(result$number, "integer")
    expect_equal(result$number, c(1L, 2L, 3L))
  })
})

test_that("separate block: remove = FALSE keeps original column", {
  df <- data.frame(
    x = c("a-1", "b-2"),
    stringsAsFactors = FALSE
  )

  blk <- new_separate_block(
    state = list(
      col = "x",
      into = c("letter", "number"),
      sep = "-",
      remove = FALSE,
      convert = FALSE,
      extra = "warn",
      fill = "warn"
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_true("x" %in% colnames(result))
    expect_true("letter" %in% colnames(result))
    expect_true("number" %in% colnames(result))
  })
})

test_that("separate block: state change updates separator and column names", {
  df <- data.frame(
    data = c("2024_01_15", "2024_02_20"),
    stringsAsFactors = FALSE
  )

  blk <- new_separate_block(
    state = list(
      col = "data",
      into = c("year", "month_day"),
      sep = "_",
      remove = TRUE,
      convert = FALSE,
      extra = "merge",
      fill = "warn"
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), df)
    expect_equal(ncol(result1), 2)
    expect_equal(result1$year, c("2024", "2024"))
    expect_equal(result1$month_day, c("01_15", "02_20"))

    # Change to separate into 3 columns
    session$returned$state$state(list(
      col = "data",
      into = c("year", "month", "day"),
      sep = "_",
      remove = TRUE,
      convert = FALSE,
      extra = "warn",
      fill = "warn"
    ))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), df)
    expect_equal(ncol(result2), 3)
    expect_true("day" %in% colnames(result2))
    expect_equal(result2$day, c("15", "20"))
  })
})

# =========================================================================
# Join block (binary, needs special handling with x and y)
# =========================================================================

test_that("join block: empty keys passes through x", {
  df_x <- data.frame(id = 1:3, val_x = c("a", "b", "c"),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(id = 2:4, val_y = c("x", "y", "z"),
                     stringsAsFactors = FALSE)

  blk <- new_join_block(
    state = list(
      type = "left_join", keys = list(), exprs = list(),
      suffix_x = ".x", suffix_y = ".y"
    )
  )

  testServer(
    blk$expr_server,
    args = list(x = reactive(df_x), y = reactive(df_y)),
    {
      session$flushReact()
      expr_val <- session$returned$expr()
      # With no keys, expression is just .(x) — pass-through
      resolved <- do.call(bquote, list(
        expr_val,
        list(x = as.name("x"), y = as.name("y"))
      ))
      result <- eval(resolved, envir = list(x = df_x, y = df_y))
      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 3)
      expect_equal(colnames(result), colnames(df_x))
    }
  )
})

test_that("join block: equi-join with key", {
  df_x <- data.frame(id = 1:3, val_x = c("a", "b", "c"),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(id = 2:4, val_y = c("x", "y", "z"),
                     stringsAsFactors = FALSE)

  blk <- new_join_block(
    state = list(
      type = "inner_join",
      keys = list(list(xCol = "id", op = "==", yCol = "id")),
      exprs = list(),
      suffix_x = ".x", suffix_y = ".y"
    )
  )

  testServer(
    blk$expr_server,
    args = list(x = reactive(df_x), y = reactive(df_y)),
    {
      session$flushReact()
      expr_val <- session$returned$expr()
      resolved <- do.call(bquote, list(
        expr_val,
        list(x = as.name("x"), y = as.name("y"))
      ))
      result <- eval(resolved, envir = list(x = df_x, y = df_y))
      # Inner join: only matching rows (id 2 and 3)
      expect_equal(nrow(result), 2)
      expect_true(all(result$id %in% c(2, 3)))
    }
  )
})

test_that("join block: state change switches join type", {
  df_x <- data.frame(id = 1:3, val_x = c("a", "b", "c"),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(id = 2:4, val_y = c("x", "y", "z"),
                     stringsAsFactors = FALSE)

  blk <- new_join_block(
    state = list(
      type = "inner_join",
      keys = list(list(xCol = "id", op = "==", yCol = "id")),
      exprs = list(),
      suffix_x = ".x", suffix_y = ".y"
    )
  )

  eval_join <- function(expr_val) {
    resolved <- do.call(bquote, list(
      expr_val,
      list(x = as.name("x"), y = as.name("y"))
    ))
    eval(resolved, envir = list(x = df_x, y = df_y))
  }

  testServer(
    blk$expr_server,
    args = list(x = reactive(df_x), y = reactive(df_y)),
    {
      session$flushReact()
      result1 <- eval_join(session$returned$expr())
      expect_equal(nrow(result1), 2)  # inner join

      # Switch to full join
      session$returned$state$state(list(
        type = "full_join",
        keys = list(list(xCol = "id", op = "==", yCol = "id")),
        exprs = list(),
        suffix_x = ".x", suffix_y = ".y"
      ))
      session$flushReact()
      result2 <- eval_join(session$returned$expr())
      expect_equal(nrow(result2), 4)  # full join: ids 1,2,3,4
    }
  )
})
