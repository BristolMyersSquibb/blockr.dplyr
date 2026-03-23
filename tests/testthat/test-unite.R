# Helper: evaluate a bquoted expression the same way blockr.core does.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("unite block: empty cols pass data through", {
  blk <- new_unite_block(
    state = list(
      col = "united", cols = list(),
      sep = "_", remove = TRUE, na.rm = FALSE
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), mtcars)
    expect_equal(result, mtcars)
  })
})

test_that("unite block: combines columns into one", {
  df <- data.frame(
    first = c("John", "Jane", "Bob"),
    last = c("Doe", "Smith", "Jones"),
    age = c(30, 25, 40),
    stringsAsFactors = FALSE
  )

  blk <- new_unite_block(
    state = list(
      col = "full_name",
      cols = list("first", "last"),
      sep = " ",
      remove = TRUE,
      na.rm = FALSE
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_true("full_name" %in% colnames(result))
    expect_false("first" %in% colnames(result))
    expect_false("last" %in% colnames(result))
    expect_equal(result$full_name, c("John Doe", "Jane Smith", "Bob Jones"))
  })
})

test_that("unite block: remove = FALSE keeps original columns", {
  df <- data.frame(
    x = c("a", "b"),
    y = c("1", "2"),
    stringsAsFactors = FALSE
  )

  blk <- new_unite_block(
    state = list(
      col = "combined",
      cols = list("x", "y"),
      sep = "-",
      remove = FALSE,
      na.rm = FALSE
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_true("combined" %in% colnames(result))
    expect_true("x" %in% colnames(result))
    expect_true("y" %in% colnames(result))
    expect_equal(result$combined, c("a-1", "b-2"))
  })
})

test_that("unite block: na.rm handles NA values", {
  df <- data.frame(
    x = c("a", NA, "c"),
    y = c("1", "2", NA),
    stringsAsFactors = FALSE
  )

  blk <- new_unite_block(
    state = list(
      col = "combined",
      cols = list("x", "y"),
      sep = "_",
      remove = TRUE,
      na.rm = TRUE
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result <- eval_bquoted(session$returned$expr(), df)
    expect_equal(result$combined, c("a_1", "2", "c"))
  })
})

test_that("unite block: state change updates separator and columns", {
  df <- data.frame(
    a = c("x", "y"),
    b = c("1", "2"),
    c = c("p", "q"),
    stringsAsFactors = FALSE
  )

  blk <- new_unite_block(
    state = list(
      col = "ab",
      cols = list("a", "b"),
      sep = "_",
      remove = TRUE,
      na.rm = FALSE
    )
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    result1 <- eval_bquoted(session$returned$expr(), df)
    expect_equal(result1$ab, c("x_1", "y_2"))

    # Change to unite all three columns with different separator
    session$returned$state$state(list(
      col = "abc",
      cols = list("a", "b", "c"),
      sep = "-",
      remove = TRUE,
      na.rm = FALSE
    ))
    session$flushReact()
    result2 <- eval_bquoted(session$returned$expr(), df)
    expect_true("abc" %in% colnames(result2))
    expect_equal(result2$abc, c("x-1-p", "y-2-q"))
  })
})
