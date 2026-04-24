# Tests for the new JS-driven filter block

# Helper: evaluate a bquoted expression the same way blockr.core does.
# Resolves .(data) placeholder, then evaluates in an env where data = df.
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

# --- Unit tests for expression builders ---

test_that("make_filter_expr handles empty conditions", {
  expr <- make_filter_expr(list(), "&")
  expect_type(expr, "language")
  result <- eval_bquoted(expr, iris)
  expect_equal(nrow(result), nrow(iris))
})

test_that("make_filter_expr handles values condition (include)", {
  conds <- list(list(
    type = "values", column = "Species",
    values = list("setosa"), mode = "include"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, iris)
  expect_true(all(result$Species == "setosa"))
  expect_equal(nrow(result), 50)
})

test_that("make_filter_expr handles values condition (exclude)", {
  conds <- list(list(
    type = "values", column = "Species",
    values = list("setosa"), mode = "exclude"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, iris)
  expect_true(all(result$Species != "setosa"))
  expect_equal(nrow(result), 100)
})

test_that("make_filter_expr handles multiple values", {
  conds <- list(list(
    type = "values", column = "Species",
    values = list("setosa", "virginica"), mode = "include"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, iris)
  expect_true(all(result$Species %in% c("setosa", "virginica")))
  expect_equal(nrow(result), 100)
})

test_that("make_filter_expr handles numeric values as strings", {
  conds <- list(list(
    type = "values", column = "cyl",
    values = list("4", "6"), mode = "include"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, mtcars)
  expect_true(all(result$cyl %in% c(4, 6)))
})

test_that("make_filter_expr handles numeric condition (greater than)", {
  conds <- list(list(
    type = "numeric", column = "Sepal.Length",
    op = ">", value = 6
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, iris)
  expect_true(all(result$Sepal.Length > 6))
})

test_that("make_filter_expr handles numeric condition (<=)", {
  conds <- list(list(
    type = "numeric", column = "Sepal.Length",
    op = "<=", value = 5
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, iris)
  expect_true(all(result$Sepal.Length <= 5))
})

test_that("make_filter_expr handles numeric condition (is / ==)", {
  conds <- list(list(
    type = "numeric", column = "cyl",
    op = "is", value = 4
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, mtcars)
  expect_true(all(result$cyl == 4))
})

test_that("make_filter_expr handles expression condition", {
  conds <- list(list(
    type = "expr",
    expr = "Sepal.Length > 5 & Species == 'setosa'"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, iris)
  expect_true(all(result$Sepal.Length > 5))
  expect_true(all(result$Species == "setosa"))
})

test_that("make_filter_expr combines conditions with AND", {
  conds <- list(
    list(type = "values", column = "Species",
         values = list("setosa", "versicolor"), mode = "include"),
    list(type = "numeric", column = "Sepal.Length",
         op = ">", value = 5)
  )
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, iris)
  expect_true(all(result$Species %in% c("setosa", "versicolor")))
  expect_true(all(result$Sepal.Length > 5))
})

test_that("make_filter_expr combines conditions with OR", {
  conds <- list(
    list(type = "values", column = "Species",
         values = list("setosa"), mode = "include"),
    list(type = "numeric", column = "Sepal.Length",
         op = ">", value = 7)
  )
  expr <- make_filter_expr(conds, "|")
  result <- eval_bquoted(expr, iris)
  expect_true(all(result$Species == "setosa" | result$Sepal.Length > 7))
})

test_that("make_filter_expr handles NA special value", {
  df <- data.frame(x = c(1, 2, NA, 4))
  conds <- list(list(
    type = "values", column = "x",
    values = list("<NA>"), mode = "include"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, df)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$x))
})

test_that("make_filter_expr skips invalid expression", {
  conds <- list(list(type = "expr", expr = "this is not valid R((("))
  expr <- make_filter_expr(conds, "&")
  # Invalid expression returns NULL from make_expr_part, so all parts are NULL
  # and we get the fallback TRUE filter
  result <- eval_bquoted(expr, iris)
  expect_equal(nrow(result), nrow(iris))
})

test_that("build_column_meta returns correct structure", {
  meta <- build_column_meta(iris)
  expect_length(meta, ncol(iris))
  expect_equal(meta[[1]]$name, "Sepal.Length")
  expect_equal(meta[[1]]$type, "numeric")
  expect_false(meta[[1]]$hasNA)
  expect_true(!is.null(meta[[1]]$min))
  expect_true(!is.null(meta[[1]]$max))

  # Species should be character type with values
  species_meta <- meta[[5]]
  expect_equal(species_meta$name, "Species")
  expect_equal(species_meta$type, "character")
  expect_true(length(species_meta$values) > 0)
})

# --- testServer tests for filter block ---

test_that("filter block produces correct expression from initial state", {
  blk <- new_filter_block(
    state = list(
      conditions = list(
        list(type = "values", column = "cyl",
             values = list("4", "6"), mode = "include")
      ),
      operator = "&"
    )
  )

  testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars)),
    {
      session$flushReact()
      result <- session$returned
      expect_true(is.reactive(result$expr))
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))

      # Evaluate the bquoted expression
      evaluated <- eval_bquoted(expr_result, mtcars)
      expect_true(is.data.frame(evaluated))
      expect_true(all(evaluated$cyl %in% c(4, 6)))
    }
  )
})

test_that("filter block produces correct expression for numeric condition", {
  blk <- new_filter_block(
    state = list(
      conditions = list(
        list(type = "numeric", column = "mpg", op = ">", value = 25)
      ),
      operator = "&"
    )
  )

  testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars)),
    {
      session$flushReact()
      expr_result <- session$returned$expr()
      evaluated <- eval_bquoted(expr_result, mtcars)
      expect_true(is.data.frame(evaluated))
      expect_true(all(evaluated$mpg > 25))
    }
  )
})

test_that("filter block state change propagates to expression", {
  blk <- new_filter_block(
    state = list(
      conditions = list(
        list(type = "values", column = "Species",
             values = list("setosa"), mode = "include")
      ),
      operator = "&"
    )
  )

  testServer(
    blk$expr_server,
    args = list(data = reactive(iris)),
    {
      session$flushReact()
      expr_result <- session$returned$expr()
      evaluated <- eval_bquoted(expr_result, iris)
      expect_equal(nrow(evaluated), 50)

      # Update state via reactiveVal
      state <- session$returned$state
      state$state(list(
        conditions = list(
          list(type = "values", column = "Species",
               values = list("virginica"), mode = "include")
        ),
        operator = "&"
      ))
      session$flushReact()
      expr_result2 <- session$returned$expr()
      evaluated2 <- eval_bquoted(expr_result2, iris)
      expect_equal(nrow(evaluated2), 50)
      expect_true(all(evaluated2$Species == "virginica"))
    }
  )
})

# --- Column summary label support ---

test_that("build_column_summary reads attr(x, 'label')", {
  df <- data.frame(age = 1:3, sex = c("M", "F", "M"), stringsAsFactors = FALSE)
  attr(df$age, "label") <- "Age in years"
  attr(df$sex, "label") <- "Sex"
  summary <- build_column_summary(df)
  labels <- vapply(summary, function(x) x$label, character(1))
  expect_equal(labels, c("Age in years", "Sex"))
})

test_that("build_column_summary defaults label to empty string", {
  summary <- build_column_summary(data.frame(a = 1:3, b = 4:6))
  labels <- vapply(summary, function(x) x$label, character(1))
  expect_equal(labels, c("", ""))
})

test_that("build_column_summary ignores non-character label attr", {
  df <- data.frame(a = 1:3)
  attr(df$a, "label") <- 42
  expect_equal(build_column_summary(df)[[1]]$label, "")
})

test_that("build_column_values includes label", {
  df <- data.frame(age = 1:5)
  attr(df$age, "label") <- "Age"
  info <- build_column_values(df, "age")
  expect_equal(info$label, "Age")
})
