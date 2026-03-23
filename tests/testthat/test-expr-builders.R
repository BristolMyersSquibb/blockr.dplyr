# Tests for all expression builders in R/expr-builders.R

eval_bquoted <- function(expr, df, name = "data") {
  resolved <- do.call(bquote, list(expr, stats::setNames(list(as.name(name)), name)))
  eval(resolved, envir = stats::setNames(list(df), name))
}

# --- Mutate ---

test_that("make_mutate_expr handles empty rows", {
  expr <- make_mutate_expr(list())
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), nrow(mtcars))
})

test_that("make_mutate_expr creates new column", {
  rows <- list(list(name = "mpg2", expr = "mpg * 2"))
  expr <- make_mutate_expr(rows)
  result <- eval_bquoted(expr, mtcars)
  expect_true("mpg2" %in% colnames(result))
  expect_equal(result$mpg2, mtcars$mpg * 2)
})

test_that("make_mutate_expr handles multiple rows", {
  rows <- list(
    list(name = "a", expr = "mpg + 1"),
    list(name = "b", expr = "cyl * 2")
  )
  expr <- make_mutate_expr(rows)
  result <- eval_bquoted(expr, mtcars)
  expect_true("a" %in% colnames(result))
  expect_true("b" %in% colnames(result))
})

# --- Summarize ---

test_that("make_summarize_expr with simple summary", {
  summaries <- list(list(type = "simple", name = "avg", func = "mean", col = "mpg"))
  expr <- make_summarize_expr(summaries)
  result <- eval_bquoted(expr, mtcars)
  expect_equal(result$avg, mean(mtcars$mpg))
})

test_that("make_summarize_expr with n()", {
  summaries <- list(list(type = "simple", name = "count", func = "n", col = ""))
  expr <- make_summarize_expr(summaries)
  result <- eval_bquoted(expr, mtcars)
  expect_equal(result$count, nrow(mtcars))
})

test_that("make_summarize_expr with grouping", {
  summaries <- list(list(type = "simple", name = "avg", func = "mean", col = "mpg"))
  expr <- make_summarize_expr(summaries, by = "cyl")
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), length(unique(mtcars$cyl)))
})

test_that("make_summarize_expr with expression", {
  summaries <- list(list(type = "expr", name = "custom", expr = "sum(mpg) / dplyr::n()"))
  expr <- make_summarize_expr(summaries)
  result <- eval_bquoted(expr, mtcars)
  expect_equal(result$custom, sum(mtcars$mpg) / nrow(mtcars))
})

# --- Join ---

test_that("make_join_expr with equi-join", {
  keys <- list(list(xCol = "id", op = "==", yCol = "id"))
  expr <- make_join_expr("left_join", keys)
  expect_true(is.call(expr))
  expect_true(grepl("left_join", deparse(expr)))
})

test_that("make_join_expr with no keys (natural join)", {
  expr <- make_join_expr("inner_join", list())
  expect_true(is.call(expr))
})

# --- Select ---

test_that("make_select_expr with columns", {
  expr <- make_select_expr(c("mpg", "cyl"))
  result <- eval_bquoted(expr, mtcars)
  expect_equal(colnames(result), c("mpg", "cyl"))
})

test_that("make_select_expr exclude mode", {
  expr <- make_select_expr(c("mpg"), exclude = TRUE)
  result <- eval_bquoted(expr, mtcars)
  expect_false("mpg" %in% colnames(result))
})

test_that("make_select_expr with distinct", {
  expr <- make_select_expr(c("cyl"), distinct = TRUE)
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), length(unique(mtcars$cyl)))
})

test_that("make_select_expr empty columns selects all", {
  expr <- make_select_expr(character())
  result <- eval_bquoted(expr, mtcars)
  expect_equal(ncol(result), ncol(mtcars))
})

# --- Arrange ---

test_that("make_arrange_expr sorts ascending", {
  cols <- list(list(column = "mpg", direction = "asc"))
  expr <- make_arrange_expr(cols)
  result <- eval_bquoted(expr, mtcars)
  expect_true(all(diff(result$mpg) >= 0))
})

test_that("make_arrange_expr sorts descending", {
  cols <- list(list(column = "mpg", direction = "desc"))
  expr <- make_arrange_expr(cols)
  result <- eval_bquoted(expr, mtcars)
  expect_true(all(diff(result$mpg) <= 0))
})

test_that("make_arrange_expr empty is pass-through", {
  expr <- make_arrange_expr(list())
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), nrow(mtcars))
})

# --- Rename ---

test_that("make_rename_expr renames column", {
  expr <- make_rename_expr(list(miles = "mpg"))
  result <- eval_bquoted(expr, mtcars)
  expect_true("miles" %in% colnames(result))
  expect_false("mpg" %in% colnames(result))
})

test_that("make_rename_expr empty is pass-through", {
  expr <- make_rename_expr(list())
  result <- eval_bquoted(expr, mtcars)
  expect_equal(colnames(result), colnames(mtcars))
})

# --- Slice ---

test_that("make_slice_expr head", {
  expr <- make_slice_expr("head", n = 3L)
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), 3)
})

test_that("make_slice_expr tail", {
  expr <- make_slice_expr("tail", n = 5L)
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), 5)
})

test_that("make_slice_expr sample", {
  expr <- make_slice_expr("sample", n = 10L)
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), 10)
})

# --- Pivot longer ---

test_that("make_pivot_longer_expr works", {
  df <- data.frame(id = 1:3, a = 4:6, b = 7:9)
  expr <- make_pivot_longer_expr(c("a", "b"), "var", "val")
  result <- eval_bquoted(expr, df)
  expect_equal(nrow(result), 6)
  expect_true("var" %in% colnames(result))
  expect_true("val" %in% colnames(result))
})

test_that("make_pivot_longer_expr empty cols is pass-through", {
  expr <- make_pivot_longer_expr(character())
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), nrow(mtcars))
})

# --- Pivot wider ---

test_that("make_pivot_wider_expr works", {
  df <- data.frame(id = c(1, 1, 2, 2), var = c("a", "b", "a", "b"), val = 1:4)
  expr <- make_pivot_wider_expr("var", "val")
  result <- eval_bquoted(expr, df)
  expect_equal(nrow(result), 2)
  expect_true("a" %in% colnames(result))
})

# --- Unite ---

test_that("make_unite_expr works", {
  df <- data.frame(first = c("John", "Jane"), last = c("Doe", "Smith"))
  expr <- make_unite_expr("full", c("first", "last"), sep = " ")
  result <- eval_bquoted(expr, df)
  expect_true("full" %in% colnames(result))
  expect_equal(result$full, c("John Doe", "Jane Smith"))
})

# --- Separate ---

test_that("make_separate_expr works", {
  df <- data.frame(full = c("John Doe", "Jane Smith"))
  expr <- make_separate_expr("full", c("first", "last"), sep = " ")
  result <- eval_bquoted(expr, df)
  expect_true("first" %in% colnames(result))
  expect_true("last" %in% colnames(result))
})

# --- Build column meta ---

test_that("make_mutate_expr with grouping", {
  rows <- list(list(name = "avg", expr = "mean(mpg)"))
  expr <- make_mutate_expr(rows, by = "cyl")
  result <- eval_bquoted(expr, mtcars)
  # Grouped mutate: avg should differ by cyl group
  expect_true("avg" %in% colnames(result))
  expect_equal(nrow(result), nrow(mtcars))
  # Values should be group-specific means, not the overall mean
  expect_false(all(result$avg == mean(mtcars$mpg)))
})

test_that("build_column_meta returns correct structure", {
  meta <- build_column_meta(iris)
  expect_length(meta, ncol(iris))
  expect_equal(meta[[1]]$name, "Sepal.Length")
  expect_equal(meta[[1]]$type, "numeric")
})
