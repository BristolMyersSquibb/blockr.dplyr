# Tests for all expression builders in R/expr-builders.R

eval_bquoted <- function(expr, df, name = "data") {
  resolved <- do.call(
    bquote,
    list(expr, stats::setNames(list(as.name(name)), name))
  )
  eval(
    resolved,
    envir = stats::setNames(list(df), name)
  )
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
  summaries <- list(list(
    type = "simple", name = "avg",
    func = "mean", col = "mpg"
  ))
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
  summaries <- list(list(
    type = "simple", name = "avg",
    func = "mean", col = "mpg"
  ))
  expr <- make_summarize_expr(summaries, by = "cyl")
  result <- eval_bquoted(expr, mtcars)
  expect_equal(nrow(result), length(unique(mtcars$cyl)))
})

test_that("make_summarize_expr with expression", {
  summaries <- list(list(
    type = "expr", name = "custom",
    expr = "sum(mpg) / dplyr::n()"
  ))
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

test_that("build_column_meta values are lists (auto-unbox safe)", {
  df <- data.frame(id = rep("ONLY", 3), x = rep(42, 3))
  meta <- build_column_meta(df)
  # Character column: values must be a list, not a character vector
  expect_type(meta[[1]]$values, "list")
  # Numeric column: uniqueValues must be a list
  expect_type(meta[[2]]$uniqueValues, "list")
})

# --- Filter value type conversion (colType tag) ---

test_that("values condition keeps character values on character columns", {
  df <- data.frame(
    USUBJID = c("001", "007", "100"),
    x = 1:3,
    stringsAsFactors = FALSE
  )
  conds <- list(list(
    type = "values", column = "USUBJID", values = list("007"),
    mode = "include", colType = "character"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, df)
  expect_equal(result$USUBJID, "007")
})

test_that("values condition coerces to numeric on numeric columns", {
  df <- data.frame(cyl = c(4, 6, 8))
  conds <- list(list(
    type = "values", column = "cyl", values = list("4", "6"),
    mode = "include", colType = "numeric"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, df)
  expect_equal(result$cyl, c(4, 6))
})

test_that("values condition converts logical columns", {
  df <- data.frame(flag = c(TRUE, FALSE, TRUE))
  conds <- list(list(
    type = "values", column = "flag", values = list("TRUE"),
    mode = "include", colType = "logical"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, df)
  expect_equal(result$flag, c(TRUE, TRUE))
})

test_that("values condition on factor columns matches as character", {
  df <- data.frame(Species = factor(c("setosa", "virginica", "setosa")))
  conds <- list(list(
    type = "values", column = "Species", values = list("setosa"),
    mode = "include", colType = "character"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, df)
  expect_equal(nrow(result), 2)
})

test_that("values condition without colType falls back to coercion heuristic", {
  # Legacy saved states carry no colType: all-numeric-looking values coerce
  df <- data.frame(cyl = c(4, 6, 8))
  conds <- list(list(
    type = "values", column = "cyl", values = list("4"), mode = "include"
  ))
  expr <- make_filter_expr(conds, "&")
  result <- eval_bquoted(expr, df)
  expect_equal(result$cyl, 4)

  # ... and mixed values stay character
  df2 <- data.frame(g = c("a", "b"), stringsAsFactors = FALSE)
  conds2 <- list(list(
    type = "values", column = "g", values = list("a"), mode = "include"
  ))
  result2 <- eval_bquoted(make_filter_expr(conds2, "&"), df2)
  expect_equal(result2$g, "a")
})

test_that("preserveOrder match() respects colType", {
  df <- data.frame(id = c("001", "010", "007"), stringsAsFactors = FALSE)
  conds <- list(list(
    type = "values", column = "id", values = list("007", "001"),
    mode = "include", colType = "character"
  ))
  expr <- make_filter_expr(conds, "&", preserve_order = TRUE)
  result <- eval_bquoted(expr, df)
  expect_equal(result$id, c("007", "001"))
})

test_that("exclude mode respects colType on character columns", {
  df <- data.frame(id = c("001", "007"), stringsAsFactors = FALSE)
  conds <- list(list(
    type = "values", column = "id", values = list("007"),
    mode = "exclude", colType = "character"
  ))
  result <- eval_bquoted(make_filter_expr(conds, "&"), df)
  expect_equal(result$id, "001")
})

# --- Join / summarize with non-syntactic column names ---

test_that("make_join_expr handles non-syntactic and quote-bearing names", {
  x <- data.frame(`my id` = c(1, 2), v = c("a", "b"), check.names = FALSE)
  y <- data.frame(`their "id"` = c(2, 3), w = c("X", "Y"), check.names = FALSE)

  keys <- list(list(xCol = "my id", op = "==", yCol = 'their "id"'))
  expr <- make_join_expr("inner_join", keys)
  resolved <- do.call(
    bquote,
    list(expr, list(x = as.name("x"), y = as.name("y")))
  )
  result <- eval(resolved, envir = list(x = x, y = y))
  expect_equal(result$w, "X")
})

test_that("make_join_expr non-equi join builds join_by()", {
  keys <- list(list(xCol = "a", op = ">=", yCol = "b"))
  expr <- make_join_expr("left_join", keys)
  expect_match(deparse1(expr), "join_by", fixed = TRUE)
  x <- data.frame(a = c(1, 5))
  y <- data.frame(b = c(2, 4), tag = c("lo", "hi"))
  resolved <- do.call(
    bquote,
    list(expr, list(x = as.name("x"), y = as.name("y")))
  )
  result <- eval(resolved, envir = list(x = x, y = y))
  expect_equal(result$tag, c(NA, "lo", "hi"))
})

test_that("make_join_expr custom suffixes survive special characters", {
  x <- data.frame(id = 1, v = "x")
  y <- data.frame(id = 1, v = "y")
  expr <- make_join_expr(
    "left_join",
    list(list(xCol = "id", op = "==", yCol = "id")),
    suffix_x = '_l"', suffix_y = "_r"
  )
  resolved <- do.call(
    bquote,
    list(expr, list(x = as.name("x"), y = as.name("y")))
  )
  result <- eval(resolved, envir = list(x = x, y = y))
  expect_true('v_l"' %in% names(result))
})

test_that("make_join_expr rejects unknown join types and operators", {
  expr <- make_join_expr(
    "system_join",
    list(list(xCol = "id", op = "%in%", yCol = "id"))
  )
  txt <- deparse1(expr)
  expect_match(txt, "left_join", fixed = TRUE)
  expect_false(grepl("%in%", txt, fixed = TRUE))
})

test_that("make_summarize_expr handles non-syntactic column names", {
  df <- data.frame(`my value` = c(1, 2, 3), check.names = FALSE)
  summaries <- list(list(
    type = "simple", name = "avg", func = "mean", col = "my value"
  ))
  expr <- make_summarize_expr(summaries)
  result <- eval_bquoted(expr, df)
  expect_equal(result$avg, 2)
})

# --- Empty-state pass-through and column metadata ---

test_that("empty filter and summarize states pass data through unchanged", {
  expect_identical(eval_bquoted(make_filter_expr(list(), "&"), mtcars), mtcars)
  expect_identical(eval_bquoted(make_summarize_expr(list()), mtcars), mtcars)
  # summaries present but all invalid (no name yet) also pass through
  invalid <- list(list(type = "simple", name = "", func = "mean", col = "mpg"))
  expect_identical(eval_bquoted(make_summarize_expr(invalid), mtcars), mtcars)
})

test_that("column_type detects integer and logical correctly", {
  expect_equal(column_type(1L), "integer")
  expect_equal(column_type(1.5), "numeric")
  expect_equal(column_type(TRUE), "logical")
  expect_equal(column_type("a"), "character")
  expect_equal(column_type(factor("a")), "character")
})

test_that("build_column_values keeps factor level order", {
  df <- data.frame(
    sev = factor(c("HIGH", "LOW", "MED"), levels = c("LOW", "MED", "HIGH"))
  )
  info <- build_column_values(df, "sev")
  expect_equal(unlist(info$values), c("LOW", "MED", "HIGH"))
})

test_that("build_column_values survives all-NA numeric columns", {
  df <- data.frame(x = c(NA_real_, NA_real_))
  expect_no_warning(info <- build_column_values(df, "x"))
  expect_null(info$min)
  expect_equal(length(info$uniqueValues), 0)
})
