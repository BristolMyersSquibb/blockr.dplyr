# Tests for AI-readiness: blocks with constructor args produce correct output
# via testServer (using block_server from blockr.core).

test_that("select_block AI args work via testServer", {
  block <- new_select_block(columns = c("mpg", "cyl"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(colnames(result), c("mpg", "cyl"))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("select_block exclude mode AI args work via testServer", {
  block <- new_select_block(columns = c("mpg", "cyl"), exclude = TRUE)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_false("mpg" %in% colnames(result))
      expect_false("cyl" %in% colnames(result))
      expect_true("hp" %in% colnames(result))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("arrange_block AI args work via testServer", {
  block <- new_arrange_block(
    columns = list(list(column = "mpg", direction = "desc"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(result$mpg[1] >= result$mpg[nrow(result)])
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("arrange_block with character columns AI args work via testServer", {
  block <- new_arrange_block(columns = "mpg")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(result$mpg[1] <= result$mpg[nrow(result)])
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("rename_block AI args work via testServer", {
  block <- new_rename_block(renames = list(miles_per_gallon = "mpg"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("miles_per_gallon" %in% colnames(result))
      expect_false("mpg" %in% colnames(result))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("slice_block AI args work via testServer - head", {
  block <- new_slice_block(type = "head", n = 5)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 5)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("slice_block AI args work via testServer - max", {
  block <- new_slice_block(type = "max", order_by = "mpg", n = 3)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      # Top 3 by mpg should have high mpg values
      expect_true(all(result$mpg >= sort(mtcars$mpg, decreasing = TRUE)[4]))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize_block AI args work via testServer", {
  block <- new_summarize_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      count = list(func = "dplyr::n", col = "")
    ),
    by = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("avg_mpg" %in% colnames(result))
      expect_true("count" %in% colnames(result))
      expect_true("cyl" %in% colnames(result))
      expect_equal(nrow(result), length(unique(mtcars$cyl)))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("mutate_expr_block AI args work via testServer", {
  block <- new_mutate_expr_block(
    exprs = list(mpg_squared = "mpg^2")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("mpg_squared" %in% colnames(result))
      expect_equal(result$mpg_squared, mtcars$mpg^2)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("filter_expr_block AI args work via testServer", {
  block <- new_filter_expr_block(exprs = "mpg > 25")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(all(result$mpg > 25))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize_expr_block AI args work via testServer", {
  block <- new_summarize_expr_block(
    exprs = list(avg_mpg = "mean(mpg)", n = "dplyr::n()"),
    by = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("avg_mpg" %in% colnames(result))
      expect_true("n" %in% colnames(result))
      expect_equal(nrow(result), length(unique(mtcars$cyl)))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("unite_block AI args work via testServer", {
  test_data <- data.frame(
    first = c("John", "Jane"),
    last = c("Doe", "Smith"),
    age = c(30, 25),
    stringsAsFactors = FALSE
  )

  block <- new_unite_block(
    col = "full_name",
    cols = c("first", "last"),
    sep = " "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("full_name" %in% colnames(result))
      expect_equal(result$full_name, c("John Doe", "Jane Smith"))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate_block AI args work via testServer", {
  test_data <- data.frame(
    full_name = c("John Doe", "Jane Smith"),
    age = c(30, 25),
    stringsAsFactors = FALSE
  )

  block <- new_separate_block(
    col = "full_name",
    into = c("first", "last"),
    sep = " "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("first" %in% colnames(result))
      expect_true("last" %in% colnames(result))
      expect_equal(result$first, c("John", "Jane"))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("pivot_longer_block AI args work via testServer", {
  test_data <- data.frame(
    id = 1:2,
    col_a = c(10, 20),
    col_b = c(15, 25)
  )

  block <- new_pivot_longer_block(
    cols = c("col_a", "col_b"),
    names_to = "variable",
    values_to = "value"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("variable" %in% colnames(result))
      expect_true("value" %in% colnames(result))
      expect_equal(nrow(result), 4) # 2 rows * 2 cols
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("pivot_wider_block AI args work via testServer", {
  test_data <- data.frame(
    id = c(1, 1, 2, 2),
    category = c("a", "b", "a", "b"),
    value = c(10, 15, 20, 25)
  )

  block <- new_pivot_wider_block(
    names_from = "category",
    values_from = "value"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("a" %in% colnames(result))
      expect_true("b" %in% colnames(result))
      expect_equal(nrow(result), 2)
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# Registry metadata validation
test_that("all registered dplyr blocks have arguments and examples", {
  # Register blocks first
  register_dplyr_blocks()
  registry <- blockr.core:::block_registry

  # Registry uses class names (without "new_" prefix)
  dplyr_blocks <- c(
    "select_block", "arrange_block", "mutate_expr_block",
    "summarize_block", "summarize_expr_block",
    "filter_block", "filter_expr_block",
    "rename_block", "slice_block",
    "pivot_longer_block", "pivot_wider_block",
    "unite_block", "separate_block",
    "join_block", "bind_rows_block"
  )

  for (block_name in dplyr_blocks) {
    entry <- get(block_name, envir = registry)
    expect_true(
      !is.null(attr(entry, "arguments")),
      info = paste(block_name, "missing arguments")
    )
    # Check that arguments have an examples attribute (R-native list)
    args <- attr(entry, "arguments")
    expect_true(
      !is.null(attr(args, "examples")),
      info = paste(block_name, "missing examples attribute on arguments")
    )
  }
})
