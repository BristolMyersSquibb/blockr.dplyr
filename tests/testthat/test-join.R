test_that("join block left_join basic functionality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  # Create test data
  x_data <- data.frame(
    id = c(1, 2, 3),
    name = c("Alice", "Bob", "Charlie"),
    stringsAsFactors = FALSE
  )

  y_data <- data.frame(
    id = c(1, 2, 4),
    age = c(25, 30, 35),
    stringsAsFactors = FALSE
  )

  blk <- new_join_block(type = "left_join", by = "id")

  # Test expression generation in a mock environment
  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Check expression
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("dplyr::left_join", expr_text)))
      expect_true(any(grepl("by", expr_text)))
    }
  )
})

test_that("join block inner_join functionality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  x_data <- data.frame(
    id = c(1, 2, 3),
    value_x = c(10, 20, 30)
  )

  y_data <- data.frame(
    id = c(2, 3, 4),
    value_y = c(200, 300, 400)
  )

  blk <- new_join_block(type = "inner_join", by = "id")

  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("dplyr::inner_join", expr_text)))
    }
  )
})

test_that("join block right_join functionality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  x_data <- data.frame(id = c(1, 2), name = c("A", "B"))
  y_data <- data.frame(id = c(2, 3), age = c(25, 30))

  blk <- new_join_block(type = "right_join", by = "id")

  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("dplyr::right_join", expr_text)))
    }
  )
})

test_that("join block full_join functionality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  x_data <- data.frame(id = c(1, 2), x_val = c(10, 20))
  y_data <- data.frame(id = c(2, 3), y_val = c(200, 300))

  blk <- new_join_block(type = "full_join", by = "id")

  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("dplyr::full_join", expr_text)))
    }
  )
})

test_that("join block semi_join functionality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  x_data <- data.frame(id = c(1, 2, 3), name = c("A", "B", "C"))
  y_data <- data.frame(id = c(1, 2))

  blk <- new_join_block(type = "semi_join", by = "id")

  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("dplyr::semi_join", expr_text)))
    }
  )
})

test_that("join block anti_join functionality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  x_data <- data.frame(id = c(1, 2, 3), name = c("A", "B", "C"))
  y_data <- data.frame(id = c(1, 2))

  blk <- new_join_block(type = "anti_join", by = "id")

  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("dplyr::anti_join", expr_text)))
    }
  )
})

test_that("join block with multiple columns", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  x_data <- data.frame(
    id = c(1, 2, 3),
    group = c("A", "B", "A"),
    value = c(10, 20, 30)
  )

  y_data <- data.frame(
    id = c(1, 2, 3),
    group = c("A", "B", "C"),
    score = c(100, 200, 300)
  )

  blk <- new_join_block(type = "left_join", by = c("id", "group"))

  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl('c\\("id", "group"\\)', expr_text))
    }
  )
})

test_that("join block with different column names", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  x_data <- data.frame(
    user_id = c(1, 2, 3),
    user_name = c("Alice", "Bob", "Charlie")
  )

  y_data <- data.frame(
    id = c(1, 2, 4),
    name = c("Alice", "Bob", "David"),
    age = c(25, 30, 35)
  )

  # Join where x$user_id matches y$id and x$user_name matches y$name
  blk <- new_join_block(
    type = "left_join",
    by = list(user_id = "id", user_name = "name")
  )

  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expect_true(inherits(expr_result, "call"))

      # Expression should contain the by parameter with named vector
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("by", expr_text))
    }
  )
})

test_that("join block state management", {
  skip_if_not_installed("shiny")

  x_data <- data.frame(id = 1:3, x_val = c(10, 20, 30))
  y_data <- data.frame(id = 2:4, y_val = c(200, 300, 400))

  blk <- new_join_block(type = "left_join", by = "id")

  shiny::testServer(
    blk$expr_server,
    args = list(x = reactive(x_data), y = reactive(y_data)),
    {
      session$flushReact()

      result <- session$returned

      # State should be available
      expect_true(is.list(result$state))

      # Should have type and by reactives
      expect_true("type" %in% names(result$state))
      expect_true("by" %in% names(result$state))
      expect_true(is.reactive(result$state$type))
      expect_true(is.reactive(result$state$by))
    }
  )
})

test_that("join block UI generation", {
  blk <- new_join_block(type = "left_join", by = "id")

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain select input for join type
  expect_true(grepl("select", ui_text, ignore.case = TRUE))
})

test_that("join block with no by parameter uses UI interaction", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("shiny")

  # When by is not provided in constructor, it's determined via UI
  x_data <- data.frame(id = 1:3, name = c("A", "B", "C"), x_val = c(10, 20, 30))
  y_data <- data.frame(
    id = 1:3,
    name = c("A", "B", "C"),
    y_val = c(100, 200, 300)
  )

  blk <- new_join_block(type = "left_join")

  # Just verify the block was created successfully
  # The actual join keys would be selected via UI interaction
  expect_s3_class(blk, c("join_block", "transform_block", "block"))
})

# Data transformation tests using block_server
test_that("join block left join - testServer", {
  data_x <- data.frame(id = c(1, 2, 3), name = c('Alice', 'Bob', 'Charlie'))
  data_y <- data.frame(id = c(1, 2, 4), age = c(25, 30, 35))

  block <- new_join_block(type = "left_join", by = c("id"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true("name" %in% names(result))
      expect_true("age" %in% names(result))
      expect_equal(nrow(result), 3)
    },
    args = list(x = block, data = list(x = function() data_x, y = function() data_y))
  )
})

test_that("join block inner join - testServer", {
  data_x <- data.frame(id = c(1, 2, 3), x_val = c(10, 20, 30))
  data_y <- data.frame(id = c(2, 3, 4), y_val = c(200, 300, 400))

  block <- new_join_block(type = "inner_join", by = c("id"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 2)
      expect_true(all(c("x_val", "y_val") %in% names(result)))
    },
    args = list(x = block, data = list(x = function() data_x, y = function() data_y))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("join block - input type changes join behavior - testServer", {
  data_x <- data.frame(id = c(1, 2, 3), name = c("Alice", "Bob", "Charlie"))
  data_y <- data.frame(id = c(2, 3, 4), age = c(25, 30, 35))

  block <- new_join_block(type = "left_join", by = c("id"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result - left_join keeps all rows from left (3 rows)
      result <- session$returned$result()
      expect_equal(nrow(result), 3)
      expect_true(is.na(result$age[result$id == 1]))  # No match for id=1

      # Change to inner_join - only matching rows (2 rows)
      expr$setInputs(type = "inner_join")
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 2)
      expect_false(1 %in% result$id)  # id=1 excluded

      # Change to right_join - keeps all rows from right (3 rows)
      expr$setInputs(type = "right_join")
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 3)
      expect_true(4 %in% result$id)  # id=4 included from right

      # Change to anti_join - rows in left without matches in right
      expr$setInputs(type = "anti_join")
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 1)
      expect_equal(result$id, 1)  # Only id=1 has no match
      expect_false("age" %in% names(result))  # anti_join doesn't add columns
    },
    args = list(x = block, data = list(x = function() data_x, y = function() data_y))
  )
})
