test_that("join block constructor with defaults", {
  blk <- new_join_block()
  expect_s3_class(blk, c("join_block", "transform_block", "block"))
})

test_that("join block constructor with type parameter", {
  # Test all join types
  join_types <- c(
    "left_join",
    "inner_join",
    "right_join",
    "full_join",
    "semi_join",
    "anti_join"
  )

  for (jtype in join_types) {
    blk <- new_join_block(type = jtype)
    expect_s3_class(blk, c("join_block", "transform_block", "block"))
  }
})

test_that("join block constructor with by parameter", {
  # Single column join
  blk1 <- new_join_block(by = "id")
  expect_s3_class(blk1, c("join_block", "transform_block", "block"))

  # Multiple column join
  blk2 <- new_join_block(by = c("id", "name"))
  expect_s3_class(blk2, c("join_block", "transform_block", "block"))

  # Named join (different column names in x and y)
  blk3 <- new_join_block(by = list(id = "user_id", name = "user_name"))
  expect_s3_class(blk3, c("join_block", "transform_block", "block"))
})

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
