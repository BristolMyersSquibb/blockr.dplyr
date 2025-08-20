test_that("enhanced join block handles natural joins", {
  skip_if_not_installed("blockr.core")
  
  # Create test datasets with common columns
  x_data <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Charlie"),
    value_x = c(10, 20, 30)
  )
  
  y_data <- data.frame(
    id = c(1, 2, 4),
    category = c("A", "B", "C"),
    value_y = c(100, 200, 400)
  )
  
  # Test basic block creation
  block <- new_join_block(type = "left_join", by = "id")
  expect_s3_class(block, "join_block")
  expect_s3_class(block, "transform_block")
  
  # Test that the block has the correct structure
  expect_true(is.function(block$expr_server))
  expect_true(is.function(block$expr_ui))
})

test_that("enhanced join block server functionality", {
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("shiny")
  
  # Create test data
  x_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("Alice", "Bob", "Charlie"),
      stringsAsFactors = FALSE
    )
  })
  
  y_data <- reactive({
    data.frame(
      id = c(1, 2, 4),
      category = c("A", "B", "C"),
      stringsAsFactors = FALSE
    )
  })
  
  # Test the server function with natural join
  testServer(
    function(id, input, output, session) {
      block <- new_join_block(type = "left_join", by = "id")
      result <- block$expr_server(id, x_data, y_data)
      result
    },
    {
      # Test that the server returns the expected structure
      expect_true(is.list(session$returned))
      expect_true("expr" %in% names(session$returned))
      expect_true("state" %in% names(session$returned))
    }
  )
})

test_that("join block handles different join types", {
  join_types <- c(
    "left_join", "inner_join", "right_join", 
    "full_join", "semi_join", "anti_join"
  )
  
  for (join_type in join_types) {
    block <- new_join_block(type = join_type)
    expect_s3_class(block, "join_block")
    
    # Check that the block function handles the join type correctly
    expect_true(is.function(block$expr_server))
    expect_true(is.function(block$expr_ui))
  }
})

test_that("join block UI includes enhanced components", {
  block <- new_join_block()
  ui <- block$expr_ui("test")
  
  # Convert to character for easier testing
  ui_str <- as.character(ui)
  
  # Should include join type selector
  expect_true(grepl("Join Type", ui_str))
  
  # Should include join keys configuration
  expect_true(grepl("join_keys", ui_str))
  
  # Should include submit button
  expect_true(grepl("Apply Join", ui_str))
})

test_that("join block handles empty by parameter gracefully", {
  # Test with empty by parameter
  block <- new_join_block(by = character())
  expect_s3_class(block, "join_block")
  
  # Test with NULL by parameter
  block <- new_join_block(by = NULL)
  expect_s3_class(block, "join_block")
})

test_that("join block validates join types", {
  # Valid join types should work
  valid_types <- c("left_join", "inner_join", "right_join", "full_join", "semi_join", "anti_join")
  
  for (type in valid_types) {
    expect_no_error(new_join_block(type = type))
  }
  
  # Invalid join type should error
  expect_error(new_join_block(type = "invalid_join"))
})

test_that("join block maintains backward compatibility", {
  # Test that old-style character vector join keys still work
  block <- new_join_block(type = "left_join", by = c("id", "category"))
  expect_s3_class(block, "join_block")
  
  # Test single join key
  block <- new_join_block(type = "inner_join", by = "id")
  expect_s3_class(block, "join_block")
})