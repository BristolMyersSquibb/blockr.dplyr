# Basic construction tests
test_that("separate block constructor", {
  blk <- new_separate_block()
  expect_s3_class(blk, c("separate_block", "transform_block", "block"))
})

test_that("separate block with parameters", {
  blk <- new_separate_block(
    col = "full_name",
    into = c("first", "last"),
    sep = " "
  )
  expect_s3_class(blk, c("separate_block", "transform_block", "block"))
})

# testServer tests for data transformation
test_that("separate basic transformation - testServer", {
  # Create test data
  test_data <- data.frame(
    full_name = c("John Doe", "Jane Smith", "Bob Johnson"),
    age = c(30, 25, 35)
  )

  block <- new_separate_block(
    col = "full_name",
    into = c("first_name", "last_name"),
    sep = " "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have first_name, last_name, and age columns
      expect_true(all(c("first_name", "last_name", "age") %in% names(result)))
      # full_name should be removed (default remove = TRUE)
      expect_false("full_name" %in% names(result))
      # Check separated values
      expect_equal(result$first_name[1], "John")
      expect_equal(result$last_name[1], "Doe")
      expect_equal(result$first_name[2], "Jane")
      expect_equal(result$last_name[2], "Smith")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with custom separator - testServer", {
  test_data <- data.frame(
    date_string = c("2024-01-15", "2024-02-20", "2024-03-25")
  )

  block <- new_separate_block(
    col = "date_string",
    into = c("year", "month", "day"),
    sep = "-"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true(all(c("year", "month", "day") %in% names(result)))
      expect_equal(result$year[1], "2024")
      expect_equal(result$month[1], "01")
      expect_equal(result$day[1], "15")
      expect_equal(result$year[2], "2024")
      expect_equal(result$month[2], "02")
      expect_equal(result$day[2], "20")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with remove=FALSE - testServer", {
  test_data <- data.frame(
    full_name = c("John Doe", "Jane Smith"),
    id = c(1, 2)
  )

  block <- new_separate_block(
    col = "full_name",
    into = c("first", "last"),
    sep = " ",
    remove = FALSE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # All columns should be present (remove = FALSE)
      expect_true(all(c("full_name", "first", "last", "id") %in% names(result)))
      expect_equal(result$full_name[1], "John Doe")
      expect_equal(result$first[1], "John")
      expect_equal(result$last[1], "Doe")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with convert=TRUE - testServer", {
  test_data <- data.frame(
    data_string = c("10-20", "30-40", "50-60")
  )

  block <- new_separate_block(
    col = "data_string",
    into = c("min_val", "max_val"),
    sep = "-",
    convert = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # With convert=TRUE, numeric strings should be converted to integers
      expect_true(is.numeric(result$min_val))
      expect_true(is.numeric(result$max_val))
      expect_equal(result$min_val[1], 10)
      expect_equal(result$max_val[1], 20)
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with regex separator - testServer", {
  test_data <- data.frame(
    mixed_col = c("a-b", "c_d", "e.f")
  )

  # Use regex to match any of: hyphen, underscore, or period
  block <- new_separate_block(
    col = "mixed_col",
    into = c("col1", "col2"),
    sep = "[-_.]"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(result$col1[1], "a")
      expect_equal(result$col2[1], "b")
      expect_equal(result$col1[2], "c")
      expect_equal(result$col2[2], "d")
      expect_equal(result$col1[3], "e")
      expect_equal(result$col2[3], "f")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with extra='drop' - testServer", {
  test_data <- data.frame(
    address = c("123 Main St Apt 4", "456 Oak Ave")
  )

  block <- new_separate_block(
    col = "address",
    into = c("number", "street"),
    sep = " ",
    extra = "drop"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Extra pieces should be dropped silently
      expect_equal(result$number[1], "123")
      expect_equal(result$street[1], "Main")
      expect_equal(result$number[2], "456")
      expect_equal(result$street[2], "Oak")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with extra='merge' - testServer", {
  test_data <- data.frame(
    address = c("123 Main St Apt 4", "456 Oak Ave Unit 2B")
  )

  block <- new_separate_block(
    col = "address",
    into = c("number", "street", "rest"),
    sep = " ",
    extra = "merge"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Extra pieces should be merged into the last column
      expect_equal(result$number[1], "123")
      expect_equal(result$street[1], "Main")
      expect_equal(result$rest[1], "St Apt 4")
      expect_equal(result$number[2], "456")
      expect_equal(result$street[2], "Oak")
      expect_equal(result$rest[2], "Ave Unit 2B")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with fill='right' - testServer", {
  test_data <- data.frame(
    name = c("John Doe Smith", "Jane", "Bob Johnson")
  )

  block <- new_separate_block(
    col = "name",
    into = c("first", "middle", "last"),
    sep = " ",
    fill = "right"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Row 2 has only one piece, should fill with NA on the right
      expect_equal(result$first[2], "Jane")
      expect_true(is.na(result$middle[2]))
      expect_true(is.na(result$last[2]))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with fill='left' - testServer", {
  test_data <- data.frame(
    name = c("John Doe Smith", "Jane", "Bob Johnson")
  )

  block <- new_separate_block(
    col = "name",
    into = c("first", "middle", "last"),
    sep = " ",
    fill = "left"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Row 2 has only one piece, should fill with NA on the left
      expect_true(is.na(result$first[2]))
      expect_true(is.na(result$middle[2]))
      expect_equal(result$last[2], "Jane")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with non-syntactic column names - testServer", {
  test_data <- data.frame(
    a = c("x y", "a b")
  )
  names(test_data) <- c("Full Name")

  block <- new_separate_block(
    col = "Full Name",
    into = c("First Name", "Last Name"),
    sep = " "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true("First Name" %in% names(result))
      expect_true("Last Name" %in% names(result))
      expect_equal(result$`First Name`[1], "x")
      expect_equal(result$`Last Name`[1], "y")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with default regex separator - testServer", {
  test_data <- data.frame(
    mixed = c("a-b", "c_d", "e.f", "g h")
  )

  # Default sep = "[^[:alnum:]]+" matches any non-alphanumeric
  block <- new_separate_block(
    col = "mixed",
    into = c("col1", "col2")
    # sep defaults to "[^[:alnum:]]+"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # All rows should be separated correctly by the default regex
      expect_equal(result$col1[1], "a")
      expect_equal(result$col2[1], "b")
      expect_equal(result$col1[2], "c")
      expect_equal(result$col2[2], "d")
      expect_equal(result$col1[3], "e")
      expect_equal(result$col2[3], "f")
      expect_equal(result$col1[4], "g")
      expect_equal(result$col2[4], "h")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate full parameter combination - testServer", {
  test_data <- data.frame(
    product_data = c("A1-100-red", "B2-200", "C3-300-blue-large"),
    id = c(1, 2, 3)
  )

  block <- new_separate_block(
    col = "product_data",
    into = c("code", "price", "details"),
    sep = "-",
    remove = FALSE,
    convert = FALSE,
    extra = "merge",
    fill = "right"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Original column should remain (remove = FALSE)
      expect_true("product_data" %in% names(result))
      # Row 1: Normal case
      expect_equal(result$code[1], "A1")
      expect_equal(result$price[1], "100")
      expect_equal(result$details[1], "red")
      # Row 2: Missing piece, should fill right with NA
      expect_equal(result$code[2], "B2")
      expect_equal(result$price[2], "200")
      expect_true(is.na(result$details[2]))
      # Row 3: Extra pieces, should merge into last column
      expect_equal(result$code[3], "C3")
      expect_equal(result$price[3], "300")
      expect_equal(result$details[3], "blue-large")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("separate with comma-separated string for into parameter - testServer", {
  test_data <- data.frame(
    full_name = c("John Doe", "Jane Smith")
  )

  # Test that into can be specified as comma-separated string
  block <- new_separate_block(
    col = "full_name",
    into = "first_name, last_name",  # Single string with comma
    sep = " "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true(all(c("first_name", "last_name") %in% names(result)))
      expect_equal(result$first_name[1], "John")
      expect_equal(result$last_name[1], "Doe")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})
