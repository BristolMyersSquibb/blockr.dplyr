# Basic construction tests
test_that("unite block constructor", {
  blk <- new_unite_block()
  expect_s3_class(blk, c("unite_block", "transform_block", "block"))
})

test_that("unite block with parameters", {
  blk <- new_unite_block(
    col = "full_name",
    cols = c("first", "last"),
    sep = " "
  )
  expect_s3_class(blk, c("unite_block", "transform_block", "block"))
})

# testServer tests for data transformation
test_that("unite basic transformation - testServer", {
  # Create test data
  test_data <- data.frame(
    first_name = c("John", "Jane", "Bob"),
    last_name = c("Doe", "Smith", "Johnson"),
    age = c(30, 25, 35)
  )

  block <- new_unite_block(
    col = "full_name",
    cols = c("first_name", "last_name"),
    sep = " "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have full_name and age columns (first_name and last_name removed)
      expect_true("full_name" %in% names(result))
      expect_true("age" %in% names(result))
      expect_false("first_name" %in% names(result))
      expect_false("last_name" %in% names(result))
      # Check combined values
      expect_equal(result$full_name[1], "John Doe")
      expect_equal(result$full_name[2], "Jane Smith")
      expect_equal(result$full_name[3], "Bob Johnson")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite with custom separator - testServer", {
  test_data <- data.frame(
    year = c("2024", "2024", "2024"),
    month = c("01", "02", "03"),
    day = c("15", "20", "25")
  )

  block <- new_unite_block(
    col = "date",
    cols = c("year", "month", "day"),
    sep = "-"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true("date" %in% names(result))
      expect_equal(result$date[1], "2024-01-15")
      expect_equal(result$date[2], "2024-02-20")
      expect_equal(result$date[3], "2024-03-25")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite with remove=FALSE - testServer", {
  test_data <- data.frame(
    first = c("John", "Jane"),
    last = c("Doe", "Smith")
  )

  block <- new_unite_block(
    col = "full_name",
    cols = c("first", "last"),
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
      expect_true(all(c("first", "last", "full_name") %in% names(result)))
      expect_equal(result$full_name[1], "John Doe")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite with na.rm=TRUE - testServer", {
  test_data <- data.frame(
    prefix = c("Dr.", NA, "Prof."),
    first = c("John", "Jane", "Bob"),
    last = c("Doe", "Smith", "Johnson")
  )

  # Without NA removal
  block_keep_na <- new_unite_block(
    col = "full_name",
    cols = c("prefix", "first", "last"),
    sep = " ",
    na.rm = FALSE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_keep_na),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # NA values should appear as "NA" in the string
      expect_true(grepl("NA", result$full_name[2]))
    },
    args = list(x = block_keep_na, data = list(data = function() test_data))
  )

  # With NA removal
  block_drop_na <- new_unite_block(
    col = "full_name",
    cols = c("prefix", "first", "last"),
    sep = " ",
    na.rm = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_drop_na),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # NA values should be omitted
      expect_equal(result$full_name[1], "Dr. John Doe")
      expect_equal(result$full_name[2], "Jane Smith")  # No "NA" in string
      expect_equal(result$full_name[3], "Prof. Bob Johnson")
    },
    args = list(x = block_drop_na, data = list(data = function() test_data))
  )
})

test_that("unite with multiple columns - testServer", {
  test_data <- data.frame(
    country = c("USA", "UK", "Canada"),
    state = c("CA", "England", "ON"),
    city = c("LA", "London", "Toronto"),
    zip = c("90001", "SW1A", "M5H")
  )

  block <- new_unite_block(
    col = "location",
    cols = c("city", "state", "country", "zip"),
    sep = ", "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true("location" %in% names(result))
      expect_equal(result$location[1], "LA, CA, USA, 90001")
      expect_equal(result$location[2], "London, England, UK, SW1A")
      expect_equal(result$location[3], "Toronto, ON, Canada, M5H")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite with non-syntactic column names - testServer", {
  test_data <- data.frame(
    a = c("x", "y"),
    b = c("1", "2")
  )
  names(test_data) <- c("First Name", "Last Name")

  block <- new_unite_block(
    col = "Full Name",
    cols = c("First Name", "Last Name"),
    sep = " "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true("Full Name" %in% names(result))
      expect_equal(result$`Full Name`[1], "x 1")
      expect_equal(result$`Full Name`[2], "y 2")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite full parameter combination - testServer", {
  test_data <- data.frame(
    dept = c("Sales", "IT", "HR"),
    division = c(NA, "Tech", "Admin"),
    location = c("NYC", "SF", "LA"),
    code = c("001", "002", "003")
  )

  block <- new_unite_block(
    col = "department_id",
    cols = c("dept", "division", "location", "code"),
    sep = "-",
    remove = FALSE,
    na.rm = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # All original columns should remain (remove = FALSE)
      expect_true(all(c("dept", "division", "location", "code", "department_id") %in% names(result)))
      # NA should be omitted (na.rm = TRUE)
      expect_equal(result$department_id[1], "Sales-NYC-001")
      expect_equal(result$department_id[2], "IT-Tech-SF-002")
      expect_equal(result$department_id[3], "HR-Admin-LA-003")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite with underscore separator (default) - testServer", {
  test_data <- data.frame(
    category = c("A", "B", "C"),
    subcategory = c("1", "2", "3"),
    item = c("x", "y", "z")
  )

  block <- new_unite_block(
    col = "item_id",
    cols = c("category", "subcategory", "item")
    # sep defaults to "_"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(result$item_id[1], "A_1_x")
      expect_equal(result$item_id[2], "B_2_y")
      expect_equal(result$item_id[3], "C_3_z")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("unite - input col changes output column name - testServer", {
  test_data <- data.frame(
    first = c("John", "Jane"),
    last = c("Doe", "Smith")
  )

  block <- new_unite_block(
    col = "full_name",
    cols = c("first", "last"),
    sep = " "
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result
      result <- session$returned$result()
      expect_true("full_name" %in% names(result))
      expect_equal(result$full_name[1], "John Doe")

      # Change column name
      expr$setInputs(col = "combined_name")
      session$flushReact()
      result <- session$returned$result()
      expect_true("combined_name" %in% names(result))
      expect_false("full_name" %in% names(result))
      expect_equal(result$combined_name[1], "John Doe")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite - input sep changes output separator - testServer", {
  test_data <- data.frame(
    year = c("2024", "2024"),
    month = c("01", "02"),
    day = c("15", "20")
  )

  block <- new_unite_block(
    col = "date",
    cols = c("year", "month", "day"),
    sep = "-"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result with "-"
      result <- session$returned$result()
      expect_equal(result$date[1], "2024-01-15")

      # Change separator to "/"
      expr$setInputs(sep = "/")
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$date[1], "2024/01/15")
      expect_equal(result$date[2], "2024/02/20")

      # Change separator to space
      expr$setInputs(sep = " ")
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$date[1], "2024 01 15")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite - input remove changes column retention - testServer", {
  test_data <- data.frame(
    first = c("John", "Jane"),
    last = c("Doe", "Smith"),
    age = c(30, 25)
  )

  block <- new_unite_block(
    col = "full_name",
    cols = c("first", "last"),
    sep = " ",
    remove = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result - columns removed
      result <- session$returned$result()
      expect_true("full_name" %in% names(result))
      expect_false("first" %in% names(result))
      expect_false("last" %in% names(result))

      # Change remove to FALSE
      expr$setInputs(remove = FALSE)
      session$flushReact()
      result <- session$returned$result()
      expect_true("full_name" %in% names(result))
      expect_true("first" %in% names(result))
      expect_true("last" %in% names(result))
      expect_equal(result$full_name[1], "John Doe")
      expect_equal(result$first[1], "John")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("unite - input na_rm changes NA handling - testServer", {
  test_data <- data.frame(
    prefix = c("Dr.", NA, "Prof."),
    name = c("John", "Jane", "Bob")
  )

  block <- new_unite_block(
    col = "full",
    cols = c("prefix", "name"),
    sep = " ",
    na.rm = FALSE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result - NA included as "NA"
      result <- session$returned$result()
      expect_true(grepl("NA", result$full[2]))

      # Change na_rm to TRUE
      expr$setInputs(na_rm = TRUE)
      session$flushReact()
      result <- session$returned$result()
      # NA should be omitted
      expect_equal(result$full[1], "Dr. John")
      expect_equal(result$full[2], "Jane")  # No "NA" prefix
      expect_equal(result$full[3], "Prof. Bob")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})
