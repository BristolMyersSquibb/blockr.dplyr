# AI Discovery Test v2 — Harder queries, verbose conversations
#
# Tests more complex / edge-case prompts against all blocks.
# Verbose mode shows full LLM conversations for quality review.
#
# Requires: OPENAI_API_KEY environment variable
#
# Usage:
#   cd blockr.dplyr && Rscript dev/test-ai-discovery-v2.R

pkgload::load_all("/workspace/blockr.dplyr")
library(blockr.ai)

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("OPENAI_API_KEY not set. Export it before running this script.")
}

# --- Test data ---

df_sales <- data.frame(
  product = rep(c("Widget", "Gadget", "Doohickey"), each = 4),
  quarter = rep(paste0("Q", 1:4), 3),
  revenue = c(100, 150, 200, 180, 80, 90, 110, 95, 50, 60, 70, 65),
  units = c(10, 15, 20, 18, 8, 9, 11, 10, 5, 6, 7, 7),
  region = rep(c("North", "South"), 6),
  stringsAsFactors = FALSE
)

df_messy <- data.frame(
  id = 1:5,
  full_address = c("123 Main St, NYC", "456 Oak Ave, LA",
                    "789 Pine Rd, CHI", "101 Elm Dr, SF", "202 Cedar Ln, BOS"),
  score_2024 = c(85, 92, 78, 95, 88),
  score_2025 = c(90, 88, 82, 97, 91),
  stringsAsFactors = FALSE
)

# --- Harder test cases ---

tests <- list(
  # Filter: multiple conditions, numeric comparison
  list(
    name = "filter-multi",
    block = new_filter_block(),
    data = df_sales,
    prompt = "show only Widget products with revenue above 150"
  ),

  # Filter: exclude mode
  list(
    name = "filter-exclude",
    block = new_filter_block(),
    data = iris,
    prompt = "remove all virginica rows"
  ),

  # Select: exclude mode
  list(
    name = "select-exclude",
    block = new_select_block(),
    data = mtcars,
    prompt = "drop the qsec and vs columns"
  ),

  # Arrange: multi-column sort
  list(
    name = "arrange-multi",
    block = new_arrange_block(),
    data = df_sales,
    prompt = "sort by product alphabetically then by revenue highest first"
  ),

  # Rename: multiple renames
  list(
    name = "rename-multi",
    block = new_rename_block(),
    data = df_sales,
    prompt = "rename revenue to total_revenue and units to units_sold"
  ),

  # Mutate: complex expression
  list(
    name = "mutate-complex",
    block = new_mutate_block(),
    data = df_sales,
    prompt = "add revenue_per_unit as revenue divided by units, and a flag column called high_revenue that is TRUE when revenue is above 100"
  ),

  # Mutate: with grouping
  list(
    name = "mutate-grouped",
    block = new_mutate_block(),
    data = df_sales,
    prompt = "add a column pct_of_product that shows each row's revenue as a percentage of its product's total revenue, grouped by product"
  ),

  # Summarize: multiple summaries + grouping
  list(
    name = "summarize-multi",
    block = new_summarize_block(),
    data = df_sales,
    prompt = "for each product, calculate total revenue, average units, and count of rows"
  ),

  # Summarize: expression mode
  list(
    name = "summarize-expr",
    block = new_summarize_block(),
    data = df_sales,
    prompt = "calculate the ratio of max revenue to min revenue for each product"
  ),

  # Slice: sample mode
  list(
    name = "slice-sample",
    block = new_slice_block(),
    data = mtcars,
    prompt = "random sample of 5 rows"
  ),

  # Slice: min/max with order_by
  list(
    name = "slice-top",
    block = new_slice_block(),
    data = df_sales,
    prompt = "top 3 rows by revenue"
  ),

  # Pivot longer: year columns
  list(
    name = "pivot-years",
    block = new_pivot_longer_block(),
    data = df_messy,
    prompt = "pivot score_2024 and score_2025 into long format with a year column and a score column"
  ),

  # Pivot wider: from long sales data
  list(
    name = "pivot-quarters",
    block = new_pivot_wider_block(),
    data = df_sales,
    prompt = "pivot so each quarter becomes a column with revenue values, keeping product as the row identifier"
  ),

  # Unite: with custom separator
  list(
    name = "unite-custom",
    block = new_unite_block(),
    data = df_sales,
    prompt = "combine product and quarter into a single column called product_quarter with an underscore"
  ),

  # Separate: real-world split
  list(
    name = "separate-address",
    block = new_separate_block(),
    data = df_messy,
    prompt = "split full_address into street and city using comma as separator"
  )
)

# --- Run tests with verbose output ---

cat("\n========================================\n")
cat("blockr.dplyr AI Discovery Test v2\n")
cat("========================================\n\n")

pass <- 0
fail <- 0
iterations <- c()

for (t in tests) {
  cat(sprintf("\n--- %s ---\n", t$name))
  cat(sprintf("Prompt: %s\n", t$prompt))

  result <- tryCatch(
    discover_block_args(
      prompt = t$prompt,
      block = t$block,
      data = t$data,
      verbose = TRUE
    ),
    error = function(e) {
      list(success = FALSE, error = conditionMessage(e), conversation = list())
    }
  )

  n_iter <- length(Filter(function(m) m$role == "user", result$conversation %||% list()))
  iterations <- c(iterations, n_iter)

  if (isTRUE(result$success)) {
    rows <- if (is.data.frame(result$result)) nrow(result$result) else "?"
    cat(sprintf("RESULT: PASS (%s rows, %d iterations)\n", rows, n_iter))
    if (!is.null(result$args)) {
      cat("Args: ", jsonlite::toJSON(result$args, auto_unbox = TRUE), "\n")
    }
    pass <- pass + 1
  } else {
    cat(sprintf("RESULT: FAIL (%s, %d iterations)\n", result$error %||% "unknown", n_iter))
    fail <- fail + 1
  }
}

cat(sprintf("\n========================================\n"))
cat(sprintf("SUMMARY: %d passed, %d failed\n", pass, fail))
cat(sprintf("Average iterations: %.1f\n", mean(iterations)))
cat(sprintf("========================================\n"))
