#!/usr/bin/env Rscript

# Generate screenshots for all blockr.dplyr blocks
#
# This script creates screenshots of all blocks for use in pkgdown documentation.
# Screenshots are saved to man/figures/ directory.
#
# To run: source("dev/screenshots/generate_all.R")

# Set NOT_CRAN environment variable BEFORE loading any packages
# This is required for shinytest2 to work in non-interactive mode
Sys.setenv(NOT_CRAN = "true")

# Load package with devtools::load_all() to ensure latest changes are picked up
devtools::load_all(".")

# Source the validation function
source("dev/screenshots/validate-screenshot.R")

cat("Generating screenshots for all blockr.dplyr blocks...\n")
cat("Output directory: man/figures/\n\n")

# Common screenshot settings
SCREENSHOT_WIDTH <- 1400
SCREENSHOT_HEIGHT <- 700
SCREENSHOT_DELAY <- 3

# =============================================================================
# 1. ARRANGE BLOCK - Sort by horsepower descending
# =============================================================================
cat("1/16 - Arrange block\n")
validate_block_screenshot(
  block = new_arrange_block(columns = "hp", desc = TRUE),
  data = mtcars,
  filename = "arrange-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 2. BIND COLS BLOCK - Combine datasets side-by-side
# Uses iris twice to show columns being combined
# =============================================================================
cat("2/16 - Bind cols block\n")
validate_block_screenshot(
  block = new_bind_cols_block(),
  data = datasets::iris,
  filename = "bind-cols-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "iris",
  dataset_package = "datasets",
  dataset_y = "iris",
  dataset_y_package = "datasets",
  input_names = c("1", "2"),
  verbose = FALSE
)

# =============================================================================
# 3. BIND ROWS BLOCK - Stack datasets vertically
# Uses iris twice to show rows being stacked
# =============================================================================
cat("3/16 - Bind rows block\n")
validate_block_screenshot(
  block = new_bind_rows_block(id_name = "source"),
  data = datasets::iris,
  filename = "bind-rows-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "iris",
  dataset_package = "datasets",
  dataset_y = "iris",
  dataset_y_package = "datasets",
  input_names = c("1", "2"),
  verbose = FALSE
)

# =============================================================================
# 4. FILTER BLOCK - Interactive value selection (formerly value_filter)
# =============================================================================
cat("4/16 - Filter block\n")
validate_block_screenshot(
  block = new_filter_block(
    conditions = list(
      list(column = "cyl", values = c(4, 6), mode = "include")
    )
  ),
  data = mtcars,
  filename = "filter-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 5. FILTER EXPR BLOCK - Filter with R expressions
# =============================================================================
cat("5/16 - Filter expr block\n")
validate_block_screenshot(
  block = new_filter_expr_block(exprs = "mpg > 20 & hp > 90"),
  data = mtcars,
  filename = "filter-expr-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 6. JOIN BLOCK - Join two datasets
# Uses BOD dataset for both x and y (joined on Time column)
# =============================================================================
cat("6/16 - Join block\n")
validate_block_screenshot(
  block = new_join_block(type = "left_join", by = "Time"),
  data = datasets::BOD,
  filename = "join-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "BOD",
  dataset_package = "datasets",
  dataset_y = "BOD",
  dataset_y_package = "datasets",
  verbose = FALSE
)

# =============================================================================
# 7. MUTATE EXPR BLOCK - Create new columns with expressions
# =============================================================================
cat("7/16 - Mutate expr block\n")
validate_block_screenshot(
  block = new_mutate_expr_block(
    exprs = list(
      power_to_weight = "hp / wt",
      kmpg = "mpg * 1.60934"
    )
  ),
  data = mtcars,
  filename = "mutate-expr-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 8. PIVOT LONGER BLOCK - Reshape wide to long format
# =============================================================================
cat("8/16 - Pivot longer block\n")
validate_block_screenshot(
  block = new_pivot_longer_block(
    cols = c("mpg", "hp", "wt", "qsec"),
    names_to = "metric",
    values_to = "value"
  ),
  data = mtcars,
  filename = "pivot-longer-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 9. PIVOT WIDER BLOCK - Reshape long to wide format
# =============================================================================
cat("9/16 - Pivot wider block\n")
validate_block_screenshot(
  block = new_pivot_wider_block(
    names_from = "Time",
    values_from = "weight",
    names_prefix = "day_"
  ),
  data = datasets::ChickWeight[
    datasets::ChickWeight$Time %in% c(0, 10, 20) &
      datasets::ChickWeight$Chick %in% c(1, 2, 3, 4, 5),
  ],
  filename = "pivot-wider-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 10. RENAME BLOCK - Rename columns
# =============================================================================
cat("10/16 - Rename block\n")
validate_block_screenshot(
  block = new_rename_block(
    renames = list(
      miles_per_gallon = "mpg",
      horsepower = "hp",
      weight_1000lbs = "wt"
    )
  ),
  data = mtcars,
  filename = "rename-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 11. SELECT BLOCK - Choose columns
# =============================================================================
cat("11/16 - Select block\n")
validate_block_screenshot(
  block = new_select_block(columns = c("mpg", "cyl", "hp", "wt", "qsec")),
  data = mtcars,
  filename = "select-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 12. SEPARATE BLOCK - Split column into multiple columns
# =============================================================================
cat("12/16 - Separate block\n")
# Use esoph dataset which has agegp column like "25-34" that can be separated
validate_block_screenshot(
  block = new_separate_block(
    col = "agegp",
    into = c("age_min", "age_max"),
    sep = "-"
  ),
  data = datasets::esoph,
  filename = "separate-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "esoph",
  dataset_package = "datasets",
  verbose = FALSE
)

# =============================================================================
# 13. SLICE BLOCK - Select rows by position
# =============================================================================
cat("13/16 - Slice block\n")
validate_block_screenshot(
  block = new_slice_block(type = "max", n = 5, by_column = "mpg"),
  data = mtcars,
  filename = "slice-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 14. SUMMARIZE BLOCK - No-code summarization
# =============================================================================
cat("14/16 - Summarize block\n")
validate_block_screenshot(
  block = new_summarize_block(
    summaries = list(
      avg_mpg = list(func = "mean", col = "mpg"),
      max_hp = list(func = "max", col = "hp")
    ),
    by = "cyl"
  ),
  data = mtcars,
  filename = "summarize-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 15. SUMMARIZE EXPR BLOCK - Summarize with expressions
# =============================================================================
cat("15/16 - Summarize expr block\n")
validate_block_screenshot(
  block = new_summarize_expr_block(
    exprs = list(
      avg_mpg = "mean(mpg)",
      avg_hp = "mean(hp)",
      count = "dplyr::n()"
    ),
    by = "cyl"
  ),
  data = mtcars,
  filename = "summarize-expr-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 16. UNITE BLOCK - Combine multiple columns into one
# =============================================================================
cat("16/16 - Unite block\n")
# Unite cyl and gear columns from mtcars
validate_block_screenshot(
  block = new_unite_block(
    col = "cyl_gear",
    cols = c("cyl", "gear"),
    sep = "_"
  ),
  data = mtcars,
  filename = "unite-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

cat("\n✓ All screenshots generated!\n")
cat("Screenshots saved to: man/figures/\n\n")

# =============================================================================
# VALIDATION: Check screenshots match registry
# =============================================================================

source("dev/screenshots/validate_registry.R")
