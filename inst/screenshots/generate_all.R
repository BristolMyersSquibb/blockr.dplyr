#!/usr/bin/env Rscript

# Generate screenshots for all blockr.dplyr blocks
#
# This script creates screenshots of all blocks for use in pkgdown documentation.
# Screenshots are saved to man/figures/ directory.
#
# To run: source("inst/screenshots/generate_all.R")

# Set NOT_CRAN environment variable BEFORE loading any packages
# This is required for shinytest2 to work in non-interactive mode
Sys.setenv(NOT_CRAN = "true")

# Load package with devtools::load_all() to ensure latest changes are picked up
devtools::load_all(".")

# Source the validation function
source("inst/screenshots/validate-screenshot.R")

cat("Generating screenshots for all blockr.dplyr blocks...\n")
cat("Output directory: man/figures/\n\n")

# =============================================================================
# FILTER BLOCK - Filter high-performance cars
# =============================================================================
cat("1/11 - Filter block\n")
validate_block_screenshot(
  block = new_filter_expr_block(exprs = "mpg > 20 & hp > 90"),
  data = mtcars,
  filename = "filter-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# SELECT BLOCK - Choose key performance metrics
# =============================================================================
cat("2/11 - Select block\n")
validate_block_screenshot(
  block = new_select_block(columns = c("mpg", "cyl", "hp", "wt", "qsec")),
  data = mtcars,
  filename = "select-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# ARRANGE BLOCK - Sort by horsepower descending
# =============================================================================
cat("3/11 - Arrange block\n")
validate_block_screenshot(
  block = new_arrange_block(columns = "hp", desc = TRUE),
  data = mtcars,
  filename = "arrange-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# SLICE BLOCK - Top 5 most fuel-efficient cars
# =============================================================================
cat("4/11 - Slice block\n")
validate_block_screenshot(
  block = new_slice_block(type = "max", n = 5, by_column = "mpg"),
  data = mtcars,
  filename = "slice-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# MUTATE BLOCK - Calculate power-to-weight ratio
# =============================================================================
cat("5/11 - Mutate block\n")
validate_block_screenshot(
  block = new_mutate_block(
    exprs = list(
      power_to_weight = "hp / wt",
      kmpg = "mpg * 1.60934"
    )
  ),
  data = mtcars,
  filename = "mutate-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# RENAME BLOCK - Make column names more descriptive
# =============================================================================
cat("6/11 - Rename block\n")
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
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# SUMMARIZE BLOCK - Statistics by cylinder count (with advanced options)
# =============================================================================
cat("7/11 - Summarize block\n")
validate_block_screenshot(
  block = new_summarize_block(
    exprs = list(
      avg_mpg = "mean(mpg)",
      avg_hp = "mean(hp)",
      count = "dplyr::n()"
    ),
    by = "cyl"
  ),
  data = mtcars,
  filename = "summarize-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  expand_advanced = TRUE,
  verbose = FALSE
)

# =============================================================================
# JOIN BLOCK - Join performance and efficiency data
# =============================================================================
cat("8/11 - Join block\n")
validate_block_screenshot(
  block = new_join_block(type = "left_join", by = c("mpg", "cyl")),
  data = list(
    x = mtcars[1:15, c("mpg", "cyl", "hp", "disp")],
    y = mtcars[10:25, c("mpg", "cyl", "wt", "qsec")]
  ),
  filename = "join-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# BIND ROWS BLOCK - Stack datasets vertically (with advanced options)
# =============================================================================
cat("9/11 - Bind rows block\n")
validate_block_screenshot(
  block = new_bind_rows_block(id_name = "source"),
  data = mtcars,
  filename = "bind-rows-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  expand_advanced = TRUE,
  verbose = FALSE
)

# =============================================================================
# BIND COLS BLOCK - Combine datasets side-by-side
# =============================================================================
cat("10/11 - Bind cols block\n")
validate_block_screenshot(
  block = new_bind_cols_block(),
  data = mtcars,
  filename = "bind-cols-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# VALUE FILTER BLOCK - Interactive value selection
# =============================================================================
cat("11/11 - Value filter block\n")
validate_block_screenshot(
  block = new_value_filter_block(
    conditions = list(
      list(column = "cyl", values = c(4, 6), mode = "include")
    )
  ),
  data = mtcars,
  filename = "value-filter-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

cat("\n✓ All screenshots generated successfully!\n")
cat("Screenshots saved to: man/figures/\n\n")

# =============================================================================
# VALIDATION: Check screenshots match registry
# =============================================================================

source("inst/screenshots/validate_registry.R")
