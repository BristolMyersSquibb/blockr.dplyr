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
cat("1/13 - Filter block\n")
validate_block_screenshot(
  block = new_filter_expr_block(exprs = "mpg > 20 & hp > 90"),
  data = mtcars,
  filename = "filter-expr-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# SELECT BLOCK - Choose key performance metrics
# =============================================================================
cat("2/13 - Select block\n")
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
cat("3/13 - Arrange block\n")
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
cat("4/13 - Slice block\n")
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
cat("5/13 - Mutate block\n")
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
cat("6/13 - Rename block\n")
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
cat("7/13 - Summarize block\n")
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
cat("8/13 - Join block\n")
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
cat("9/13 - Bind rows block\n")
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
cat("10/13 - Bind cols block\n")
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
cat("11/13 - Value filter block\n")
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

# =============================================================================
# PIVOT LONGER BLOCK - Reshape wide data to long format (tidyr)
# =============================================================================
cat("12/13 - Pivot longer block\n")
validate_block_screenshot(
  block = new_pivot_longer_block(
    cols = c("mpg", "hp", "wt", "qsec"),
    names_to = "metric",
    values_to = "value"
  ),
  data = mtcars,
  filename = "pivot-longer-block.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 1,
  verbose = FALSE
)

# =============================================================================
# PIVOT WIDER BLOCK - Reshape long data to wide format (tidyr)
# =============================================================================
cat("13/13 - Pivot wider block\n")
# Use ChickWeight dataset - already in long format
validate_block_screenshot(
  block = new_pivot_wider_block(
    names_from = "Time",
    values_from = "weight",
    names_prefix = "day_"
  ),
  data = datasets::ChickWeight[datasets::ChickWeight$Time %in% c(0, 10, 20) &
                                datasets::ChickWeight$Chick %in% c(1, 2, 3, 4, 5), ],
  filename = "pivot-wider-block.png",
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
