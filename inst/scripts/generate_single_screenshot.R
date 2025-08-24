#!/usr/bin/env Rscript

#' Generate Single Screenshot for blockr.dplyr
#'
#' This script generates one screenshot at a time to validate block implementations.
#' Run this script multiple times, changing the block_type parameter.
#'
#' Usage:
#' R -e "block_type <- 'select'; source('inst/scripts/generate_single_screenshot.R')"

# Check dependencies
if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("Please install webshot2: install.packages('webshot2')")
}

if (!requireNamespace("blockr.dplyr", quietly = TRUE)) {
  stop("blockr.dplyr package must be installed and loaded")
}

if (!requireNamespace("blockr.core", quietly = TRUE)) {
  stop("blockr.core package must be installed")
}

library(webshot2)
library(blockr.dplyr)
library(blockr.core)

# Increase timeout for Shiny app launching
options(webshot.app.timeout = 120)

# Configuration
SCREENSHOT_WIDTH <- 1200
SCREENSHOT_HEIGHT <- 800
OUTPUT_DIR <- "man/figures"

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Default to select if not specified
if (!exists("block_type")) {
  block_type <- "select"
}

cat(sprintf("Generating screenshot for: %s\n", block_type))

# Helper function to create temporary app and take screenshot
create_screenshot <- function(block, filename, data = list(data = mtcars)) {
  cat(sprintf("Generating %s...\n", filename))
  
  tryCatch({
    # Create temporary directory for the app
    temp_dir <- tempfile("blockr_app")
    dir.create(temp_dir)
    
    # Save data to RDS file
    saveRDS(data, file.path(temp_dir, "data.rds"))
    
    # Create minimal app.R file
    app_content <- sprintf('
library(blockr.dplyr)
library(blockr.core)

# Load data
data <- readRDS("data.rds")

# Run the app
blockr.core::serve(
  %s,
  data = data
)
    ', deparse(substitute(block), width.cutoff = 500))
    
    writeLines(app_content, file.path(temp_dir, "app.R"))
    
    # Take screenshot
    webshot2::appshot(
      app = temp_dir,
      file = file.path(OUTPUT_DIR, filename),
      vwidth = SCREENSHOT_WIDTH,
      vheight = SCREENSHOT_HEIGHT,
      delay = 5  # Wait for app to load
    )
    
    # Cleanup
    unlink(temp_dir, recursive = TRUE)
    
    cat(sprintf("✓ %s created\n", filename))
    
  }, error = function(e) {
    cat(sprintf("✗ Failed to create %s: %s\n", filename, e$message))
  })
}

# Generate screenshot based on block_type
switch(block_type,
  "select" = create_screenshot(
    new_select_block(
      columns = c("mpg", "cyl", "disp", "hp"),
      interface = "table"
    ),
    "select-table.png"
  ),
  
  "select-cards" = create_screenshot(
    new_select_block(
      columns = c("mpg", "cyl", "disp"),
      interface = "cards"
    ),
    "select-cards.png"
  ),
  
  "filter" = create_screenshot(
    new_filter_block(
      multi_condition = TRUE
    ),
    "filter-multi.png"
  ),
  
  "mutate" = create_screenshot(
    new_mutate_block(
      exprs = list(
        list(name = "mpg_per_cyl", value = "mpg / cyl"),
        list(name = "hp_cat", value = "ifelse(hp > 150, 'high', 'low')")
      )
    ),
    "mutate-multi.png"
  ),
  
  "arrange" = create_screenshot(
    new_arrange_block(
      columns = list(
        list(column = "cyl", desc = FALSE),
        list(column = "mpg", desc = TRUE)
      )
    ),
    "arrange-multi.png"
  ),
  
  "summarize" = create_screenshot(
    new_summarize_block(
      .by = c("cyl"),
      exprs = list(
        list(name = "mean_mpg", value = "mean(mpg)"),
        list(name = "sd_mpg", value = "sd(mpg)")
      )
    ),
    "summarize-grouped.png"
  ),
  
  "rename" = create_screenshot(
    new_rename_block(
      columns = list(
        list(old_column = "mpg", new_column = "miles_per_gallon"),
        list(old_column = "cyl", new_column = "cylinders")
      )
    ),
    "rename-multi.png"
  ),
  
  "distinct" = create_screenshot(
    new_distinct_block(
      columns = c("cyl", "gear")
    ),
    "distinct.png"
  ),
  
  "slice" = create_screenshot(
    new_slice_block(
      type = "head",
      n = 10
    ),
    "slice-head.png"
  ),
  
  stop(sprintf("Unknown block_type: %s. Valid options: select, select-cards, filter, mutate, arrange, summarize, rename, distinct, slice", block_type))
)

cat(sprintf("Screenshot generation complete for %s!\n", block_type))
cat(sprintf("Screenshot saved to: %s/\n", OUTPUT_DIR))