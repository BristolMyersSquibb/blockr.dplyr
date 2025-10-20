#!/usr/bin/env Rscript

# Validate that screenshots match registry
#
# This script checks that:
# 1. All registered blockr.dplyr blocks have screenshots
# 2. No extra PNG files exist in man/figures/
#
# To run: source("inst/screenshots/validate_registry.R")

library(blockr.dplyr)

cat("Validating screenshots against registry...\n\n")

# =============================================================================
# Get registered blocks from package registry
# =============================================================================

registry <- blockr.core::available_blocks()

# Extract constructor names and packages from attributes
registered_constructors <- sapply(registry, function(x) attr(x, "ctor_name"))
registered_packages <- sapply(registry, function(x) attr(x, "package"))

# Filter for blockr.dplyr blocks only
dplyr_constructors <- registered_constructors[
  registered_packages == "blockr.dplyr"
]

# Convert constructor names to expected filenames
# e.g., "new_filter_expr_block" -> "filter-expr-block.png"
# e.g., "new_bind_cols_block" -> "bind-cols-block.png"
expected_files <- sub("^new_", "", dplyr_constructors) # Remove "new_" prefix
expected_files <- gsub("_", "-", expected_files) # Convert all underscores to hyphens
expected_files <- paste0(expected_files, ".png") # Add .png extension

# Get all PNG files in man/figures/
actual_files <- list.files(
  "man/figures",
  pattern = "\\.png$",
  full.names = FALSE
)

# =============================================================================
# Check for missing screenshots
# =============================================================================

missing_files <- setdiff(expected_files, actual_files)
if (length(missing_files) > 0) {
  cat("⚠️  WARNING: Missing screenshots:\n")
  cat(paste("  -", missing_files), sep = "\n")
  cat("\n")
}

# =============================================================================
# Check for extra screenshots (not in registry)
# =============================================================================

extra_files <- setdiff(actual_files, expected_files)
if (length(extra_files) > 0) {
  cat("⚠️  WARNING: Extra screenshot files found (not in registry):\n")
  cat(paste("  -", extra_files), sep = "\n")
  cat("\nConsider removing these files:\n")
  for (f in extra_files) {
    cat(sprintf("  rm man/figures/%s\n", f))
  }
  cat("\n")
}

# =============================================================================
# Success message
# =============================================================================

if (length(missing_files) == 0 && length(extra_files) == 0) {
  cat(
    "✓ Perfect! All",
    length(dplyr_constructors),
    "registered blocks have screenshots\n"
  )
  cat("✓ No extra files in man/figures/\n")
} else {
  cat("Summary:\n")
  cat(sprintf("  Registered blocks: %d\n", length(dplyr_constructors)))
  cat(sprintf("  Expected files: %d\n", length(expected_files)))
  cat(sprintf("  Found files: %d\n", length(actual_files)))
  cat(sprintf("  Missing: %d\n", length(missing_files)))
  cat(sprintf("  Extra: %d\n", length(extra_files)))
}
