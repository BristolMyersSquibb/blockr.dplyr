# Screenshot Generation for blockr.dplyr

This directory contains tools for generating screenshots of all blockr.dplyr blocks for use in pkgdown documentation.

## Files

- `validate-screenshot.R`: Functions for generating screenshots of individual blocks or batches
- `generate_all.R`: Script that generates screenshots for all blocks at once
- `screenshot-results.csv`: Results from the last screenshot generation run (auto-generated)

## Usage

### Generate all screenshots

From the root of the blockr.dplyr package:

```r
source("inst/screenshots/generate_all.R")
```

This will:
- Generate screenshots for all blocks with example configurations
- Save them to `man/figures/` directory
- Create a results CSV showing which blocks succeeded/failed

### Generate a screenshot for a single block

```r
source("inst/screenshots/validate-screenshot.R")

# Example: Generate screenshot for filter block
result <- validate_block_screenshot(
  block = new_filter_block(exprs = "mpg > 20"),
  data = mtcars,
  filename = "my-filter-block.png",
  output_dir = "man/figures"
)

if (result$success) {
  cat("Screenshot saved to:", result$path, "\n")
} else {
  cat("Failed:", result$error, "\n")
}
```

## Adding a new block

To add a new block to the screenshot generation:

1. Open `generate_all.R`
2. Add your block to the `blocks` list with a descriptive name
3. Configure it with interesting startup values
4. Run the script

Example:

```r
blocks <- list(
  # ... existing blocks ...

  # My new block (simple)
  "my-new-block" = new_my_block(param1 = "value1", param2 = 42),

  # Block with advanced options expanded
  "my-advanced-block" = list(
    block = new_my_block(param1 = "value1"),
    expand_advanced = TRUE  # This will click "Show advanced options" before screenshot
  )
)
```

## Advanced Options

Some blocks (like `summarize_block`) have collapsible "advanced options" sections. To capture screenshots with these options expanded:

```r
"summarize-block" = list(
  block = new_summarize_block(...),
  expand_advanced = TRUE
)
```

The screenshot system will automatically:
1. Launch the Shiny app
2. Wait for it to load
3. Click the advanced options toggle
4. Wait for the animation to complete
5. Take the screenshot

## Requirements

- `shinytest2` package: `install.packages("shinytest2")`
- `blockr.core` package
- `blockr.dplyr` package (loaded from development if available)

## Output

Screenshots are saved to `man/figures/` with the following naming pattern:
- `filter-block.png`
- `select-block.png`
- `arrange-block.png`
- etc.

These can be referenced in pkgdown documentation and README files.

## Validation

The script automatically validates that:
- All 11 registered blocks have screenshots
- No extra PNG files exist in `man/figures/`

If extra files are found, the script will provide `rm` commands to clean them up.
