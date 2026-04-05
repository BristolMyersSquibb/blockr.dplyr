# AI Discovery Test — All Blocks
#
# Runs discover_block_args() with gpt-4o-mini against every blockr.dplyr block.
# Verifies: registry metadata → LLM prompt → JSON generation → block evaluation.
#
# Requires: OPENAI_API_KEY environment variable
#
# Usage:
#   cd blockr.dplyr && Rscript dev/test-ai-discovery.R

pkgload::load_all("/workspace/blockr.dplyr")
library(blockr.ai)

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("OPENAI_API_KEY not set. Export it before running this script.")
}

# --- Test data for reshape/string blocks ---

df_wide <- data.frame(
  id = 1:3,
  a = c(10, 20, 30),
  b = c(40, 50, 60)
)

df_long <- data.frame(
  id = c(1L, 1L, 2L, 2L),
  var = c("a", "b", "a", "b"),
  val = c(10, 40, 20, 50)
)

df_names <- data.frame(
  first = c("John", "Jane"),
  last = c("Doe", "Smith")
)

df_full <- data.frame(
  full_name = c("John Doe", "Jane Smith"),
  age = c(30, 25)
)

# --- Block tests ---
# Each entry: name, block, data, prompt
# Skip join/bind_rows/bind_cols — standalone validator doesn't support arity > 1

tests <- list(
  list(
    name = "filter",
    block = new_filter_block(),
    data = iris,
    prompt = "show only setosa species"
  ),
  list(
    name = "select",
    block = new_select_block(),
    data = mtcars,
    prompt = "keep only the mpg and cyl columns"
  ),
  list(
    name = "arrange",
    block = new_arrange_block(),
    data = mtcars,
    prompt = "sort by mpg descending"
  ),
  list(
    name = "rename",
    block = new_rename_block(),
    data = mtcars,
    prompt = "rename mpg to miles_per_gallon"
  ),
  list(
    name = "mutate",
    block = new_mutate_block(),
    data = mtcars,
    prompt = "add a column called hp_per_cyl that is hp divided by cyl"
  ),
  list(
    name = "summarize",
    block = new_summarize_block(),
    data = mtcars,
    prompt = "calculate average mpg grouped by cyl"
  ),
  list(
    name = "slice",
    block = new_slice_block(),
    data = mtcars,
    prompt = "first 10 rows"
  ),
  list(
    name = "pivot_longer",
    block = new_pivot_longer_block(),
    data = df_wide,
    prompt = "pivot columns a and b into long format with name column called variable and value column called measurement"
  ),
  list(
    name = "pivot_wider",
    block = new_pivot_wider_block(),
    data = df_long,
    prompt = "pivot the var column to wide format using val as values"
  ),
  list(
    name = "unite",
    block = new_unite_block(),
    data = df_names,
    prompt = "combine first and last into a column called full_name separated by a space"
  ),
  list(
    name = "separate",
    block = new_separate_block(),
    data = df_full,
    prompt = "split full_name into first_name and last_name by space"
  )
)

# --- Run tests ---

cat("\n========================================\n")
cat("blockr.dplyr AI Discovery Test\n")
cat("========================================\n\n")

pass <- 0
fail <- 0

for (t in tests) {
  cat(sprintf("%-16s ", t$name))

  result <- tryCatch(
    discover_block_args(
      prompt = t$prompt,
      block = t$block,
      data = t$data,
      verbose = FALSE
    ),
    error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    }
  )

  if (isTRUE(result$success)) {
    rows <- if (is.data.frame(result$result)) nrow(result$result) else "?"
    cat(sprintf("PASS  (%s rows)\n", rows))
    pass <- pass + 1
  } else {
    cat(sprintf("FAIL  (%s)\n", result$error %||% "unknown error"))
    fail <- fail + 1
  }
}

cat(sprintf("\n--- %d passed, %d failed ---\n", pass, fail))

if (fail > 0) {
  cat("\nNote: join, bind_rows, bind_cols are not tested here\n")
  cat("(standalone validator doesn't support arity > 1).\n")
  cat("Test those interactively with test-ai-basic.R or test-ai-expr.R.\n")
}
