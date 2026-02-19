# Test App 3: Reshape & String Ops
#
# Each block gets its own dataset to allow independent testing.
#   dataset(iris)         -> pivot_longer
#   dataset(people)       -> unite
#   dataset(people_united)-> separate
#   dataset(long)         -> pivot_wider
#
# Usage:
#   Rscript dev/test-ai-reshape.R
#   (launches on port 7654)

pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.dock")
pkgload::load_all("/workspace/blockr.dag")
pkgload::load_all("/workspace/blockr.dplyr")
pkgload::load_all("/workspace/blockr.ai")

# --- static datasets --------------------------------------------------------

people <- data.frame(
  name  = c("Alice", "Bob", "Charlie"),
  value = c("100", "200", "300"),
  group = c("A", "B", "A"),
  stringsAsFactors = FALSE
)

people_united <- data.frame(
  combined = c("part1:part2", "alpha:beta", "foo:bar"),
  id       = 1:3,
  stringsAsFactors = FALSE
)

long <- data.frame(
  id    = c(1L, 1L, 2L, 2L, 3L, 3L),
  part1 = rep(c("a", "b"), 3),
  part2 = c(10, 15, 20, 25, 30, 35),
  stringsAsFactors = FALSE
)

# --- board ------------------------------------------------------------------

app <- serve(
  new_dock_board(
    blocks = c(
      # --- pivot_longer ---
      iris_data      = new_dataset_block("iris"),
      pivot_longer   = new_pivot_longer_block(),

      # --- unite ---
      people_data    = new_static_block(people),
      unite          = new_unite_block(),

      # --- separate ---
      united_data    = new_static_block(people_united),
      separate       = new_separate_block(),

      # --- pivot_wider ---
      long_data      = new_static_block(long),
      pivot_wider    = new_pivot_wider_block()
    ),
    links = c(
      new_link("iris_data",   "pivot_longer", "data"),
      new_link("people_data", "unite",        "data"),
      new_link("united_data", "separate",     "data"),
      new_link("long_data",   "pivot_wider",  "data")
    ),
    extensions = list(dag = new_dag_extension())
  ),
  plugins = custom_plugins(ai_ctrl_block())
)

shiny::runApp(app, port = 7654L, host = "0.0.0.0", launch.browser = FALSE)
