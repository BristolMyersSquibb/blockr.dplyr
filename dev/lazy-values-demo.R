# Run from the monorepo root (/workspace): Rscript blockr.dplyr/dev/lazy-values-demo.R
# Demo: filter-block values load lazily, on dropdown-open (fix/filter-lazy-value-open).
#
# What to try in the browser:
# - The board is interactive almost immediately: startup ships only the 424B
#   column summary, never a value list (pre-fix it shipped the auto-selected
#   `id` column's 50K unique values, ~289KB, before the dock finished restoring).
# - "filter id" has the 50K-unique `id` column selected: open its values
#   dropdown — a brief "Loading…" then all 50K options stream in on demand.
# - "filter saved" was restored with `category is A|B`: the chips and the
#   filtered result (~3.8K rows) are there from state alone; its value list
#   also only loads if you open the dropdown.
# - "filter group" is the low-cardinality comparison: open its dropdown and
#   the 100 group values pop in with no perceptible wait — same lazy path,
#   payload just doesn't matter at this size.
# - In the id dropdown, only the first 200 matches get DOM nodes
#   ("+49,xxx more — type to narrow"); search covers the full list.

pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.dock", quiet = TRUE)
pkgload::load_all("blockr.dag", quiet = TRUE)
pkgload::load_all("blockr.dplyr", quiet = TRUE)

set.seed(42)
n <- 50000L
big_df <- data.frame(
  id       = seq_len(n),
  category = sample(LETTERS, n, replace = TRUE),
  group    = sample(paste0("Group_", 1:100), n, replace = TRUE),
  value    = rnorm(n),
  score    = sample(1:1000, n, replace = TRUE),
  label    = sample(paste0("item_", 1:5000), n, replace = TRUE),
  stringsAsFactors = FALSE
)
big_df$value[sample(n, 500)] <- NA

app <- serve(
  new_dock_board(
    extensions = list(dag = new_dag_extension()),
    blocks = c(
      data = new_static_block(big_df),
      filter_id = new_filter_block(),
      filter_saved = new_filter_block(
        state = list(
          conditions = list(
            list(type = "values", column = "category",
                 values = list("A", "B"), mode = "include")
          ),
          operator = "&"
        )
      ),
      filter_group = new_filter_block(
        state = list(
          conditions = list(
            list(type = "values", column = "group",
                 values = list("Group_1"), mode = "include")
          ),
          operator = "&"
        )
      )
    ),
    links = c(
      new_link("data", "filter_id", "data"),
      new_link("data", "filter_saved", "data"),
      new_link("data", "filter_group", "data")
    )
  )
)

shiny::runApp(app, port = 3838, host = "0.0.0.0", launch.browser = FALSE)
