# Demo script for the new summarize no-code block
library(blockr.core)
# library(blockr.dplyr)


pkgload::load_all()

# Basic usage with mtcars
# serve(new_scatter_block(), data = list(data = mtcars))

# works!
# serve(
#   new_dock_board(
#     blocks = c(
#       a = new_dataset_block("iris"),
#       b = new_summarize_block()
#     ),
#     links = list(from = "a", to = "b", input = "data"),
#     extensions = new_dag_extension()
#   )
# )
#
#

# Add custom functions from blockr.topline or any other package
options(
  blockr.dplyr.summary_functions = c(
    "extract parentheses (paren_num)" = "blockr.topline::paren_num",
    "first number (first_num)" = "blockr.topline::first_num"
  )
)
# works too!!
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_summarize_nocode_block(by = "Species")
    ),
    links = list(from = "a", to = "b", input = "data")
  )
)


# serve(new_summarize_block(
#   exprs = list(count = "dplyr::n()"),
#   by = c("cyl")
# ), data = list(data = mtcars))


# With predefined summaries
# serve(
#   new_summarize_nocode_block(
#     summaries = list(
#       avg_mpg = list(func = "mean", col = "mpg"),
#       max_hp = list(func = "max", col = "hp"),
#       count = list(func = "dplyr::n", col = "")
#     )
#   ),
#   data = list(data = mtcars)
# )

# library(blockr.dag)
# library(blockr.dock)
# library(blockr.core)

# serve(
#   new_dock_board(
#     blocks = c(
#       a = new_dataset_block("iris"),
#       b = new_summarize_block()
#     ),
#     links = list(from = "a", to = "b", input = "data"),
#     extensions = new_dag_extension()
#   )
# )


# With predefined summaries
# serve(
#   new_summarize_nocode_block(
#     summaries = list(
#       avg_mpg = list(func = "mean", col = "mpg"),
#       max_hp = list(func = "max", col = "hp"),
#       count = list(func = "dplyr::n", col = "")
#     )
#   ),
#   list(data = mtcars)
# )

# With grouping
# serve(
#   new_summarize_nocode_block(
#     summaries = list(
#       avg_mpg = list(func = "mean", col = "mpg"),
#       total_count = list(func = "dplyr::n", col = "")
#     ),
#     by = "cyl"
#   ),
#   list(data = mtcars)
# )
