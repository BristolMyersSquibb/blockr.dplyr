# Filter benchmark: blockr.dplyr GitHub API (dev)
library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.dplyr)


# 50k-row dataset
set.seed(1L)
big_df <- data.frame(
  id = seq_len(50000),
  value = rnorm(50000),
  category = sample(c("A", "B", "C", "D"), 50000, replace = TRUE),
  stringsAsFactors = FALSE
)

serve(
  new_dock_board(
    extensions = new_dag_extension(),
    layout = list(
      list("ext_panel-dag_extension")
    ),
    blocks = list(
      data = new_static_block(big_df),
      filtered = new_filter_block(
        state = list(
          conditions = list(
            list(
              type = "values",
              column = "category",
              values = list("A", "B"),
              mode = "include"
            )
          ),
          operator = "&"
        )
      )
    ),
    links = list(
      new_link("data", "filtered", "data")
    )
  )
)
