# Load required libraries
library(blockr.core)
library(blockr.ui)
library(blockr.dplyr)
pkgload::load_all()

# Demo workflow for join block - simple two dataset example
blockr.core::serve(
  blockr.ui::new_dag_board(
    blocks = c(
      # Create first dataset: BOD data
      bod_data1 = new_dataset_block(dataset = "BOD"),

      # Create second dataset: BOD data again
      bod_data2 = new_dataset_block(dataset = "BOD"),

      # Join the two datasets - explicit join on Time column
      join_result = new_join_block(
        type = "left_join"
      )
    ),
    links = c(
      # Connect the join
      new_link("bod_data1", "join_result", "x"),
      new_link("bod_data2", "join_result", "y")
    )
  )
)
