library(blockr.core)
library(blockr.dplyr)
library(blockr.ai)

serve(
  new_board(
    blocks = list(
      a = new_dataset_block("iris"),
      b = new_filter_block()
    ),
    links = links(from = "a", to = "b")
  ),
  plugins = custom_plugins(ai_ctrl_block())
)
