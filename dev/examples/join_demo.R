# Join block demo
pkgload::load_all()
pkgload::load_all("../blockr.dock")
library(blockr)

run_app(
  blocks = c(
    bod_data1 = new_dataset_block(dataset = "BOD"),
    bod_data2 = new_dataset_block(dataset = "BOD"),
    join_result = new_join_block(type = "left_join")
  ),
  links = c(
    new_link("bod_data1", "join_result", "x"),
    new_link("bod_data2", "join_result", "y")
  )
)
