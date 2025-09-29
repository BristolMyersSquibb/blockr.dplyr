# Minimal Shiny app for testing enhanced filter block
# This app is used by shinytest2 to test column switching behavior

library(blockr.core)
library(blockr.dplyr)

# Since blockr.core::serve() returns a shiny app object,
# we can use it directly for shinytest2
blockr.core::serve(
  new_enhanced_filter_block("Petal.Length > 5"),
  data = list(data = iris)
)