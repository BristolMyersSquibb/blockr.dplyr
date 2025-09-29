library(blockr.core)
library(blockr.dplyr)

# Simple test to check column switching
app <- blockr.core::serve(
  new_enhanced_filter_block("Petal.Length > 5"),
  data = list(data = iris)
)

# Run the app
shiny::runApp(app, port = 5467, launch.browser = FALSE)
