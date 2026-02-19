library(blockr.core)
library(blockr.dplyr)

# Minimal test: simulate AI external write to filter conditions.
# The "Simulate AI" button writes conditions externally, just like
# blockr.ai's ai_ctrl_block would.

sim_ctrl <- function() {
  ctrl_block(
    server = function(id, x, vars, data, eval) {
      moduleServer(id, function(input, output, session) {
        observeEvent(input$sim, {
          message("[SIM] Writing conditions: Species = setosa")
          vars[["conditions"]](list(
            list(column = "Species", values = "setosa", mode = "include")
          ))
          message("[SIM] Done writing conditions")
        })
        reactive(TRUE)
      })
    },
    ui = function(id, x) {
      ns <- NS(id)
      actionButton(ns("sim"), "Simulate AI: Species = setosa",
                   class = "btn btn-primary btn-sm mt-2")
    }
  )
}

serve(
  new_board(
    blocks = list(
      a = new_dataset_block("iris"),
      b = new_filter_block()
    ),
    links = links(from = "a", to = "b")
  ),
  plugins = custom_plugins(sim_ctrl())
)
