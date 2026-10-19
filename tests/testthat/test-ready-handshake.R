# A block on a dock panel that is not on the startup view has its server run
# at boot but its JavaScript delivered later, with the panel. The boot pushes
# are dropped (Shiny discards custom messages with no handler), so the client
# announces itself on bind and R re-sends. See blockr.core#317.

# Capture custom messages. Assigning into `session$rootScope()$...` errors on
# the proxy session, so the root MockShinySession has to be bound first.
capture_messages <- function(session) {
  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()
  root <- session$rootScope()
  root$sendCustomMessage <- function(type, message) {
    sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
    invisible(NULL)
  }
  sent
}

types_of <- function(sent) vapply(sent$msgs, `[[`, character(1L), "type")

test_that("a client announcing itself gets the mutate state re-sent", {
  blk <- new_mutate_block(
    mutations = list(list(name = "kpl", expr = "mpg * 0.425144")),
    by = list("cyl")
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    sent <- capture_messages(session)

    session$setInputs(mutate_input_ready = 1)
    session$flushReact()

    expect_setequal(types_of(sent), c("mutate-block-update", "mutate-columns"))

    state <- sent$msgs[[which(types_of(sent) == "mutate-block-update")]]$message
    expect_equal(state$state$mutations[[1L]]$name, "kpl")
    expect_equal(state$state$by, list("cyl"))

    cols <- sent$msgs[[which(types_of(sent) == "mutate-columns")]]$message
    expect_true(length(cols$columns) > 0L)
  })
})

test_that("the announce re-sends summarize's state, columns and functions", {
  blk <- new_summarize_block(
    summaries = list(
      list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg")
    ),
    by = list("cyl")
  )

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    sent <- capture_messages(session)

    session$setInputs(summarize_input_ready = 1)
    session$flushReact()

    expect_setequal(
      types_of(sent),
      c("summarize-block-update", "summarize-columns", "summarize-functions")
    )

    state <- sent$msgs[[
      which(types_of(sent) == "summarize-block-update")
    ]]$message
    expect_equal(state$state$summaries[[1L]]$name, "avg_mpg")
  })
})

test_that("ready gets the state while the upstream is still unset", {
  blk <- new_filter_block(
    conditions = list(list(type = "expr", expr = "mpg > 20"))
  )

  testServer(
    blk$expr_server,
    args = list(data = reactive(req(FALSE))),
    {
      session$flushReact()
      sent <- list()
      root <- session$rootScope()
      root$sendInputMessage <- function(inputId, message) {
        sent[[length(sent) + 1L]] <<- message
        invisible(NULL)
      }

      session$setInputs(block = list(messages = list(list(type = "ready"))))
      session$flushReact()

      # No columns yet, but the state gets through: the block is mounted
      # with an input that has no data.
      expect_length(sent, 1L)
      expect_equal(sent[[1L]]$state$pred[[1L]]$code, "mpg > 20")
      expect_null(sent[[1L]]$context$inputs$data)
    }
  )
})
