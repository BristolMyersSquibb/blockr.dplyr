# A list-OF-records state field written FLAT -- list(name = "m", func = "mean")
# instead of list(list(name = "m", func = "mean")) -- used to be accepted by
# every constructor and then fail twice: `expr_fn` threw "$ operator is invalid
# for atomic vectors", and the state reached the client as a JSON object, where
# `for (const s of summaries)` threw "summaries is not iterable". The block
# rendered nothing and nothing upstream could tell: `get_block_result()`
# returns a block's pass-through input, not its render.
#
# Humans copying a worked example get the nesting right. A block built from a
# schema -- blockr.assistant -- does not, reliably, which is how this surfaced.
#
# Add a row here when a block gains a list-of-records field, and declare that
# field in `record_fields` on its `new_js_transform_block()` call.

record_list_blocks <- list(
  list(
    label  = "summarize",
    ctor   = function(v) new_summarize_block(summaries = v),
    field  = "summaries",
    ready  = "summarize_input_ready",
    record = list(name = "m", func = "mean", col = "mpg")
  ),
  list(
    label  = "mutate",
    ctor   = function(v) new_mutate_block(mutations = v),
    field  = "mutations",
    ready  = "mutate_input_ready",
    record = list(name = "kpl", expr = "mpg * 0.425144")
  ),
  list(
    label  = "filter",
    ctor   = function(v) new_filter_block(conditions = v),
    field  = "conditions",
    ready  = "filter_input_ready",
    record = list(column = "mpg", op = ">", values = list("20"))
  ),
  list(
    label  = "arrange",
    ctor   = function(v) new_arrange_block(columns = v),
    field  = "columns",
    ready  = "arrange_input_ready",
    record = list(column = "mpg", desc = FALSE)
  )
)

# The state as the client actually receives it: the "<name>-block-update"
# custom message, serialised the way Shiny serialises it.
sent_state <- function(blk, ready, field) {
  out <- NULL

  testServer(blk$expr_server, args = list(data = shiny::reactive(mtcars)), {
    session$flushReact()

    msgs <- list()
    # Bind the root MockShinySession first -- assigning into
    # `session$rootScope()$...` errors on the proxy (see test-ready-handshake.R).
    root <- session$rootScope()
    root$sendCustomMessage <- function(type, message) {
      msgs[[length(msgs) + 1L]] <<- list(type = type, message = message)
      invisible(NULL)
    }

    do.call(session$setInputs, stats::setNames(list(1), ready))
    session$flushReact()

    hit <- which(vapply(msgs, `[[`, character(1L), "type") == paste0(
      sub("_input_ready$", "", ready), "-block-update"
    ))
    if (length(hit)) out <<- msgs[[hit[[1L]]]]$message$state[[field]]
  })

  out
}

as_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

for (spec in record_list_blocks) {
  local({
    s <- spec

    test_that(paste(s$label, "sends a JSON array for a nested record list"), {
      state <- sent_state(s$ctor(list(s$record)), s$ready, s$field)
      expect_false(is.null(state))
      expect_match(as_json(state), "^\\[", info = s$label)
    })

    # The regression: one record written flat.
    test_that(paste(s$label, "sends a JSON array for a FLAT record"), {
      state <- sent_state(s$ctor(s$record), s$ready, s$field)
      expect_false(is.null(state))
      expect_match(as_json(state), "^\\[", info = s$label)
    })

    # A named list of records is the other shape that serialises to an object.
    test_that(paste(s$label, "sends a JSON array for a NAMED record list"), {
      state <- sent_state(s$ctor(list(a = s$record)), s$ready, s$field)
      expect_false(is.null(state))
      expect_match(as_json(state), "^\\[", info = s$label)
    })

    # A flat record used to throw here before it ever reached the client.
    test_that(paste(s$label, "builds an expression from a FLAT record"), {
      blk <- s$ctor(s$record)
      testServer(blk$expr_server, args = list(data = shiny::reactive(mtcars)), {
        session$flushReact()
        expect_no_error(shiny::isolate(session$returned$expr()))
      })
    })
  })
}

test_that("as_record_list leaves a canonical list alone", {
  x <- list(list(name = "a"), list(name = "b"))
  expect_identical(as_record_list(x), x)
  expect_identical(as_record_list(list()), list())
  expect_identical(as_record_list(NULL), NULL)
  # a plain character vector is not a record list
  expect_identical(as_record_list(c("a", "b")), c("a", "b"))
})
