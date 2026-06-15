#' Factory for JS-driven transform blocks
#'
#' All blockr.dplyr blocks share one skeleton: a JS class owns the UI, sends
#' a JSON `state` through a Shiny input binding, and R turns that state into
#' a dplyr/tidyr expression. These helpers generate the skeleton so each
#' block only supplies its name, expression builder, and any extras.
#'
#' Naming convention derived from `name` (e.g. `"pivot-longer"`):
#' - input id: `pivot_longer_input`
#' - container class: `pivot-longer-block-container`
#' - messages: `pivot-longer-columns`, `pivot-longer-block-update`
#' - assets: `inst/js/pivot-longer-block.js`, `inst/css/pivot-longer-block.css`
#'
#' The JS side mirrors this via `Blockr.registerBlock()` in blockr-core.js.
#'
#' @name js-block
#' @keywords internal
NULL

#' Build a JS-driven single-input transform block
#'
#' @param class Block S3 class, e.g. `"filter_block"`.
#' @param name Kebab-case block name driving the naming convention.
#' @param state Initial state list, assembled by the calling constructor from
#'   its flat arguments (e.g. `list(conditions = conditions, operator =
#'   operator)`). Its `names()` define the fields that are serialized and that
#'   must match the constructor's formals for save/restore to round-trip.
#' @param expr_fn Function of the state list returning a `bquoted`
#'   language object.
#' @param columns_meta Function of the data frame returning the column
#'   metadata pushed to JS on every data change, or `NULL` to skip.
#' @param setup Optional `function(input, session, ns, data, input_name)`
#'   registering block-specific observers (e.g. filter's lazy value
#'   requests, summarize's function list).
#' @param normalize_state Applied to the state before sending it to JS via
#'   the update message (e.g. force length-1 vectors to JSON arrays).
#' @param shared_deps Character subset of `c("select", "input")`.
#' @param ctor,ctor_pkg Forwarded to [blockr.core::new_transform_block()];
#'   the default records the calling constructor for serialization.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @noRd
new_js_transform_block <- function(class,
                                   name,
                                   state,
                                   expr_fn,
                                   columns_meta = build_column_picker_meta,
                                   setup = NULL,
                                   normalize_state = identity,
                                   shared_deps = "select",
                                   ctor = sys.parent(),
                                   ctor_pkg = NULL,
                                   ...) {
  input_name <- js_block_input_name(name)

  # blockr.core's `initial_block_state()` reads each constructor formal by
  # name from the (expr-)server's enclosing environment. Our flat formals
  # (`conditions`, `operator`, ...) live in the calling constructor's frame,
  # not here -- bind them locally so the static state path resolves them.
  # `names(state)` equals the constructor's flat formals by construction.
  list2env(state, environment())

  new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        if (!is.null(columns_meta)) {
          observeEvent(data(), {
            session$sendCustomMessage(
              paste0(name, "-columns"),
              list(id = ns(input_name), columns = columns_meta(data()))
            )
          })
        }

        if (!is.null(setup)) {
          setup(input, session, ns, data, input_name)
        }

        sync <- js_block_state(input, session, name, input_name, state,
                               normalize_state)

        list(
          expr = reactive(expr_fn(sync$state())),
          state = sync$fields
        )
      })
    },
    js_block_ui(name, shared_deps),
    class = class,
    ctor = ctor,
    ctor_pkg = ctor_pkg,
    expr_type = "bquoted",
    external_ctrl = TRUE,
    # All fields may legitimately be empty on a fresh block (the expr
    # builders self-heal via `%||%`); revisit per block if any field should
    # instead gate output until set.
    allow_empty_state = TRUE,
    ...
  )
}

#' Input id derived from the block name
#' @noRd
js_block_input_name <- function(name) {
  paste0(gsub("-", "_", name, fixed = TRUE), "_input")
}

#' Wire the bidirectional JS <-> R state sync over per-field reactiveVals
#'
#' The JS side maintains state as a single blob, but R holds it as one
#' `reactiveVal` per field (`names(state)`). This is what lets external
#' controllers (e.g. blockr.ai, board restore) set individual fields such as
#' `conditions` or `operator` directly -- blockr.core requires every
#' externally controllable variable to be a `reactiveVal`, and serializes the
#' named fields flat so they round-trip onto the constructor's flat arguments.
#'
#' - JS -> R: the input binding (one blob) is decomposed into the per-field
#'   reactiveVals.
#' - R -> JS: any field change (JS edit, restore, or external control) pushes
#'   the recombined blob back via the `<name>-block-update` message. The
#'   `self_write` guard breaks the R -> JS -> R loop. The observer fires on
#'   init (no `ignoreInit`): that first message delivers the constructor state
#'   to the JS class, queued by `Blockr.registerBlock()` until the element
#'   binds.
#'
#' @return A list with `fields` (named list of per-field reactiveVals, to be
#'   returned as the block's `state`) and `state` (a reactive recombining them
#'   into the blob, for `expr_fn` and the JS push).
#'
#' Reused by the bespoke servers (join, bind_rows) whose data arity doesn't
#' fit [new_js_transform_block()].
#' @noRd
js_block_state <- function(input, session, name, input_name, state,
                           normalize_state = identity) {
  fields <- names(state)

  r_fields <- setNames(
    lapply(fields, function(f) reactiveVal(state[[f]])),
    fields
  )
  r_state <- reactive(
    setNames(lapply(fields, function(f) r_fields[[f]]()), fields)
  )

  self_write <- new.env(parent = emptyenv())
  self_write$active <- FALSE

  # JS -> R: decompose the single blob into the per-field reactiveVals.
  observeEvent(input[[input_name]], {
    self_write$active <- TRUE
    blob <- input[[input_name]]
    for (f in fields) r_fields[[f]](blob[[f]])
  })

  # R -> JS: push the recombined blob whenever any field changes.
  observeEvent(r_state(), {
    if (self_write$active) {
      self_write$active <- FALSE
    } else {
      session$sendCustomMessage(
        paste0(name, "-block-update"),
        list(
          id = session$ns(input_name),
          state = normalize_state(r_state())
        )
      )
    }
  })

  list(fields = r_fields, state = r_state)
}

#' Standard block UI: dependencies + the container div the JS binds to
#' @noRd
js_block_ui <- function(name, shared_deps = "select") {
  force(name)
  force(shared_deps)
  function(id) {
    tagList(
      blockr_core_js_dep(),
      blockr_blocks_css_dep(),
      if ("select" %in% shared_deps) blockr_select_dep(),
      if ("input" %in% shared_deps) blockr_input_dep(),
      js_block_dep(name),
      div(
        class = "block-container",
        div(
          id = NS(id, js_block_input_name(name)),
          class = paste0(name, "-block-container")
        )
      )
    )
  }
}

#' HTML dependency for a block's JS + CSS pair
#' @noRd
js_block_dep <- function(name) {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = paste0(name, "-block-js"),
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = paste0(name, "-block.js")
    ),
    htmltools::htmlDependency(
      name = paste0(name, "-block-css"),
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = paste0(name, "-block.css")
    )
  )
}
