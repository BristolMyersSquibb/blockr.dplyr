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
#' @param state Initial state list (the constructor's `state` argument —
#'   must be forwarded unchanged so save/restore round-trips).
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

  new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- reactiveVal(state)

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

        js_block_sync(input, session, name, input_name, r_state,
                      normalize_state)

        list(
          expr = reactive(expr_fn(r_state())),
          state = list(state = r_state)
        )
      })
    },
    js_block_ui(name, shared_deps),
    class = class,
    ctor = ctor,
    ctor_pkg = ctor_pkg,
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' Input id derived from the block name
#' @noRd
js_block_input_name <- function(name) {
  paste0(gsub("-", "_", name, fixed = TRUE), "_input")
}

#' Wire the bidirectional JS <-> R state sync
#'
#' JS -> R: the input binding writes into `r_state`. R -> JS: external
#' state changes (board restore, programmatic control) are pushed via the
#' `<name>-block-update` message. The `self_write` guard breaks the
#' R -> JS -> R loop. The R -> JS observer deliberately fires on init
#' (no `ignoreInit`): that first message delivers the constructor state to
#' the JS class, queued by `Blockr.registerBlock()` until the element binds.
#'
#' Reused by the bespoke servers (join, bind_rows, bind_cols) whose data
#' arity doesn't fit [new_js_transform_block()].
#' @noRd
js_block_sync <- function(input, session, name, input_name, r_state,
                          normalize_state = identity) {
  self_write <- new.env(parent = emptyenv())
  self_write$active <- FALSE

  observeEvent(input[[input_name]], {
    self_write$active <- TRUE
    r_state(input[[input_name]])
  })

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

  invisible(self_write)
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
