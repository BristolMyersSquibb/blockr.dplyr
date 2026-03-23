# JS-Driven Block Development Guide

All blockr.dplyr blocks follow a single pattern: JavaScript builds the UI, sends a JSON state to R, R builds a dplyr expression via `bbquote()`.

## Block anatomy

Each block has 4 files:

```
inst/js/filter-block.js      # JS class + Shiny input binding
inst/css/filter-block.css    # Block-specific CSS
R/filter_block.R             # R constructor + server + UI + htmlDependency
R/expr-builders.R            # Expression builder (shared file, one function per block)
```

## Reference implementation

Use the filter block as the canonical example: `R/filter_block.R`, `inst/js/filter-block.js`.

## R constructor pattern

```r
new_my_block <- function(state = list(param1 = default1), ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- reactiveVal(state)

        # Bidirectional sync
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        # Send column info to JS
        observeEvent(data(), {
          session$sendCustomMessage("my-columns",
            list(id = ns("my_input"), columns = colnames(data())))
        })

        # JS → R
        observeEvent(input$my_input, {
          self_write$active <- TRUE
          r_state(input$my_input)
        })

        # R → JS (external control)
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage("block-update",
              list(id = ns("my_input"), state = r_state()))
          }
        }, ignoreInit = TRUE)

        list(
          expr = reactive({
            s <- r_state()
            make_my_expr(s$param1 %||% default1)
          }),
          state = list(state = r_state)
        )
      })
    },
    function(id) {
      tagList(
        blockr_core_js_dep(), blockr_blocks_css_dep(),
        blockr_select_dep(),  # if using Blockr.Select
        blockr_input_dep(),   # if using Blockr.Input
        my_block_dep(),
        div(class = "block-container",
          div(id = NS(id, "my_input"), class = "my-block-container"))
      )
    },
    class = "my_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}
```

Key points:
- Single `state` reactiveVal, name matches constructor param
- `expr_type = "bquoted"` — expressions use `.(data)` placeholder resolved by blockr.core
- `allow_empty_state = "state"` — pass through data when no config set
- No `req()` — return identity expression for empty state

## JS class pattern

```javascript
(() => {
  'use strict';

  class MyBlock {
    constructor(el) {
      this.el = el;
      this._callback = null;
      this._submitted = false;
      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() { /* build UI, attach event listeners */ }
    _compose() { return { /* state JSON */ }; }
    _submit() { this._submitted = true; this._callback?.(true); }
    getValue() { return this._submitted ? this._compose() : null; }
    setState(state) { /* rebuild UI from state, do NOT fire _callback */ }
    updateColumns(cols) { /* update dropdowns with new column names */ }
  }

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (scope) => $(scope).find('.my-block-container'),  // jQuery required
    getId: (el) => el.id || null,
    getValue: (el) => el._block?.getValue() ?? null,
    setValue: (el, value) => el._block?.setState(value),
    subscribe: (el, callback) => { if (el._block) el._block._callback = () => callback(true); },
    unsubscribe: (el) => { if (el._block) el._block._callback = null; },
    initialize: (el) => {
      el._block = new MyBlock(el);
      if (el._pendingColumns) { el._block.updateColumns(el._pendingColumns); delete el._pendingColumns; }
      if (el._pendingState) { el._block.setState(el._pendingState); delete el._pendingState; }
    }
  });
  Shiny.inputBindings.register(binding, 'blockr.my');

  // Global message handlers (dispatch by msg.id)
  Shiny.addCustomMessageHandler('my-columns', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) el._block.updateColumns(msg.columns);
    else if (el) el._pendingColumns = msg.columns;
  });
})();
```

## Shared JS components

- `Blockr.Select.single(container, config)` / `.multi(container, config)` — dropdown, tags
- `Blockr.Input.create(container, config)` — code input with autocomplete
- `Blockr.icons` — SVG strings: `.x`, `.plus`, `.confirm`, `.code`, `.chevron`, `.remove`, `.gear`

## Shared CSS classes

| Class | Purpose |
|-------|---------|
| `.blockr-row` | Gray bordered row container (42px, flex) |
| `.blockr-row-content` | Flex-filling content area inside row |
| `.blockr-row-remove` | Remove button (hidden until row hover) |
| `.blockr-pill` | Click-through toggle button |
| `.blockr-add-row` | Footer bar with "Add" links |
| `.blockr-add-link` | "+ Add something" text link |
| `.blockr-add-link-expr` | Code icon button for adding expression rows |
| `.blockr-expr-confirm` | "Enter ↵" / checkmark confirm button |
| `.blockr-num-input` | Number input inside rows |
| `.blockr-text-input` | Standalone text input (42px, matches dock) |
| `.blockr-label` | Input label (12px, 500 weight) |
| `.blockr-select--bordered` | Add to standalone Blockr.Select for 42px bordered style |
| `.blockr-gear-header` | Top-right gear icon container |
| `.blockr-gear-btn` | Gear settings button |
| `.blockr-popover` | Settings popover panel |
| `.blockr-popover-row/label/input/checkbox` | Popover content elements |

## Row divider principle

First "key" input in a row gets `border-right` separator. Text separators (`=`, `→`, `of`) go between value-side elements.

## Expression building with bbquote

```r
make_my_expr <- function(param) {
  if (is_empty(param)) return(bbquote(.(data)))  # pass-through
  expr <- quote(dplyr::my_fn(.(data)))
  expr[["arg"]] <- as.name(param)
  bbquote(.(expr), list(expr = expr))
}
```

The `.(data)` placeholder is resolved by blockr.core's `eval_impl` at evaluation time.

For namespaced function calls, use `str2lang()`:
```r
expr <- as.call(list(str2lang("dplyr::slice_head"), quote(.(data))))
```

## Registry

```r
register_blocks("new_my_block",
  arguments = list(
    structure(
      c(state = "Description of the state object"),
      examples = list(state = list(param1 = "example_value")),
      prompt = "Optional LLM guidance"
    )
  )
)
```

## Testing

Unit tests for expression builders (pure R):
```r
eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, envir = list(data = df))
}

test_that("make_my_expr works", {
  expr <- make_my_expr("mpg")
  result <- eval_bquoted(expr, mtcars)
  expect_true(is.data.frame(result))
})
```

testServer tests (reactive context):
```r
test_that("my block evaluates", {
  blk <- new_my_block(state = list(param1 = "mpg"))
  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    expr_result <- session$returned$expr()
    evaluated <- eval_bquoted(expr_result, mtcars)
    expect_true(is.data.frame(evaluated))
  })
})
```
