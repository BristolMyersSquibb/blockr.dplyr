# blockr.dplyr Developer Guide

## Documentation

All developer documentation is in the
[`dev/`](https://bristolmyerssquibb.github.io/blockr.dplyr/dev/) folder:

- **[dev/blocks-core-guide.md](https://bristolmyerssquibb.github.io/blockr.dplyr/dev/blocks-core-guide.md)** -
  Universal block development concepts (start here)
- **[dev/ui-guidelines.md](https://bristolmyerssquibb.github.io/blockr.dplyr/dev/ui-guidelines.md)** -
  UI patterns and responsive layouts
- **[dev/dplyr-technical-guide.md](https://bristolmyerssquibb.github.io/blockr.dplyr/dev/dplyr-technical-guide.md)** -
  Technical patterns specific to blockr.dplyr

## Quick Reference

### Block Pattern

``` r
new_my_block <- function(param = default, ...) {
  ui <- function(id) {
    tagList(
      block_responsive_css(),
      div(class = "block-container",
        div(class = "block-form-grid",
          div(class = "block-section",
            tags$h4("Options"),
            div(class = "block-section-grid",
              div(class = "block-input-wrapper",
                selectInput(NS(id, "param"), "Label", choices = param, selected = param)
              )
            )
          )
        )
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      r_param <- reactiveVal(param)
      observeEvent(input$param, r_param(input$param))

      observeEvent(colnames(data()), {
        updateSelectInput(session, "param", choices = colnames(data()), selected = r_param())
      })

      list(
        expr = reactive({
          req(r_param())
          # Build dplyr expression
        }),
        state = list(param = r_param)
      )
    })
  }

  new_transform_block(server, ui, class = "my_block", ...)
}
```

### Critical Rules

1.  **Initialize UI inputs with constructor parameters** - Never use
    empty `choices = character()`
2.  **Use `req()` for validation** - Framework handles errors
    automatically
3.  **Static UI for inputs** - Use `conditionalPanel()`, not
    `renderUI()` for inputs
4.  **All constructor params in state** - State names must match
    constructor parameter names exactly

### Development Workflow

1.  Read
    [dev/blocks-core-guide.md](https://bristolmyerssquibb.github.io/blockr.dplyr/dev/blocks-core-guide.md)
2.  Implement following the pattern above
3.  Run `devtools::document()` after adding `@importFrom`
4.  Validate with `blockr-validate-blocks` agent
5.  Format with `air format .`

## Resources

- [blockr.core vignettes](https://blockr-org.github.io/blockr.core/)
- [Example
  blocks](https://bristolmyerssquibb.github.io/blockr.dplyr/R/) -
  filter.R, mutate.R, select.R
- [dplyr documentation](https://dplyr.tidyverse.org/)
