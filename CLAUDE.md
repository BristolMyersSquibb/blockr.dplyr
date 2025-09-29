# blockr.dplyr Development Guide

## Overview
`blockr.dplyr` provides interactive dplyr-powered data transformation blocks for [blockr.core](https://github.com/cynkra/blockr.core). Each block wraps dplyr functions with visual interfaces for code-free data manipulation.

## Writing a New Block

### Minimal Block Pattern
Every block follows this structure using `new_transform_block()`:

```r
#' @export
new_example_block <- function(param = default_value, ...) {
  new_transform_block(
    function(id, data) {  # or (id, x, y) for multi-dataframe blocks
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Core reactive for result
        result <- reactive({
          req(data())  # Wait for valid data
          df <- data()

          # Your dplyr transformation here
          dplyr::your_function(df, ...)
        })

        # Return list with required elements
        list(
          result = result,
          state = reactive(list(param = input$param))
        )
      })
    },
    ui = function(id, param) {
      ns <- NS(id)
      div(
        # CRITICAL: Initialize with constructor params
        selectInput(ns("param"), "Label", choices = param, selected = param)
      )
    },
    fields = list(param = param),
    ...
  )
}
```

### Key Technical Requirements

#### 1. UI Initialization Pattern ⚠️
**Inputs MUST initialize with constructor parameters in the UI definition:**
```r
# ✅ CORRECT - Initialize in UI function
selectInput(NS(id, "param"), choices = param, selected = param)

# ❌ WRONG - Empty initialization causes timing issues
selectInput(NS(id, "param"), choices = character())
```

#### 2. Multi-Dataframe Blocks
For join/bind operations, use this signature:
```r
new_join_block <- function(...) {
  new_transform_block(
    function(id, x, y) {  # Two data inputs
      # Access via x() and y() reactives
    }
  )
}
```

#### 3. ACE Editor Setup
For expression inputs with autocompletion:
```r
# In UI
blockr.dplyr:::setup_ace_editor(NS(id, "expr"))

# In server
blockr.dplyr:::initialize_ace_editor(session, "expr", colnames(data()))

```

#### 4. State Management
Return state as a list with serializable values:
```r
state = reactive(list(
  param1 = input$param1,
  param2 = input$param2
))
```

### Reusable Modules

The package includes several reusable UI modules:

- **`mod_by_selector`** - Column selector for grouping (`.by` parameter)
- **`mod_join_keys`** - Visual join key configuration
- **`mod_enhanced_filter`** - Multi-condition filter builder
- **`mod_value_filter`** - Value-based filtering interface
- **`mod_table_select`** - Table-based column selection

### Block Validation

Use the `blockr-validate-blocks` agent for automated testing:
```bash
# Validate all blocks automatically
Task: Use blockr-validate-blocks agent
```

This generates screenshots showing whether blocks render correctly (UI + output) or are broken (UI only).

## Package Development

- **Documentation**: Use roxygen2 (`@export`, `@importFrom`). Run `devtools::document()` after changes
- **README**: Edit `README.Rmd`, not `README.md` (auto-generated)
- **Formatting**: Use Air formatter (`air format .`)
- **Testing**: Run existing tests with `devtools::test()`

## Resources

- [blockr.core documentation](https://github.com/cynkra/blockr.core)
- [Example blocks in this package](R/)
- [dplyr documentation](https://dplyr.tidyverse.org/)
