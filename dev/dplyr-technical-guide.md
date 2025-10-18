# blockr.dplyr Technical Guide

## Block Initialization Pattern

**CRITICAL: UI inputs MUST initialize with constructor parameters.** This is essential for proper state management and save/restore functionality.

### The Pattern (Required)

```r
# In UI function - constructor parameters go directly in UI
selectInput(
  inputId = NS(id, "param"),
  choices = param,      # ← Constructor parameter
  selected = param      # ← Constructor parameter
)

# In server function - use updateSelectInput for dynamic changes
observeEvent(data_changes(), {
  updateSelectInput(
    session,
    inputId = "param",
    choices = new_choices,
    selected = current_selection()  # ← Preserve current selection
  )
})
```

### Why This Matters

- **blockr.core compatibility** - All blockr.core blocks follow this pattern
- **Reliable initialization** - Constructor parameters appear immediately without timing issues
- **State persistence** - Save/restore works correctly across sessions
- **No renderUI for inputs** - Dynamic UI causes timing races and initialization failures

### Module Pattern

When using modules that need constructor parameters:

```r
# Module UI function
mod_ui <- function(id, initial_choices = character(), initial_selected = character()) {
  ns <- NS(id)
  selectInput(ns("input"), choices = initial_choices, selected = initial_selected, multiple = TRUE)
}

# In block UI - pass constructor parameters
mod_ui(NS(id, "module"), initial_choices = param, initial_selected = param)
```

### What NOT to Do ❌

```r
# BAD: Empty UI with renderUI - causes timing issues
selectInput(NS(id, "param"), choices = character(), selected = character())

output$dynamic_ui <- renderUI({
  selectInput(NS(id, "param"), choices = get_choices())  # Will be empty on startup
})
```

---

## Dynamic UI Timing Issues

**The Problem**: When inputs are created inside `renderUI()`, they are not immediately available:
- Inputs created in `renderUI()` take time to register with Shiny
- Reading `input$x` before the input exists returns `NULL`
- This causes issues during block initialization and when UI content changes

**IMPORTANT: Never use priority settings to solve timing issues.** Priority settings (`priority` argument in `observe()`, etc.) are not the right solution and create fragile, hard-to-maintain code.

**The Right Solutions**:

### For Static-Count Inputs (Preferred)

When the number of inputs is fixed, avoid `renderUI()` entirely:

```r
# ❌ PROBLEMATIC: Input in dynamic UI
output$dynamic_ui <- renderUI({
  selectInput(NS(id, "n"), "Number", choices = 1:10)  # Won't be available immediately
})

# ✅ SOLUTION: Static UI with updates
function(id) {
  div(
    selectInput(NS(id, "n"), "Number", choices = 1:10),  # Always available
    uiOutput(NS(id, "dynamic_display"))  # Reserve renderUI for display-only elements
  )
}

# Update choices dynamically in server
observeEvent(data(), {
  updateSelectInput(session, "n", choices = new_choices)
})
```

### For Multi-Row Inputs (When renderUI is Unavoidable)

When users can add/remove rows dynamically, `renderUI()` is necessary. Use the **two-phase initialization pattern** to handle timing:

```r
# The pattern used by all multi-row modules
reactive({
  indices <- r_indices()

  # Check if inputs exist in the session yet
  has_inputs <- any(sapply(indices, function(i) {
    paste0("row_", i, "_input") %in% names(input)
  }))

  if (has_inputs) {
    # Phase 2: Inputs exist, read from them
    read_current_values_from_inputs()
  } else {
    # Phase 1: During initialization, use stored values
    r_stored_values()
  }
})
```

**This two-phase pattern solves timing issues without priority settings.** See "Multi-Row Input Blocks" section below for details.

**Summary**:
- **Preferred**: Use static UI + `updateSelectInput()` when possible
- **Exception**: Multi-row blocks need `renderUI()` + two-phase pattern
- **Never**: Don't use priority settings to solve timing issues

---

## Multi-Row Input Blocks: When renderUI is Necessary

### When You Need renderUI

Some blocks allow users to **dynamically add and remove rows** at runtime. These require `renderUI()` because:

1. **Variable number of inputs**: The count isn't known at UI definition time
2. **Unique input IDs**: Each row needs distinct IDs (`expr_1`, `expr_2`, `expr_3`, etc.)
3. **Dynamic creation/destruction**: Inputs must be created when rows are added and destroyed when removed
4. **No alternative**: `updateSelectInput()` cannot create/destroy inputs

**Examples in blockr.dplyr:**
- **Mutate block**: Add/remove multiple expressions (`new_col = expr`)
- **Summarize block**: Add/remove multiple summary expressions
- **Filter block**: Add/remove multiple filter conditions with AND/OR logic
- **Rename block**: Add/remove multiple rename pairs (`new_name <- old_name`)
- **Join block**: Add/remove multiple join key mappings
- **Arrange block**: Add/remove multiple sort columns

### The Multi-Row Pattern

All multi-row blocks follow the same pattern. Here's the complete structure with accurate implementation details:

```r
mod_multi_row_server <- function(id, get_value, get_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Initialize from constructor parameters
    # get_value and get_cols are plain functions (not reactives)
    # They're passed as closures that capture constructor parameters
    initial_values <- get_value()  # Called once at module creation
    if (length(initial_values) == 0) {
      initial_values <- list(default_value)
    }

    # 2. Track which row indices currently exist
    r_indices <- reactiveVal(seq_along(initial_values))  # e.g., c(1, 2)
    r_next_index <- reactiveVal(length(initial_values) + 1)  # e.g., 3
    r_values <- reactiveVal(initial_values)

    # 3. renderUI creates rows based on current indices
    output$rows_ui <- renderUI({
      indices <- r_indices()
      values <- r_values()

      tagList(
        lapply(seq_along(indices), function(j) {
          i <- indices[j]
          create_row_ui(
            ns(paste0("row_", i)),
            value = values[[j]],
            show_remove = (length(indices) > 1)
          )
        })
      )
    })

    # 4. Add row button
    observeEvent(input$add_row, {
      current_indices <- r_indices()
      new_index <- r_next_index()

      r_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Add new value to stored values
      current_values <- get_current_values()
      r_values(c(current_values, list(default_value)))
    })

    # 5. Dynamic remove button handlers (created in observe)
    observe({
      indices <- r_indices()
      lapply(indices, function(i) {
        observeEvent(input[[paste0("row_", i, "_remove")]], {
          if (length(r_indices()) > 1) {
            r_indices(setdiff(r_indices(), i))
            r_values(get_current_values())
          }
        })
      })
    })

    # 6. Helper function to read from inputs
    get_current_values <- function() {
      indices <- r_indices()
      result <- list()
      for (i in indices) {
        val <- input[[paste0("row_", i, "_input")]]
        if (!is.null(val) && val != "") {
          result <- append(result, list(val))
        }
      }
      result
    }

    # 7. TWO-PHASE PATTERN: Return reactive that handles initialization timing
    reactive({
      indices <- r_indices()

      # Check if inputs exist in the session yet
      has_inputs <- any(sapply(indices, function(i) {
        paste0("row_", i, "_input") %in% names(input)
      }))

      if (has_inputs) {
        # Phase 2: Inputs are registered, read current values from them
        get_current_values()
      } else {
        # Phase 1: During initialization, inputs don't exist yet
        # Use stored values from r_values() instead
        r_values()
      }
    })
  })
}
```

### Module Usage in Blocks

Multi-row modules are instantiated in block server functions like this:

```r
# In mutate block server (simplified)
server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    r_exprs <- mod_multi_kvexpr_server(
      id = "mkv",
      get_value = \() exprs,  # Plain function returning constructor param
      get_cols = \() colnames(data())  # Function that reads reactive data()
    )
    # ...
  })
}
```

**Key points:**
- `get_value = \() exprs` - Returns the constructor parameter (called once at initialization)
- `get_cols = \() colnames(data())` - Function that can read from reactive `data()` when called in reactive context
- Both are plain functions, not reactives themselves

### Why This Avoids Timing Issues

The **two-phase pattern** (step 7 above) solves the timing problem elegantly:

1. **Phase 1 (Initialization)**: When the block first loads, `renderUI()` hasn't created inputs yet. The reactive returns stored values from `r_values()`.

2. **Phase 2 (Normal operation)**: Once inputs exist in the session, the reactive reads directly from `input$*`.

This approach:
- ✅ Works reliably without priority settings
- ✅ Handles initialization gracefully
- ✅ Preserves values during row add/remove operations
- ✅ Follows reactive programming best practices

### Real Implementation Examples

See these files for complete working implementations:

- **[multi_kvexpr.R:125](blockr.dplyr/R/multi_kvexpr.R#L125)**: Key-value expressions (mutate, summarize)
  - Two-phase pattern at lines 169-172
- **[multi_filter.R:160](blockr.dplyr/R/multi_filter.R#L160)**: Filter conditions with AND/OR logic
  - Two-phase pattern at lines 225-255
- **[multi_rename.R:138](blockr.dplyr/R/multi_rename.R#L138)**: Rename pairs
  - Two-phase pattern at lines 177-193
- **[mod_join_keys.R:262](blockr.dplyr/R/mod_join_keys.R#L262)**: Join key mappings
  - Two-phase pattern at lines 353-385
- **[multi_arrange.R:129](blockr.dplyr/R/multi_arrange.R#L129)**: Multi-column sorting
  - Two-phase pattern at lines 161-177

All use the same two-phase pattern in their reactive return values.

### Decision Framework

```
Do you need variable number of inputs (add/remove rows)?
│
├─ NO → Use static UI (see "Block Initialization Pattern")
│        Examples: select, slice, distinct blocks
│        Pattern: Static selectInput + updateSelectInput()
│
└─ YES → You need renderUI for multi-row inputs
         Examples: mutate, filter, rename, join, arrange
         Pattern: renderUI + two-phase initialization
         ⚠️  NEVER use priority settings to solve timing
```

---

## Error Handling Pattern

**The `cond` pattern in blockr.core is handled automatically by the framework**, not by individual blocks.

### How it works

1. **blockr.core manages conditions**: Framework has built-in error handling through `rv$data_cond`, `rv$state_cond`, and `rv$eval_cond` reactive values
2. **Individual blocks use `req()`**: Blocks use `req()` to prevent execution when preconditions aren't met
3. **Framework catches and reports**: blockr.core automatically catches errors and populates the `cond` structure

### Implementation

```r
# Use req() to prevent execution when conditions aren't met
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Validate preconditions
    rows_compatible <- reactive({
      nrow(data()) > 0
    })

    list(
      expr = reactive({
        req(rows_compatible())  # Prevents execution if FALSE
        # Build expression...
      }),
      state = list(...)
    )
  })
}
```

**Why no manual `cond` needed**: The framework handles error conditions automatically when `req()` fails or when block evaluation throws errors.

---

## ACE Editor Integration

The package uses ACE editors for code input with autocompletion support.

### Requirements

**UI requirements:**
- Include `shinyjs::useShinyjs()` somewhere in the page
- Render the editor with `setup_ace_editor(id)`

**Server requirements:**
- Call `initialize_ace_editor(session, editor_id, column_names)` after UI is rendered
- Typically use `colnames(data())` for column names

### Minimal Example

```r
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  shinyjs::useShinyjs(),
  blockr.dplyr:::setup_ace_editor("expr")
)

server <- function(input, output, session) {
  cols <- colnames(mtcars)
  blockr.dplyr:::initialize_ace_editor(session, "expr", cols)
}

shiny::shinyApp(ui, server)
```

### Within Blocks

- `mod_vexpr_ui()`, `mod_kvexpr_ui()`, and `mod_multi_kvexpr_ui()` call `setup_ace_editor()` for you
- Their paired server modules call `initialize_ace_editor()` with `colnames(data())`
- When using `new_mutate_block()` or `new_summarize_block()`, autocompletion just works

### What the Initialization Does

- Registers a custom completer (functions/categories + column names)
- Sets `enableLiveAutocompletion` and `enableBasicAutocompletion` on the ACE editor
- Defers setup until ACE and the editor DOM are available

### Troubleshooting

If suggestions don't appear:
- Ensure `shinyAce` and `shinyjs` are installed and up to date
- Confirm `shinyjs::useShinyjs()` is present in the UI
- Check the browser console for `ace is undefined` errors
- Verify `initialize_ace_editor()` is called with correct editor id and non-empty `column_names`

---

## Package Development

### roxygen2 Workflow

- This package uses roxygen2 for documentation and NAMESPACE generation
- After modifying imports (e.g., adding @importFrom statements), run `devtools::document()` or `roxygen2::roxygenise()`
- Do not edit NAMESPACE directly - use roxygen comments in R files


## Design Decisions: Argument Naming Conventions

When creating new blocks, follow blockr.core's pattern: **match the underlying R function's argument names when possible**. When that's not feasible (like dplyr's `...`), use:

1. **R expressions**: `exprs` (plural, named or unnamed)
2. **Column selection**: `columns` (plural)
3. **Grouping**: `by` (dplyr convention)
4. **Mappings**: descriptive plural names (`renames`, `joins`, etc.)
5. **Structured config**: descriptive plural names (`conditions`, `rules`, etc.)

---

## To-Do List

**Existing blocks to enhance:**
- [ ] **slice block** - Review UI layout, ensure consistent styling with other blocks
- [ ] **select block** - Rework UI, using multi-select dropdown for column selection
- [ ] **arrange block** - Improve multi-column arrangement UI clarity
- [ ] **distinct block** - Enhance reactive UI responsiveness and visual feedback
- [ ] **Enhance `value_filter` to handle NA values** - Users need to be able to filter based on NA values.
   - Would require special handling in expression generation (NA needs `is.na()` not `%in%`)
   - Makes value_filter more complete and flexible
   - Users could then exclude NAs by selecting NA and clicking "Exclude" checkbox
- [ ] **join block** Visually enhance, complex join types can go to next version.

**New blocks to implement:**
- [ ] **pivot_wider block** - Transform long data to wide format (tidyr)
- [ ] **pivot_longer block** - Transform wide data to long format (tidyr)

### Guiding Principle: When to Add a New Block

**Add a new block when it does something fundamentally different:**
- ✅ Pivot blocks - reshape entire data structure (cannot be replicated with other blocks)
- ✅ Join blocks - merge datasets based on keys (fundamentally different operation)
- ✅ Summarize blocks - aggregate data (fundamentally different from row-wise operations)

**Don't add a block when it's just a specialized version of an existing operation:**
- ❌ `count` / `tally` - Just `summarize(n = n()) + arrange(desc(n))`, not fundamentally different
- ❌ `separate` / `unite` - Just specialized mutate operations with string functions
- ❌ `drop_na` - Can use filter block with `!is.na(column)` or enhance value_filter to handle NAs
- ❌ `relocate` - Can be done with select block (marginal benefit)
- ❌ `fill` - Less commonly used, wait for user demand
- ❌ `group_by` / `ungroup` - The `.by` argument in individual blocks is more modern (dplyr 1.1.0+) and cleaner


### V2: Advanced Modes

For a later iteration, we can add advanced modes for the following blocks:

- Filter (combined simple/advanced mode)
- Joins (https://github.com/BristolMyersSquibb/blockr.dplyr/issues/21)
- Arrange by expression

These would all follow a similar pattern: there should be a switch to go from the simple mode to and advanced, expression based mode.


## Best Practices Summary

### Shiny Reactivity
- Be mindful of reactive dependencies
- Use `isolate()` when needed to prevent circular updates
- Consider using `reactiveVal()` for values that need explicit control

### State Management
- State must be serializable for save/restore functionality
- Return state as a list from blocks
- Handle both initialization and updates gracefully

### Modularity
- Create reusable components for common patterns
- Follow existing patterns in the codebase
- Separate UI and server logic clearly
