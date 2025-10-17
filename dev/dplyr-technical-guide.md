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

**Issue**: When inputs are created inside `renderUI()` and content changes based on user interaction:
- Old inputs are destroyed when renderUI content changes
- New inputs take time to be registered with Shiny
- Submit handlers may try to access inputs before they're fully available

**Solution**: Use static UI with `conditionalPanel()` instead of dynamic `renderUI()`:

```r
# PROBLEMATIC: Input in dynamic UI
output$dynamic_ui <- renderUI({
  selectInput(NS(id, "n"), "Number", choices = 1:10)  # Won't be available in input$n
})

# SOLUTION: Input in static UI
function(id) {
  div(
    selectInput(NS(id, "n"), "Number", choices = 1:10),  # Available in input$n
    uiOutput(NS(id, "dynamic_ui"))  # Keep only non-critical elements dynamic
  )
}
```

**Best Practices**:
- Use static UI for inputs that need to be read in submit handlers
- Reserve `renderUI()` for truly dynamic content (conditional panels, varying choices)
- Consider `updateSelectInput()` for dynamic updates instead of full re-rendering

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
- **README.md is generated from README.Rmd** - Always edit README.Rmd, never edit README.md directly

### Code Formatting

This project uses **Air**, an extremely fast R code formatter written in Rust.

**Installation:**
```bash
# Unix/macOS
curl -LsSf https://github.com/posit-dev/air/releases/download/0.1.1/air-installer.sh | sh

# Windows (PowerShell)
powershell -ExecutionPolicy Bypass -c "irm https://github.com/posit-dev/air/releases/download/0.1.1/air-installer.ps1 | iex"
```

**Usage:**
```bash
# Format entire project
air format .

# Check formatting (useful for CI/CD)
air format . --check
```

**Why Air?**
- ~100x faster than styler (written in Rust vs R)
- Easy CI/CD integration with pre-compiled binary
- Editor integration: format-on-save support in VS Code, RStudio, Positron

---

## Screenshot Validation

### Using the Validation Agent

**ALWAYS USE THE VALIDATION AGENT FOR BLOCK VALIDATION**

The blockr ecosystem includes a specialized `blockr-validate-blocks` agent that handles screenshot validation automatically.

#### Core Principle
- **✅ Working Block**: Screenshot shows both configuration UI and rendered output
- **❌ Broken Block**: Screenshot shows only configuration UI, no output

#### Key Benefits

1. **Automated Configuration**: Agent knows how to configure each block type optimally
2. **Comprehensive Testing**: Tests all block types systematically
3. **Error Analysis**: Provides detailed diagnosis when blocks fail
4. **Consistent Results**: Standardized validation across all blockr packages

#### Common Issues Revealed

- **Missing `allow_empty_state`**: Blocks won't evaluate due to empty optional fields
- **Malformed dplyr expressions**: Syntax errors preventing table rendering
- **Broken reactive flows**: Field validation failures blocking data flow
- **UI timing issues**: Dynamic UI elements not properly initialized
- **State management issues**: Missing or incorrect state list configurations

#### Development Workflow

1. **Block Creation**: Use validation agent after implementing new block
2. **Refactoring**: Validate changes don't break rendering
3. **Debugging**: First diagnostic step when blocks misbehave
4. **Code Review**: Visual proof that blocks work end-to-end

#### Manual Validation (Advanced)

```r
# Single block validation
library(blockr.ggplot)  # Provides validation functions
result <- validate_block_screenshot(
  new_select_block(columns = c("mpg", "cyl", "hp")),
  data = mtcars,
  filename = "select-test.png"
)

# Batch validation
blocks <- list(
  select = new_select_block(columns = c("mpg", "cyl")),
  filter = new_filter_block(),
  mutate = new_mutate_block()
)
results <- validate_blocks_batch(blocks)
```

**File Locations**: Screenshots saved to `man/figures/*.png`

---

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
