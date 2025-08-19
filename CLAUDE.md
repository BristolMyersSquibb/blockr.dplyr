# blockr.dplyr Development Guide

## Overview
This document provides guidance for improving the `blockr.dplyr` package, which provides dplyr-powered data wrangling blocks for blockr.core.

## Current State

### Existing Blocks
- **select** - Column selection
- **filter** - Row filtering with conditions
- **mutate** - Add/modify columns with multi-expression support ✅
- **arrange** - Row ordering
- **summarize** - Group summaries with `.by` parameter and multi-expression support ✅
- **join** - Combining tables (lower priority)

### Recently Completed ✅
1. **Multi-expression support** - Both mutate and summarize blocks now support multiple expressions with dynamic add/remove functionality
2. **dplyr function compatibility** - Fixed `n()`, `first()`, `last()`, `row_number()` and other dplyr functions by properly importing them via `@importFrom`
3. **Clean code architecture** - Removed complex regex preprocessing in favor of proper namespace imports
4. **Comprehensive testing** - Added 30+ tests covering single/multiple expressions, grouping, and edge cases
5. **Proper roxygen workflow** - Updated package to use roxygen2 for NAMESPACE generation

### Recently Completed ✅
6. **Fixed autocompletion** - ACE editors now work perfectly across all blocks:
   - Column name suggestions from input data (appear first in suggestions)
   - dplyr function suggestions organized by category (arithmetic, aggregate, ranking, etc.)
   - Functions auto-insert with parentheses and position cursor inside
   - Improved timing and race condition handling via `initialize_ace_editor()`
   - Works in mutate, summarize, filter, and all expression blocks

## Current Status: Major Features Complete ✅

All core blocks (mutate, summarize, filter) now have advanced multi-expression/multi-condition support with full autocompletion. Ready for new block development or advanced UI enhancements.

### Working Features ✅
- **Multi-expression support** in mutate and summarize blocks
- **Multi-condition support** in filter block with AND/OR logic
- **Dynamic add/remove** expressions and conditions with proper UI feedback
- **dplyr function compatibility** - all common functions work correctly
- **Comprehensive autocompletion** - column names and function suggestions across all blocks
- **Proper state management** - save/restore functionality works
- **Full test coverage** - 80+ tests covering all functionality
- **Clean architecture** - proper namespace imports, no regex preprocessing
- **Backward compatibility** - all existing functionality preserved

## Recently Completed ✅
7. **Enhanced Filter Block** - Major upgrade with multi-condition support:
   - **Multi-condition interface** - Visual condition builder with add/remove functionality  
   - **AND/OR logic** - Dropdown selectors to choose between condition operators
   - **Backward compatibility** - `multi_condition = FALSE` parameter for single-condition mode
   - **Comprehensive testing** - 28 tests covering all scenarios including edge cases
   - **Full autocompletion** - Column names and function suggestions in all condition fields

### Future New Blocks
- **group_by** - Explicit grouping block
- **arrange** - Enhanced sorting with multiple columns and directions
- **rename** - Column renaming
- **distinct** - Remove duplicates
- **slice** variants - Row selection by position

### UI/UX Improvements
- Drag-and-drop for reordering expressions/conditions
- Better validation messages
- Template/snippet system for common operations
- Column picker UI improvements

## Design Philosophy

### Modularity
The blockr ecosystem favors modular, reusable components. Consider creating modules that could be reused across different blocks.

### User Experience
- Make common tasks easy
- Provide helpful defaults
- Give clear feedback on errors
- Support both beginners and advanced users

### Code Patterns
Follow the existing patterns in the codebase:
- Blocks use `new_transform_block()` as a base
- Modules use `moduleServer()` and return reactive values
- State is managed through reactive expressions
- UI and server logic are separated

## Technical Considerations

### Package Development
- This package uses roxygen2 for documentation and NAMESPACE generation
- After modifying imports (e.g., adding @importFrom statements), run `devtools::document()` or `roxygen2::roxygenise()` to regenerate NAMESPACE
- Do not edit NAMESPACE directly - use roxygen comments in R files
- **README.md is generated from README.Rmd** - Always edit README.Rmd, never edit README.md directly

### Testing Workflow
After making changes to the code, use this workflow to test:

**Option 1: R Console (Recommended)**
1. Install package: `R CMD INSTALL . --no-multiarch`
2. Open R console/RStudio
3. Run any of these to test specific functionality:
   ```r
   # Test multi-expression mutate
   library(blockr.core); serve(new_mutate_block(), list(data = mtcars))
   
   # Test multi-expression summarize 
   library(blockr.core); serve(new_summarize_block(), list(data = mtcars))
   
   # Test multi-condition filter (new!)
   library(blockr.core); serve(new_filter_block(), list(data = mtcars))
   
   # Test standalone multi-filter module
   library(blockr.dplyr); run_multi_filter_example()
   ```
4. Test in browser: Check multi-expressions/conditions, dplyr functions, autocompletion

**Option 2: Development Testing**
1. `pkgload::load_all()` (loads development code without installing)
2. Run examples from README.md manually in R console/RStudio
3. Use the enhanced filter examples provided in the README

**Option 3: Automated Testing**
- Run `devtools::test()` to ensure all 80+ unit tests pass
- Run `devtools::test(filter = 'filter')` to test filter-specific functionality
- This verifies core functionality without needing browser testing

**Note**: Running Shiny apps via command-line scripts may not work reliably in all environments. Use R console/RStudio for interactive testing.

### Shiny Reactivity
- Be mindful of reactive dependencies
- Use `isolate()` when needed to prevent circular updates
- Consider using `reactiveVal()` for values that need explicit control

### ACE Editor Integration
- The package uses ACE editors for code input
- Initialize them properly for dynamic content
- Set up autocompletion with column names

#### Autocompletion: requirements and usage

Autocompletion is provided by a custom ACE completer plus live/basic autocompletion from `shinyAce`. To ensure it works reliably:

- UI requirements
  - Include `shinyjs::useShinyjs()` somewhere in the page (the modules in this package already do this in their own UIs).
  - Render the editor with `setup_ace_editor(id)` (used internally by the `mod_*_ui()` helpers).

- Server requirements
  - After the UI is rendered, call `initialize_ace_editor(session, editor_id, column_names)` where `column_names` are the current columns you want suggested (typically `colnames(data())`).
  - This enables live/basic autocompletion and provides both function and column-name suggestions.

- Minimal example (standalone)

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

- Within blocks
  - `mod_vexpr_ui()`, `mod_kvexpr_ui()`, and `mod_multi_kvexpr_ui()` call `setup_ace_editor()` for you and include `useShinyjs()`.
  - Their paired server modules (`mod_vexpr_server()`, `mod_kvexpr_server()`, `mod_multi_kvexpr_server()`) call `initialize_ace_editor()` with `colnames(data())`, so when you use `new_mutate_block()` or `new_summarize_block()` via `blockr.core::serve(...)`, autocompletion just works.

- What the initialization does
  - Registers a custom completer (functions/categories + column names).
  - Sets `enableLiveAutocompletion` and `enableBasicAutocompletion` on the ACE editor.
  - Defers setup until ACE and the editor DOM are available, so order-of-load issues do not break autocomplete.

- Troubleshooting
  - If suggestions don’t appear:
    - Ensure `shinyAce` and `shinyjs` are installed and up to date.
    - Confirm `shinyjs::useShinyjs()` is present in the UI (or use the provided UI modules).
    - Check the browser console for `ace is undefined` errors; if present, ensure ACE assets are loading (provided by `shinyAce`).
    - Verify `initialize_ace_editor()` is called with the correct editor id and a non-empty `column_names` vector.

### State Management
- State must be serializable for save/restore functionality
- Return state as a list from blocks
- Handle both initialization and updates gracefully

## Success Criteria
- Users can easily add multiple mutations or summaries
- The interface is intuitive and responsive
- Existing functionality remains intact
- Code is clean and maintainable
- Tests pass and coverage improves

## Resources
- Check git history for previous implementations
- Look at existing modules for patterns
- The blockr.core package documentation
- dplyr documentation for function semantics
- update README.Rmd not md