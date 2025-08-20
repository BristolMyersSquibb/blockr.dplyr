# blockr.dplyr Development Guide

## Overview
This document provides guidance for improving the `blockr.dplyr` package, which provides dplyr-powered data wrangling blocks for blockr.core.

## Current State

### Existing Blocks
- **select** - Column selection with enhanced visual interface ✅
- **filter** - Row filtering with multi-condition support and AND/OR logic ✅
- **mutate** - Add/modify columns with multi-expression and `.by` parameter support ✅
- **arrange** - Row ordering with multi-column sorting ✅
- **summarize** - Group summaries with `.by` parameter and multi-expression support ✅
- **slice** - Row selection by position/value/sampling with `.by` parameter support ✅
- **rename** - Column renaming with visual mapping interface ✅
- **distinct** - Remove duplicate rows with multi-column selection ✅
- **join** - Enhanced multi-column joins with visual interface ✅
- **bind_rows** - Stack datasets vertically with column matching ✅
- **bind_cols** - Combine datasets side-by-side with conflict resolution ✅

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
   - **Comprehensive testing** - 34 tests covering all scenarios including edge cases
   - **Full autocompletion** - Column names and function suggestions in all condition fields

8. **New Rename Block** - Complete column renaming functionality:
   - **Visual mapping interface** - Clear new_name ← old_name with arrow indicators
   - **Add/remove functionality** - Dynamic interface to add/remove rename pairs
   - **Column validation** - Dropdown selectors ensure old column names exist
   - **Duplicate prevention** - Validation prevents renaming the same column multiple times
   - **Comprehensive testing** - 31 tests covering validation, UI components, and integration
   - **Polished UI** - Perfect alignment and full horizontal space utilization

## Recently Completed ✅
9. **Enhanced Arrange Block** - Complete multi-column sorting functionality:
   - **Multi-column interface** - Visual list with individual ASC/DESC controls per column
   - **Individual direction controls** - Each column gets its own ASC/DESC dropdown
   - **Visual priority indicators** - Clear 1., 2., 3. numbering showing sort order
   - **Add/remove functionality** - Dynamic interface to add/remove sort columns
   - **Backward compatibility** - Supports both character vectors and list specifications
   - **Comprehensive testing** - 22 tests covering all input formats and integration
   - **Clean UI** - Simple interface without sorting arrows for intuitive use

10. **New Distinct Block** - Complete duplicate removal functionality:
   - **Multi-column selection** - Select specific columns to check for uniqueness
   - **Keep all columns option** - Control whether to keep all columns or just selected ones
   - **Duplicate count preview** - Real-time display of how many duplicates will be removed
   - **Empty selection support** - Leave columns empty to check all columns for duplicates
   - **Comprehensive testing** - 10 tests covering various scenarios including NA handling
   - **Clean UI** - Simple interface with informative duplicate count display

11. **Unified .by Parameter Implementation** - Consistent grouping across all blocks:
   - **Unified mod_by_selector component** - Reusable Shiny module for column selection across all blocks
   - **Added .by support to mutate block** - Grouped mutations with proper dplyr syntax
   - **Refactored summarize and slice blocks** - Now use unified component for consistency
   - **Visual consistency** - Same "Group by columns (optional)" interface across all blocks
   - **State management** - Proper save/restore functionality for .by parameter
   - **Comprehensive testing** - 29+ tests covering unified component and block integrations
   - **No separate group_by block needed** - Integrated grouping eliminates UI complexity

12. **Enhanced Join Block** - Complete transformation to production-ready multi-dataframe operations:
   - **Advanced join configuration** - `mod_join_keys` component for complex column mappings
   - **Multi-column support** - Both same-name (natural) and different-name column joins
   - **Enhanced UI** - Visual join key mapping with descriptive join type labels  
   - **All join types** - Support for left, inner, right, full, semi, and anti joins
   - **Comprehensive testing** - 39 passing tests covering all join scenarios
   - **Backward compatibility** - Maintains all existing functionality

13. **New Bind Rows Block** - Vertical dataset combination:
   - **Intelligent column matching** - Automatically aligns columns by name across datasets
   - **Missing column handling** - Fills non-matching columns with NA values
   - **Source tracking** - Optional ID column to identify which dataset each row originated from
   - **Real-time preview** - Shows column differences and result structure before applying
   - **Comprehensive testing** - 41 tests covering various bind scenarios including edge cases
   - **Clean UI** - Intuitive interface with operation preview and configuration options

14. **New Bind Columns Block** - Horizontal dataset combination:
   - **Row count validation** - Prevents binding datasets with different row counts with clear error messaging
   - **Duplicate column handling** - Configurable suffixes for columns with identical names
   - **Real-time validation** - Immediate feedback on compatibility issues and conflicts
   - **Custom naming control** - Full user control over how duplicate columns are renamed
   - **Comprehensive testing** - Included in 41-test bind suite covering compatibility scenarios
   - **Error prevention UI** - Visual indicators and validation prevent invalid operations

15. **Enhanced Select Block with Table Interface** - Space-efficient column selection:
   - **Compact table layout** - DataTable interface addressing vertical space concerns
   - **Colored type tags** - Visually distinct badges for data types (num, chr, fct, etc.)
   - **Smart column information** - Type, sample values, and statistics in compact format
   - **Sortable interface** - Click column headers to sort by selection status, name, or type
   - **Dual interface support** - Choose between table (default) or card interfaces
   - **Selected columns on top** - Optional display with numbered badges and easy removal
   - **Enhanced user experience** - Professional styling with visual feedback and intuitive controls

16. **Codebase Cleanup & API Simplification** - Removed legacy code paths and parameters:
   - **Filter block simplification** - Removed `multi_condition = FALSE` parameter, always use multi-condition interface
   - **Arrange block simplification** - Removed `desc` parameter, use list-based specifications only
   - **Documentation cleanup** - Removed outdated examples and parameters from all documentation
   - **Test suite refinement** - Updated tests to focus on current interfaces only
   - **Reduced API surface** - Fewer confusing parameters, single implementation paths for each feature
   - **Better maintainability** - Cleaner codebase with reduced complexity and technical debt

## Current Status: Feature Complete & Cleaned Up! 🎉

The blockr.dplyr package is now **feature complete** with all major dplyr operations implemented as interactive blocks, plus a cleaned-up codebase with simplified APIs:

### Complete Block Ecosystem ✅
- **Single-table operations**: select (table + card interfaces), filter, mutate, arrange, summarize, slice, rename, distinct
- **Multi-table operations**: all join types, bind_rows, bind_cols with visual interfaces  
- **Advanced UI components**: multi-expression editors, condition builders, table interfaces, column mappers
- **Full autocompletion**: ACE editors with column names and dplyr function suggestions across all blocks
- **Comprehensive testing**: 400+ tests covering all functionality, edge cases, and UI interactions
- **Production ready**: Complete state management, error handling, and backward compatibility

### Multi-Dataframe System Architecture ✅
- **Reusable components**: `mod_join_keys` for complex join configurations
- **Multi-input support**: All blocks use `function(id, x, y)` signature for two-dataset operations  
- **Enhanced UI/UX**: Rich previews, real-time validation, and comprehensive user guidance
- **Registry integration**: All blocks properly registered and available in blockr ecosystem

### Working Features ✅
- **Unified .by interface** - Consistent multi-select dropdown across mutate, summarize, and slice blocks
- **Integrated grouping** - No need for separate group_by block in workflows
- **Visual consistency** - Same styling, behavior, and user experience across all blocks
- **State management** - Proper save/restore of grouping selections
- **dplyr syntax compliance** - Generates correct `.by` parameter syntax for all operations
- **Multi-dataframe operations** - Complete join and bind functionality with advanced features

## Next Implementation Priority

### High Priority Missing Functions
Based on user demand and workflow completeness:

1. **`pull()` Block** - Extract single column as vector
   - **Use case**: Final step in pipelines to extract results
   - **Implementation**: Simple single-column selector → vector output
   - **Priority**: High - completes data extraction workflows

2. **`count()` Block** - Shortcut for group + summarize counting  
   - **Use case**: Quick frequency analysis and data exploration
   - **Implementation**: Column selector + optional grouping
   - **Priority**: High - very common operation

3. **`relocate()` Block** - Change column order/positioning
   - **Use case**: Reorder columns for better presentation
   - **Implementation**: Column drag/drop or position specification
   - **Priority**: Medium-High - useful for data presentation

### Optional Extensions  
4. **`glimpse()` Block** - Data overview/inspection
   - **Use case**: Quick data structure overview
   - **Implementation**: Read-only data preview with structure info
   - **Priority**: Medium - useful for exploration

### Architectural Enhancements
- **Add .by support to filter block** - Less common but potentially useful for grouped filtering
- **Add .by support to arrange block** - May be useful for some use cases

### Advanced/Specialized (Lower Priority)
- **Set operations**: `intersect()`, `union()`, `setdiff()`, `symdiff()` 
- **Advanced joins**: `cross_join()`, `nest_join()`
- **Row-wise operations**: `rowwise()` block

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

### CRITICAL: UI Initialization Pattern for Blocks ⚠️

**Block inputs MUST initialize with constructor parameters in the UI definition.** This is essential for proper state management and save/restore functionality in blockr.

#### The Pattern (Required for all blocks)
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

#### Why This Matters
- **blockr.core compatibility** - All blockr.core blocks follow this exact pattern (see subset, head, scatter, merge examples)  
- **Reliable initialization** - Constructor parameters appear immediately in UI without timing issues
- **State persistence** - Save/restore functionality works correctly across sessions
- **No renderUI for inputs** - Dynamic UI causes timing races and initialization failures

#### Module Pattern (for reusable components)
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

#### What NOT to Do ❌
```r
# BAD: Empty UI with renderUI - causes timing issues
selectInput(NS(id, "param"), choices = character(), selected = character())

output$dynamic_ui <- renderUI({
  selectInput(NS(id, "param"), choices = get_choices())  # Will be empty on startup
})
```

### Package Development
- This package uses roxygen2 for documentation and NAMESPACE generation
- After modifying imports (e.g., adding @importFrom statements), run `devtools::document()` or `roxygen2::roxygenise()` to regenerate NAMESPACE
- Do not edit NAMESPACE directly - use roxygen comments in R files
- **README.md is generated from README.Rmd** - Always edit README.Rmd, never edit README.md directly

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

## Future Cleanup Tasks

When all major features are complete, consider these cleanup opportunities:

### Code Simplification
- **Remove single-condition filter mode** - Once users are comfortable with the multi-condition interface, the `multi_condition = FALSE` parameter and associated `mod_vexpr_server` usage could be deprecated to simplify the codebase
- **Remove single arrange functionality** - Keep only the multi-arrange interface and remove the old single-column arrange logic to simplify the codebase
- **Consolidate similar modules** - Review if any patterns can be further abstracted across mutate, summarize, and filter implementations
- **Remove legacy code paths** - Clean up any remaining code that supported pre-multi-expression functionality

### Benefits of Cleanup
- Reduced code maintenance burden
- Simpler API with fewer parameters
- More focused testing requirements
- Cleaner architecture with single implementation paths

**Note**: These cleanups should only be considered after users have had time to adopt the new multi-condition interface and confirm it meets all their needs.

## Technical Issues & Solutions

### Dynamic UI Timing Issues (Slice Block - RESOLVED)

**Issue**: When inputs are created inside `renderUI()` and the rendered content changes based on user interaction, there can be timing issues where:
- Old inputs are destroyed when the renderUI content changes
- New inputs take time to be registered with Shiny
- Submit handlers may try to access inputs before they're fully available

**Symptoms**:
- Inputs work visually but may be NULL when accessed immediately after type changes
- Race conditions between UI rendering and submit button clicks
- Inconsistent behavior depending on user interaction speed

**Root Cause**: Dynamic UI recreation causes inputs to be destroyed and recreated, leading to potential timing gaps.

**Solution**: Use static UI with `conditionalPanel()` instead of dynamic `renderUI()`:
- All inputs exist from the start (hidden/shown via conditionalPanel)
- No destruction/recreation of inputs
- No timing issues or race conditions
- Immediate availability of all inputs

**Example**:
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
- Test input availability with `names(input)` during development
- Consider `updateSelectInput()` etc. for dynamic updates instead of full re-rendering

## Resources
- Check git history for previous implementations
- Look at existing modules for patterns
- The blockr.core package documentation
- dplyr documentation for function semantics
