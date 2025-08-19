# blockr.dplyr Development Plan

## Overview
This document outlines the improvement plan for the `blockr.dplyr` package, which provides dplyr-powered data wrangling blocks for blockr.core.

## Current State

### Existing Blocks (6)
1. **select** - Column selection
2. **filter** - Row filtering with conditions  
3. **mutate** - Add/modify columns
4. **arrange** - Row ordering
5. **summarize** - Group summaries (includes grouping via `by` parameter)
6. **join** - Combining tables (lower priority for improvements)

### Supporting Modules
- `vexpr` - Single expression input (used by filter)
- `kvexpr` - Key-value expression input (used by mutate, summarize)
- `flexpr` - Flexible expression input
- `ace_utils` - ACE editor integration for code completion

## Phase 1: Improve Existing Blocks

### Priority 1: Summarize Block
**Current Issues:**
- Debug print statements in code (lines 114, 145)
- No test coverage
- Poor default value: `paste('type', 'here')`
- Limited to single expression at a time
- No common aggregation helpers

**Improvements:**
1. Remove debug statements
2. Add comprehensive tests
3. Better defaults (e.g., `n()` for count)
4. Multiple expression support with add/remove UI
5. Dropdown with common functions:
   - `n()` - count
   - `mean()`, `sum()`, `min()`, `max()`
   - `sd()`, `median()`
   - `first()`, `last()`
6. Support for `across()` function
7. Preview of grouped data

### Priority 2: Filter Block  
**Improvements:**
1. UI helpers for common operations (`==`, `!=`, `>`, `<`, `%in%`)
2. Multiple condition builder with AND/OR logic
3. Better validation messages
4. Column-specific filter templates based on data type

### Priority 3: Select Block
**Improvements:**
1. Add tidy-select helpers:
   - `starts_with()`, `ends_with()`
   - `contains()`, `matches()`
   - `where()` for type-based selection
2. Column reordering with drag-and-drop
3. Search/filter for many columns
4. Preview of selected columns

### Priority 4: Mutate Block
**Improvements:**
1. Expression builder UI with common functions
2. Better column name autocomplete
3. Multiple mutations with add/remove
4. Function templates for common operations
5. Preview of new columns

### Priority 5: Arrange Block
**Improvements:**
1. Per-column ascending/descending controls
2. Drag-and-drop for column order
3. Support for `arrange_all()`, `arrange_at()`, `arrange_if()`
4. Visual indicators for sort direction

### Priority 6: Join Block (Lower Priority)
**Improvements:**
1. Visual preview of join result
2. Support for `suffix` parameter
3. Better key column matching UI
4. Join type visualization

## Phase 2: Add Essential New Blocks

### High Priority
1. **group_by** - Essential for grouped operations
   - Multi-select columns
   - Works seamlessly with summarize
   - Visual grouping indicator

2. **rename** - Column renaming
   - Key-value pairs UI
   - Drag-and-drop reordering
   - Batch rename with patterns

3. **distinct** - Remove duplicates
   - Column selection for uniqueness
   - `.keep_all` parameter
   - Preview duplicate counts

### Medium Priority
4. **slice** variants - Row selection by position
   - `slice_head()`, `slice_tail()`
   - `slice_sample()`, `slice_min()`, `slice_max()`
   - Numeric/percentage input

5. **relocate** - Column reordering
   - Drag-and-drop interface
   - Before/after positioning
   - Batch move operations

### Lower Priority (Not Initially Planned)
- count/tally - Can be achieved with summarize
- pull - Limited use in visual workflows
- pivot_longer/pivot_wider - Complex, need careful design

## Phase 3: Infrastructure Improvements

### Testing
- Add comprehensive test suite for all blocks
- Test UI components with shinytest2
- Test expression parsing and validation
- Test state management and serialization

### Documentation
- Add vignettes for common workflows
- Document expression syntax
- Provide examples for each block
- Create user guide for the UI

### Code Quality
- Remove all debug statements
- Consistent error handling
- Improved validation messages
- Code review and refactoring

## Implementation Guidelines

### Block Development Pattern
```r
new_*_block <- function(param1 = default1, ...) {
  new_transform_block(
    # Server function
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        # Reactive state management
        # Input validation
        # Expression generation
        # Return expr and state
      })
    },
    # UI function
    function(id) {
      # Create UI elements
      # Use existing modules where applicable
    },
    class = "*_block",
    ...
  )
}
```

### Best Practices
1. Use existing UI modules (vexpr, kvexpr) where applicable
2. Follow reactive programming patterns
3. Validate user input before evaluation
4. Provide helpful error messages
5. Include default values that work
6. Write tests for new functionality
7. Update registry when adding new blocks

### Testing Requirements
- Unit tests for expression generation
- Integration tests for block connectivity
- UI tests for user interactions
- Edge case handling (empty data, NA values, etc.)

## Next Steps

1. **Immediate**: Fix debug statements in summarize block
2. **This Week**: Add test coverage for existing blocks
3. **Next Sprint**: Implement UI improvements for summarize and filter
4. **Future**: Add group_by, rename, and distinct blocks

## Notes
- Multi-input blocks (like join) are lower priority per user feedback
- Focus on single-table operations first
- Ensure all blocks work well in dashboard mode with blockr.ui
- Maintain compatibility with blockr.core API