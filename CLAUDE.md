# blockr.dplyr Development Guide

## Overview
This document provides guidance for improving the `blockr.dplyr` package, which provides dplyr-powered data wrangling blocks for blockr.core.

## Current State

### Existing Blocks
- **select** - Column selection
- **filter** - Row filtering with conditions  
- **mutate** - Add/modify columns
- **arrange** - Row ordering
- **summarize** - Group summaries with `.by` parameter for grouping
- **join** - Combining tables (lower priority)

### Current Limitations
1. The mutate and summarize blocks only support single expression input through the UI
2. Debug print statements exist in the code
3. Default values could be more helpful
4. Test coverage is incomplete

## Key Insight
The backend parsing functions (`parse_mutate`, `parse_summarize`) already support multiple expressions. The limitation is only in the UI layer which uses `mod_kvexpr_*` modules that handle single key-value pairs.

## Priority 1: Multiple Expression Support for Mutate Block

### Goal
Enable users to add/remove multiple expressions dynamically in the mutate block, similar to how one might write:
```r
dplyr::mutate(data,
  col1 = expression1,
  col2 = expression2,
  col3 = expression3
)
```

### Concepts to Consider
- Users should be able to start with one or more expressions
- Add button to create new expression rows
- Remove button on each row (but keep at least one)
- Each expression needs a name and a value
- ACE editor integration for syntax highlighting and autocompletion

### Historical Context
The package previously had multi-expression support (see git commit history around commit 56121c6). The earlier implementation used separate modules that were later simplified. You might find inspiration in that code.

### Implementation Ideas
- Create a new module that manages multiple key-value expressions
- Track expressions in a reactive list
- Use dynamic UI rendering to show/hide rows
- Handle the add/remove operations cleanly
- Ensure the returned value is compatible with existing parsing functions

### Things to Keep in Mind
- State must be stored as a list (see issue #16 mentioned in code comments)
- The module should accept both list and named vector inputs for compatibility
- Initialize ACE editors for new rows dynamically
- Consider using indices to track rows to avoid ID conflicts

## Priority 2: Apply Same Pattern to Summarize Block

### Goal
Once mutate supports multiple expressions, apply the same approach to summarize, enabling:
```r
dplyr::summarize(data,
  avg_col = mean(col1),
  sum_col = sum(col2),
  count = n(),
  .by = c("group_col")
)
```

### Additional Considerations for Summarize
- The block already has grouping functionality via the `.by` parameter
- Consider adding common aggregation function templates (mean, sum, count, etc.)
- The UI could benefit from quick-insert buttons for common patterns

## Priority 3: Clean Up and Testing

### Code Quality
- Remove debug print statements
- Improve default values to be more intuitive
- Add comprehensive test coverage
- Ensure backward compatibility with saved states

### Testing Scenarios to Cover
- Single expression
- Multiple expressions
- Adding expressions dynamically
- Removing expressions (with minimum one enforced)
- State persistence (save and restore)
- Generated code validation

## Future Enhancements to Consider

### New Blocks
- **group_by** - Explicit grouping block
- **rename** - Column renaming
- **distinct** - Remove duplicates
- **slice** variants - Row selection by position

### UI/UX Improvements
- Drag-and-drop for reordering expressions
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

### Shiny Reactivity
- Be mindful of reactive dependencies
- Use `isolate()` when needed to prevent circular updates
- Consider using `reactiveVal()` for values that need explicit control

### ACE Editor Integration
- The package uses ACE editors for code input
- Initialize them properly for dynamic content
- Set up autocompletion with column names

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