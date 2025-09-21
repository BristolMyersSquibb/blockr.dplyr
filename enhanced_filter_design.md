# Enhanced Filter Block Design Document

## Overview

The Enhanced Filter Block provides an intuitive dual-mode interface for filtering data in blockr. It accepts a filter string as input and creates appropriate UI controls based on the data types and filter patterns detected.

## Core Concept

### String-Driven Architecture

The entire filter state is represented by a single R expression string that serves as both input and output:

- **Input**: Initialize the block with a filter expression like `"mpg > 20 & cyl %in% c(4, 6)"`
- **Output**: The block generates a valid filter expression for the framework to evaluate
- **Bidirectional**: UI changes update the string; string changes update the UI

## User Interface Design

### Layout Structure

```
┌─────────────────────────────────────────────┐
│ Condition 1                          [✕]     │
│ Mode: [Simple] [Advanced]            [⇄]     │
│ [Condition-specific UI elements]             │
└─────────────────────────────────────────────┘

        [Logical Operator Dropdown]

┌─────────────────────────────────────────────┐
│ Condition 2                          [✕]     │
│ Mode: [Simple] [Advanced]            [⇄]     │
│ [Condition-specific UI elements]             │
└─────────────────────────────────────────────┘

        [+ Add Condition]
```

### Dual Mode System

#### Simple Mode
Visual interface with intelligent controls based on data type:

**Numeric Columns - Unified Range Slider**
- Single UI element: Dual-handle range slider
- Slider positions determine the filter expression:
  - Both handles at same position: `column == value`
  - Left handle at minimum: `column <= right_value`
  - Right handle at maximum: `column >= left_value`
  - Handles separated: `column >= left & column <= right`
- Covers all numeric comparisons with one intuitive control

**Range Slider Examples:**
```
Input: "mpg > 20"
Slider: [min]----------[20]==========[max]
        (left at 20, right at max)

Input: "mpg == 25"
Slider: [min]----------[25]----------[max]
        (both handles at 25)

Input: "mpg <= 30"
Slider: [min]===========[30]----------[max]
        (left at min, right at 30)

Input: "mpg >= 15 & mpg <= 25"
Slider: [min]----[15]------[25]----[max]
        (handles at 15 and 25)
```

**Character/Factor Columns**
- Membership: Multiple value selection with search
- Exclusion: "Not in" option via exclude toggle
- UI Element: Multi-select dropdown with search

#### Advanced Mode
- Full R expression editor
- Syntax highlighting
- Column name autocompletion
- Function suggestions
- Immediate syntax validation

### Mode Switching Intelligence

**Simple to Advanced**
- Current selections convert to equivalent R expression
- Preserves user intent precisely

**Advanced to Simple**
- Attempts to parse expression into simple components
- Falls back to advanced if expression is too complex
- Clear indication when simplification isn't possible

## Supported Filter Patterns

### Simple Patterns (Visual UI Available)

| Pattern | Example | UI Type |
|---------|---------|---------|
| Any numeric condition | `mpg > 20`, `mpg == 20`, `mpg <= 30` | Dual-handle range slider |
| Numeric range | `hp >= 100 & hp <= 200` | Dual-handle range slider (handles separated) |
| Numeric membership | `gear %in% c(3,4,5)` | Multi-select dropdown |
| Character/Factor membership | `name %in% c("Mazda", "Ford")` | Multi-select dropdown |
| Exclusion | `!(am %in% c(0,1))` | Multi-select + Exclude toggle |

### Complex Patterns (Advanced Mode Only)

| Pattern | Example | Reason |
|---------|---------|--------|
| Functions | `mpg > mean(mpg)` | Contains function call |
| Multiple columns | `mpg/wt > 5` | Expression involves multiple columns |
| Nested logic | `(a > 5 & b < 10) \| c == 3` | Complex logical structure |
| String functions | `grepl("Mazda", name)` | Pattern matching |

## Filter Summary Display (Optional - Future Enhancement)

*Note: Not required for initial implementation*

A future enhancement could add a summary display showing:
- Human-readable descriptions of active filters
- Syntax validation status
- Visual indicators for AND/OR connections

For the initial version, the UI conditions themselves serve as the visual representation of the active filters.

## Technical Architecture

### Initialization Flow

1. **Parse Input String**: Break into individual conditions and operators
2. **Analyze Conditions**: Determine if each can be shown in simple mode
3. **Build Static UI**: All UI elements pre-created, not dynamically rendered
4. **Apply Initial State**: Populate UI with parsed values

### State Management

Each condition maintains:
- Expression string (the source of truth)
- UI mode (simple/advanced)
- Parsed components (column, operator, values)
- Connection operator (AND/OR)

### UI Implementation Strategy

**Static Pre-creation**
- All condition slots (e.g., 10) exist in DOM from start
- Use visibility toggling, not dynamic creation
- Prevents timing issues and race conditions

**Conditional Panels**
- Different UI elements for different operators
- All variations exist, shown/hidden based on selection
- Smooth transitions between UI types

## User Workflows

### Workflow 1: Code-First User
1. Initialize with complex filter expression
2. System parses and displays what it can in simple mode
3. Complex parts remain in advanced mode
4. User can refine visually or via code

### Workflow 2: Visual-First User
1. Start with empty filter or simple expression
2. Use dropdowns and inputs to build conditions
3. Never need to write code
4. Can preview generated expression

### Workflow 3: Hybrid User
1. Build basic conditions visually
2. Add complex condition in advanced mode
3. Switch between modes as needed
4. Combine simple and complex conditions freely

## Validation Approach

### Syntax Validation Only
- Check if expression can be parsed as valid R
- No data evaluation (framework handles that)
- Immediate feedback on syntax errors
- Clear error messages

### What We Don't Validate
- Column existence (data might change)
- Function availability (environment-dependent)
- Result correctness (framework's responsibility)
- Row count impact (requires evaluation)

## Benefits

### For End Users
- **Intuitive**: Visual interface for common operations
- **Powerful**: Full R expressions when needed
- **Flexible**: Mix simple and complex conditions
- **Transparent**: See generated code
- **Safe**: Can't create invalid syntax in simple mode

### For Developers
- **Single Source of Truth**: Filter string drives everything
- **Clean Separation**: UI builds expressions, framework evaluates
- **Predictable**: Static UI prevents timing issues
- **Maintainable**: Clear structure and patterns
- **Extensible**: Easy to add new pattern recognitions

## Design Principles

1. **String as State**: The filter string is the complete state
2. **No Evaluation**: Block only builds expressions, doesn't run them
3. **Static UI**: All UI elements exist from initialization
4. **Smart Defaults**: Appropriate controls based on data types
5. **Progressive Disclosure**: Simple mode for basics, advanced for power users
6. **Clear Feedback**: Immediate validation and helpful messages
7. **Framework Integration**: Respects blockr patterns and conventions
