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
# Enhanced Filter Block - Technical Implementation Details

## Core Architecture

### String Parsing and Internal Structure

#### Input String Format
```r
"mpg > 20 & cyl %in% c(4, 6) | gear == 5"
```

#### Simplified Parsed Internal Structure

**Key Simplification**: Store only what's essential. Parse on-demand.

```r
conditions <- list(
  # Condition 1
  list(
    expression = "mpg > 20",        # The R expression (source of truth)
    logical_op = NULL,               # First condition has no operator
    mode = "simple"                  # or "advanced" for complex expressions
  ),

  # Condition 2
  list(
    expression = "cyl %in% c(4, 6)",
    logical_op = "&",                # Connects THIS to PREVIOUS condition
    mode = "simple"                  # Can be shown in simple UI
  ),

  # Condition 3
  list(
    expression = "gear == 5",
    logical_op = "|",                # OR with previous
    mode = "simple"                  # Simple comparison
  ),

  # Condition 4 - Complex expression
  list(
    expression = "hp / wt > 50",     # Too complex for simple UI
    logical_op = "&",
    mode = "advanced"                # Must use ACE editor
  )
)
```

**Note**: Column names, operators, and values are NOT stored. They are parsed from the expression when needed for the UI using helper functions like `parse_simple_expression()`.

### Why This Simplification Works

1. **Single source of truth**: The expression contains everything
2. **No redundancy**: We don't store what can be parsed
3. **Flexible**: Can handle any valid R expression
4. **Clean**: Only 3 fields instead of 6+

### Parsing When Needed

When we implement the simple UI, we'll parse expressions on-demand:

```r
# Parse simple expression to get column and values
parse_result <- parse_simple_expression("mpg > 20")
# Returns: list(column = "mpg", values = 20)

# Determine UI type based on parsed values and data type
get_ui_type <- function(parsed, data) {
  column <- parsed$column
  values <- parsed$values

  if (!column %in% colnames(data)) return("advanced")

  is_numeric <- is.numeric(data[[column]])

  if (!is_numeric) {
    return("multi_select")  # Character/factor columns
  }

  # Numeric columns
  if (length(values) == 2) {
    return("range_slider")  # Between/range values
  } else {
    return("range_slider")  # Single value comparisons too
  }
}
```

### Range Slider Interpretation (Simplified)

For numeric columns, the dual-handle range slider position determines the expression:

```r
interpret_slider <- function(column, slider_values, data) {
  min_val <- slider_values[1]
  max_val <- slider_values[2]
  data_range <- range(data[[column]], na.rm = TRUE)

  # Tolerance for "at edge" detection (1% of range)
  tol <- diff(data_range) * 0.01

  # Both handles together: equality
  if (abs(max_val - min_val) < tol) {
    return(list(
      expression = glue("{column} == {round(min_val, 2)}"),
      values = round(min_val, 2)
    ))
  }

  # Left at minimum: less than or equal
  if (abs(min_val - data_range[1]) < tol) {
    return(list(
      expression = glue("{column} <= {round(max_val, 2)}"),
      values = round(max_val, 2)
    ))
  }

  # Right at maximum: greater than or equal
  if (abs(max_val - data_range[2]) < tol) {
    return(list(
      expression = glue("{column} >= {round(min_val, 2)}"),
      values = round(min_val, 2)
    ))
  }

  # Both in range: between
  return(list(
    expression = glue("{column} >= {round(min_val, 2)} & {column} <= {round(max_val, 2)}"),
    values = c(round(min_val, 2), round(max_val, 2))
  ))
}
```

### Parsing Expressions to UI Values

When initializing from a string, we need to extract values for the UI:

```r
parse_expression_for_ui <- function(expression) {
  expr <- trimws(expression)

  # Pattern: column %in% c(...) or !(column %in% c(...))
  if (grepl("%in%", expr)) {
    include <- !grepl("^!", expr)
    # Extract column and values...
    return(list(column = column, values = values, include = include))
  }

  # Pattern: column >= X & column <= Y (range)
  if (grepl(">=.*&.*<=", expr)) {
    # Extract column, min, max...
    return(list(column = column, values = c(min_val, max_val)))
  }

  # Pattern: column [operator] value
  if (grepl("(==|>|<|>=|<=)", expr)) {
    # Extract column, operator, value...
    # For >, >=: set slider with left handle at value, right at max
    # For <, <=: set slider with left at min, right handle at value
    # For ==: set both handles at value
    return(list(column = column, values = value))
  }

  # Fallback: can't parse, use advanced mode
  return(list(mode = "advanced"))
}
```

## Key Design Decisions

### 1. Logical Operator Position
Each condition stores the operator that connects it to the PREVIOUS condition:
- First condition: `logical_op = NULL`
- Subsequent: `logical_op = "&"` or `"|"`

This simplifies both UI layout and string building.

### 2. Expression as Source of Truth
The `expression` field is always authoritative. UI values are derived from it when needed, and UI changes generate new expressions.

### 3. Minimal State
Store only what's absolutely necessary:
- `expression`: The filter expression (source of truth)
- `logical_op`: Connection to previous condition
- `mode`: Whether expression can be shown in simple UI

### 4. On-Demand Parsing
- Parse expressions only when building the UI
- Extract column names and values using `parse_simple_expression()`
- Determine UI controls based on parsed results and data types
- No need to keep parsed fields in sync with expression

## UI Rendering Strategy

### Reactive Isolation for Performance
The UI uses selective reactivity to prevent unnecessary re-renders:

**Reactive triggers (cause UI re-render):**
- `r_condition_indices()` - When conditions are added or removed
- `r_cols()` - When available columns change (rare)

**Isolated (don't trigger re-render):**
- `r_conditions()` - Content changes in ACE editors or simple mode
- `r_logic_operators()` - Dropdown value changes
- `r_condition_modes()` - Mode toggle changes

This design ensures:
1. **Stable editing experience** - No UI flicker while typing
2. **Efficient rendering** - Only re-renders for structural changes
3. **Preserved state** - Content updates don't disrupt user interaction

```r
# In renderUI:
output$conditions_ui <- renderUI({
  indices <- r_condition_indices()  # Reactive - triggers on add/remove

  # Isolated - content changes don't trigger re-render
  conditions <- isolate(r_conditions())
  logic_ops <- isolate(r_logic_operators())
  modes <- isolate(r_condition_modes())

  # Build UI based on current state...
})
```

## Implementation Phases

### Phase 1: Foundation with Toggle Buttons
- Copy existing filter block structure
- Replace renderUI with static UI (pre-created slots)
- Add toggle buttons (Simple/Advanced) to each condition
- Keep only ACE editors functional
- Verify filter still works with initial string

### Phase 2: Column Selection
- Add column dropdown in simple mode
- Track column type (numeric/character)
- Update hidden input to trigger conditionalPanel

### Phase 3: Simple Mode UI
- Add range slider for numeric columns
- Add multi-select for character/factor columns
- Include/Exclude radio for %in% operations

### Phase 4: Bidirectional Sync
- Parse expressions to populate simple UI
- Build expressions from simple UI inputs
- Handle mode switching

## Benefits of This Approach

1. **Simpler data structure** - Less fields to keep in sync
2. **More maintainable** - Logic derived from data, not stored
3. **Fewer bugs** - Can't have inconsistent type/operator/values
4. **Cleaner code** - Derivation functions instead of complex state management
5. **Follows blockr patterns** - Static UI, no renderUI