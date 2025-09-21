# Enhanced Filter Block - Technical Implementation Details

## Core Architecture

### String Parsing and Internal Structure

#### Input String Format
```r
"mpg > 20 & cyl %in% c(4, 6) | gear == 5"
```

#### Simplified Parsed Internal Structure

**Key Simplification**: Store minimal information, derive the rest.

```r
conditions <- list(
  # Condition 1
  list(
    expression = "mpg > 20",        # The R expression (source of truth)
    logical_op = NULL,               # First condition has no operator
    mode = "simple",                 # or "advanced"
    # For simple mode only (parsed from expression):
    column = "mpg",
    values = 20                      # Single value or vector - that's all we need!
  ),

  # Condition 2
  list(
    expression = "cyl %in% c(4, 6)",
    logical_op = "&",                # Connects THIS to PREVIOUS condition
    mode = "simple",
    column = "cyl",
    values = c(4, 6),                # Multiple values automatically means %in%
    include = TRUE                   # Only needed for exclude patterns like !(x %in% y)
  ),

  # Condition 3
  list(
    expression = "gear == 5",
    logical_op = "|",
    mode = "simple",
    column = "gear",
    values = 5                       # Single value - context determines == vs > vs <
  )
)
```

### Why This Simplification Works

1. **No need for `type` field**:
   - Length of `values` tells us if it's single or multiple
   - Column data type tells us if it's numeric or character

2. **No need for `operator` field**:
   - For character/factor: always `%in%` (even for single value)
   - For numeric with length(values) == 1: parse from expression
   - For numeric with length(values) == 2: always range
   - For numeric with length(values) > 2: always `%in%`

3. **Derive operations when building UI**:
```r
get_operation_type <- function(column, values, data) {
  is_numeric <- is.numeric(data[[column]])

  if (!is_numeric) {
    return("select")  # Always multi-select for character/factor
  }

  # Numeric column
  if (length(values) == 2) {
    return("range")   # Use range slider
  } else {
    return("select")  # Use multi-select (even for single numeric)
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
Store only what can't be derived:
- `expression`: The filter expression
- `logical_op`: Connection to previous
- `mode`: Simple or advanced
- `column`, `values`: Parsed from expression for simple mode

### 4. Context-Aware UI
The UI adapts based on:
- Column data type (numeric vs character/factor)
- Number of values (single vs multiple)
- Not based on stored "type" or "operator" fields

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