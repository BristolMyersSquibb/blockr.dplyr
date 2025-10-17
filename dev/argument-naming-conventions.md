# Argument Naming Conventions for blockr.dplyr

Analysis of current argument naming patterns and proposed conventions.

Last chance to make breaking changes before the API locks in.

---

## Current State: Complete Argument Inventory

### Blocks with Expression-Based Arguments

#### 1. Filter Block (`new_filter_block`)

**Location:** [R/filter.R:39](../R/filter.R#L39)

```r
new_filter_block(string = "TRUE", ...)
```

- **Argument:** `string`
- **Type:** Character vector of filter conditions (can be multiple)
- **Value Type:** R expression(s) as strings
- **Named:** No (just a vector/list of expressions)
- **Example:** `"mpg > 20"` or `list("mpg > 20", "cyl == 6")`
- **Used for:** Filtering rows by logical conditions

#### 2. Mutate Block (`new_mutate_block`)

**Location:** [R/mutate.R:26](../R/mutate.R#L26)

```r
new_mutate_block(string = list(new_col = "1"), by = character(), ...)
```

- **Argument:** `string`
- **Type:** Named list/vector of mutate expressions
- **Value Type:** Named R expressions
- **Named:** Yes (name = expression pairs)
- **Example:** `list(hp_per_cyl = "hp / cyl", log_mpg = "log(mpg)")`
- **Used for:** Creating/modifying columns

#### 3. Summarize Block (`new_summarize_block`)

**Location:** [R/summarize.R:74](../R/summarize.R#L74)

```r
new_summarize_block(
  string = list(count = "dplyr::n()"),
  by = character(),
  unpack = FALSE,
  ...
)
```

- **Argument:** `string`
- **Type:** Named list/vector of summary expressions
- **Value Type:** Named R expressions
- **Named:** Yes (name = expression pairs)
- **Example:** `list(avg_mpg = "mean(mpg)", total = "n()")`
- **Used for:** Aggregating data

#### 4. Rename Block (`new_rename_block`)

**Location:** [R/rename.R:42](../R/rename.R#L42)

```r
new_rename_block(renames = list(new_col = character()), ...)
```

- **Argument:** `renames`
- **Type:** Named list/vector (new_name = old_name)
- **Value Type:** Named mappings (not expressions!)
- **Named:** Yes (new = old pairs)
- **Example:** `list(miles_per_gallon = "mpg", cylinders = "cyl")`
- **Used for:** Renaming columns

### Blocks with Structured Configuration

#### 5. Value Filter Block (`new_value_filter_block`)

**Location:** [R/value_filter.R:48](../R/value_filter.R#L48)

```r
new_value_filter_block(
  conditions = list(),
  preserve_order = FALSE,
  ...
)
```

- **Argument:** `conditions`
- **Type:** List of filter condition specifications
- **Value Type:** Structured list objects (column, values, mode, operator)
- **Named:** No (but each element is a structured object)
- **Example:**
  ```r
  list(
    list(column = "cyl", values = c(4, 6), mode = "include"),
    list(column = "mpg", values = c(20, 30), mode = "exclude")
  )
  ```
- **Used for:** UI-based filtering with value selection (not R expressions)

### Blocks with Simple Selection Arguments

#### 6. Select Block (`new_select_block`)

**Location:** [R/select.R:12](../R/select.R#L12)

```r
new_select_block(
  columns = character(),
  interface = "table",
  show_selected_on_top = TRUE,
  ...
)
```

- **Argument:** `columns`
- **Type:** Character vector
- **Value Type:** Column names
- **Named:** No
- **Example:** `c("mpg", "cyl", "hp")`

#### 7. Distinct Block (`new_distinct_block`)

**Location:** [R/distinct.R:15](../R/distinct.R#L15)

```r
new_distinct_block(
  columns = character(),
  .keep_all = TRUE,
  ...
)
```

- **Argument:** `columns`
- **Type:** Character vector
- **Value Type:** Column names
- **Named:** No
- **Example:** `c("cyl", "gear")`

#### 8. Arrange Block (`new_arrange_block`)

**Location:** [R/arrange.R:11](../R/arrange.R#L11)

```r
new_arrange_block(columns = character(), ...)
```

- **Argument:** `columns`
- **Type:** Character vector OR list of specifications
- **Value Type:** Column names or column/direction pairs
- **Named:** No (but can contain structure)
- **Example:** `c("mpg", "cyl")` or `list(list(column = "mpg", direction = "desc"))`

### Blocks with Mixed Arguments

#### 9. Slice Block (`new_slice_block`)

**Location:** [R/slice.R:39](../R/slice.R#L39)

```r
new_slice_block(
  type = "head",
  n = 5,
  prop = 0.1,
  order_by = character(),
  with_ties = TRUE,
  weight_by = character(),
  replace = FALSE,
  rows = "1:5",    # <-- This is expression-like
  by = character(),
  ...
)
```

- **Main argument:** `rows` (for custom slice)
- **Type:** Character string
- **Value Type:** R expression for row positions
- **Named:** No
- **Example:** `"1:5"`, `"c(1, 3, 5)"`, `"-c(2, 4)"`

#### 10. Join Block (`new_join_block`)

**Location:** [R/join.R:13](../R/join.R#L13)

```r
new_join_block(
  type = character(),
  by = character(),
  ...
)
```

- **Argument:** `by`
- **Type:** Character vector or named list
- **Value Type:** Column names or column mappings
- **Named:** Sometimes (for cross-column joins)
- **Example:** `c("id")` or `c(left_id = "right_id")`

#### 11. Bind Blocks (`new_bind_rows_block`, `new_bind_cols_block`)

**Location:** [R/bind.R:30](../R/bind.R#L30)

```r
new_bind_rows_block(id_name = "", ...)
new_bind_cols_block(...)
```

- **Argument:** `id_name`
- **Type:** Character string
- **Value Type:** Column name for source identification
- **Named:** No
- **Example:** `".id"` or `"source"`

---

## Naming Patterns in blockr.core

blockr.core blocks follow a simple principle: **match the underlying R function's argument names**.

| Block | Function | blockr.core Args | Pattern |
|-------|----------|------------------|---------|
| subset_block | `subset()` | `subset`, `select` | Matches function params |
| merge_block | `merge()` | `by`, `all_x`, `all_y` | Matches function params |
| head_block | `head()` | `n`, `direction` | Matches + semantic extension |
| scatter_block | `plot()` | `x`, `y` | Matches function params |
| glue_block | `glue()` | `text` | Semantic name for template |
| dataset_block | N/A | `dataset`, `package` | Domain-specific |

**Key insight**: When wrapping an R function, use its argument names whenever possible.

### Why This Matters for blockr.dplyr

dplyr functions use `...` for expressions, which can't be directly replicated as an argument name. We need an alternative that:
1. Is consistent across blocks
2. Indicates R expressions/code
3. Follows R conventions

---

## Problems

### `string` is overloaded

| Block | Argument | Type | Named? | Purpose |
|-------|----------|------|--------|---------|
| Filter | `string` | Character vector | No | Filter conditions |
| Mutate | `string` | Named list/vector | Yes | Column transformations |
| Summarize | `string` | Named list/vector | Yes | Aggregation expressions |

Issues:
- Not descriptive
- Often a list, not a string
- Inconsistent semantics
- Hard to remember which blocks need names

### Inconsistency with value_filter

`value_filter_block` uses `conditions` while `filter_block` uses `string`. Why?

### What works

- `columns` - Consistently used across select, distinct, arrange
- `renames` - Descriptive
- `by` - Follows dplyr convention
- `conditions` (in value_filter) - Clear

---

## Proposed Convention

### Semantic Categories

#### A: R Expressions (Unnamed)
**Use:** `exprs`

Blocks: Filter

```r
exprs = list("mpg > 20", "cyl == 6")
```

#### B: R Expressions (Named)
**Use:** `exprs`

Blocks: Mutate, Summarize

```r
exprs = list(hp_per_cyl = "hp / cyl")
exprs = list(avg_mpg = "mean(mpg)")
```

#### C: Mappings
**Use:** Specific names (`renames`, `by`)

Blocks: Rename, Join

```r
renames = list(new_name = "old_name")
by = c("id") or c(left_col = "right_col")
```

#### D: Simple Selection
**Use:** `columns`

Blocks: Select, Distinct, Arrange

```r
columns = c("mpg", "cyl")
```

#### E: Structured Config
**Use:** `conditions` or descriptive names

Blocks: Value Filter

```r
conditions = list(list(column = "cyl", values = c(4, 6)))
```

### Plural vs Singular

All these arguments accept **lists or vectors of multiple items**, so they should be **plural**:

| Argument | Data Structure | Singular? | Plural? |
|----------|----------------|-----------|---------|
| filter exprs | `list("mpg > 20", "cyl == 6")` | `expr` ❌ | `exprs` ✓ |
| mutate exprs | `list(hp_per_cyl = "hp / cyl", ...)` | `expr` ❌ | `exprs` ✓ |
| summarize exprs | `list(avg_mpg = "mean(mpg)", ...)` | `expr` ❌ | `exprs` ✓ |
| **renames** | `list(new_name = "old_name", ...)` | `rename` ❌ | **`renames`** ✓ |

**Consistency principle**: Use plural names for arguments that accept multiple items. This matches `renames` which is already plural.

### Why `exprs` specifically?

1. **Follows rlang convention**: rlang has `exprs()` (plural) for multiple expressions
2. **Shorter than alternatives**: `expressions`, `conditions`, `assignments`
3. **Generic**: Works for unnamed (filter) and named (mutate/summarize) expressions
4. **Clear intent**: Signals R code/expressions
5. **Consistent with `renames`**: Both are plural

### Preserve what works

Keep:
- `columns` (select, distinct, arrange) - plural ✓
- `renames` (rename) - plural ✓
- `by` (mutate, summarize, slice, join) - dplyr convention ✓
- `conditions` (value_filter) - plural, structured data ✓

---

## Recommended Changes

| Block | Current | Proposed |
|-------|---------|----------|
| Filter | `string` | `exprs` |
| Mutate | `string` | `exprs` |
| Summarize | `string` | `exprs` |

Keep everything else as-is.

---

## blockr.core Constraint: State Must Match Constructor Args

From [blockr.core/R/block-class.R:73-75](../blockr.core/R/block-class.R#L73-L75):

> **State component names are required to match block constructor arguments**

When a block is saved and restored, blockr.core:
1. Extracts the `state` list
2. Calls the constructor with state values as arguments
3. **Expects argument names to match exactly**

This means when renaming `string` → `exprs`, we must update BOTH:

```r
# Constructor signature
new_filter_block(exprs = "TRUE", ...)  # was: string = "TRUE"

# State component (inside server function)
state = list(
  exprs = r_exprs_validated  # was: string = r_string_validated
)
```

They must match because blockr.core does:
```r
# Pseudocode for restoring a block
state <- list(exprs = "mpg > 20")
do.call(new_filter_block, state)  # Calls: new_filter_block(exprs = "mpg > 20")
```

---

## Implementation

1. Update function signatures: `R/filter.R`, `R/mutate.R`, `R/summarize.R`
2. Update state components in server functions (must match constructor args!)
3. Update internal code using these arguments
4. Update roxygen examples
5. Update tests
6. Update `inst/examples`
7. Check vignettes/READMEs
8. Update NEWS.md
9. Run R CMD check
10. Manual testing

---

## Examples

### Filter

```r
# Before
new_filter_block(string = "mpg > 20")

# After
new_filter_block(exprs = "mpg > 20")
```

### Mutate

```r
# Before
new_mutate_block(string = list(hp_per_cyl = "hp / cyl"))

# After
new_mutate_block(exprs = list(hp_per_cyl = "hp / cyl"))
```

### Summarize

```r
# Before
new_summarize_block(string = list(avg_mpg = "mean(mpg)"), by = "cyl")

# After
new_summarize_block(exprs = list(avg_mpg = "mean(mpg)"), by = "cyl")
```

---

## Alternatives Considered

### Use `expr` (singular) instead of `exprs`

Following rlang's `expr()` function for single expressions.

**Rejected**:
- Inconsistent with `renames` (plural)
- All these arguments accept multiple items
- rlang actually has `exprs()` (plural) for multiple expressions

### Use `conditions` for filter

Align with `value_filter_block`.

**Rejected**:
- `conditions` in value_filter is structured data (list-of-lists with column/values/mode)
- filter uses plain R expressions, not structured conditions
- Doesn't solve mutate/summarize inconsistency
- Less clear that it expects R code

### Use semantic names per block

- Filter: `predicates` or `where`
- Mutate: `assignments` or `transformations`
- Summarize: `aggregations` or `summaries`

**Rejected**:
- Too verbose
- Adds cognitive load (3 different names to remember)
- Generic `exprs` works fine for all

### Match dplyr exactly with `...`

Not possible - `...` is reserved and already used by `new_block()`.

---

## Guidelines for New Blocks

When creating new blocks:

1. **Match underlying function params when possible** (blockr.core pattern)
2. **R expressions**: `exprs` (plural, named or unnamed)
3. **Column selection**: `columns` (plural)
4. **Grouping**: `by` (dplyr convention)
5. **Mappings**: descriptive plural names (`renames`, `joins`, etc.)
6. **Structured config**: descriptive plural names (`conditions`, `rules`, etc.)
7. **Use plurals** for arguments accepting lists/vectors of items

---

## Design Rationale Summary

The proposed `exprs` argument name:
- ✓ Shorter and clearer than `string`
- ✓ Technically accurate (list, not string)
- ✓ Consistent across filter/mutate/summarize
- ✓ Plural like `renames`, `columns`, `conditions`
- ✓ Follows rlang convention (`exprs()`)
- ✓ Signals R code/expressions clearly
- ✓ Generic enough for different expression types
