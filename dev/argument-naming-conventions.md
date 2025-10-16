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

### Consistency over specificity

Use `exprs` consistently instead of `filter_exprs`, `mutate_exprs`, `summary_exprs`:
- One pattern to learn
- Clear that it expects R code
- Block name provides context
- Less verbose

### Preserve what works

Keep:
- `columns` (select, distinct, arrange)
- `renames` (rename)
- `by` (mutate, summarize, slice, join)
- `conditions` (value_filter)

---

## Recommended Changes

| Block | Current | Proposed |
|-------|---------|----------|
| Filter | `string` | `exprs` |
| Mutate | `string` | `exprs` |
| Summarize | `string` | `exprs` |

Keep everything else as-is.

---

## Implementation

1. Update function signatures: `R/filter.R`, `R/mutate.R`, `R/summarize.R`
2. Update internal code
3. Update roxygen examples
4. Update tests
5. Update `inst/examples`
6. Check vignettes/READMEs
7. Update NEWS.md
8. Run R CMD check
9. Manual testing

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

### Use `conditions` for filter

Align with `value_filter_block`.

Rejected: `conditions` suggests structured data (like value_filter's list-of-lists), not plain R expressions. Doesn't solve mutate/summarize inconsistency.

---

## Guidelines for New Blocks

1. R expressions: `exprs` (named or unnamed)
2. Column selection: `columns`
3. Grouping: `by`
4. Mappings: descriptive names (`renames`, etc.)
5. Structured config: descriptive names (`conditions`, etc.)
