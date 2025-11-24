# Naming Scheme Analysis: Finding the Perfect Pattern

## The Core Question

What suffix convention best distinguishes simple (no-code) from advanced (expression-based) blocks?

## Option 1: Base + `_expr` (Original Proposal)

```r
# Simple variants (no suffix)
new_filter_block()
new_summarize_block()
new_mutate_block()

# Advanced variants (with _expr)
new_filter_expr_block()
new_summarize_expr_block()
new_mutate_expr_block()
```

### Arguments For
1. **Base = Simple is intuitive**: In R/tidyverse, base function names typically represent the simpler interface
   - `lm()` vs `lm.fit()` (complex)
   - `read.csv()` vs `read.table()` (complex)
   - `subset()` vs `[` (complex)

2. **Suffix indicates complexity**: Adding a suffix suggests "extra capability" or "advanced mode"
   - `_expr` clearly signals "you need to write expressions"
   - Aligns with user mental model: simple first, advanced as opt-in

3. **Discoverability**: New users naturally find simple variants first
   - Autocomplete shows base name first
   - Documentation lists base variant prominently
   - Reduces barrier to entry

4. **Precedent in blockr ecosystem**:
   - `new_transform_block()` vs specialized variants
   - Base names are typically simpler/more general

5. **Natural language flow**:
   - "I need a filter block" → finds simple one
   - "I need expression-based filtering" → finds `_expr` variant

6. **Shorter names for common case**: Most users start with simple variants
   - `new_filter_block()` - 17 chars
   - `new_filter_expr_block()` - 22 chars

### Arguments Against
1. **Asymmetry**: Simple blocks have no indicator of their nature
   - Doesn't explicitly say "this is the no-code one"
   - Could be confusing which is which

2. **Changing defaults**: Currently `new_summarize_block()` is the expression variant
   - Flips the meaning of the base name
   - Historical inconsistency (though fixable with deprecation)

3. **Ambiguity**: Without reading docs, users might not know base = simple
   - Not self-documenting

## Option 2: Base + `_simple` (User's Alternative)

```r
# Simple variants (with _simple)
new_filter_simple_block()
new_summarize_simple_block()
new_mutate_simple_block()

# Advanced variants (no suffix)
new_filter_block()
new_summarize_block()
new_mutate_block()
```

### Arguments For
1. **Explicit about capabilities**: Clear which is which without docs
   - `_simple` means no-code, dropdown-based
   - Base name means full expression power

2. **Advanced users get shorter names**: Power users type less
   - `new_filter_block()` for the one they use most

3. **Less breaking change**:
   - `new_filter_expr_block()` stays as `new_filter_block()`
   - `new_summarize_block()` stays as-is (only nocode changes)
   - Only 2 renames instead of 3

4. **Simple is opt-in**: Explicit choice to use simplified interface
   - Could be seen as training wheels

### Arguments Against
1. **Contradicts R conventions**: Base names are usually simpler in R
   - Goes against user expectations from other R packages
   - `dplyr::filter()` is simple, not complex

2. **Discoverability problem**: New users find complex variant first
   - Autocomplete shows base name (complex) before `_simple`
   - Higher barrier to entry
   - Could frustrate non-coders

3. **Longer names for beginners**: Target audience types more
   - Non-technical users stuck with longer names
   - `new_filter_simple_block()` - 24 chars

4. **"Simple" sounds condescending**: Negative connotation
   - "I'm using the simple one" feels like lesser choice
   - Could discourage adoption

5. **Doesn't scale well**: What if we add more variants?
   - `new_filter_advanced_block()`?
   - `new_filter_expert_block()`?
   - Naming becomes arbitrary

## Option 3: Base + `_visual` / `_gui`

```r
# Simple variants (visual/GUI-based)
new_filter_visual_block()  # or new_filter_gui_block()
new_summarize_visual_block()
new_mutate_visual_block()

# Advanced variants (code-based)
new_filter_block()
new_summarize_block()
new_mutate_block()
```

### Arguments For
1. **Descriptive of interface**: Accurately describes the interaction model
   - Visual/GUI = point-and-click
   - Base = code/expressions

2. **No value judgment**: Neutral terminology
   - Not "simple" vs "advanced" (hierarchy)
   - Just different interfaces

3. **Aligns with Blockr philosophy**: Visual programming environment
   - Emphasizes the visual aspect

### Arguments Against
1. **Everything is visual**: All blocks have GUIs
   - Not a distinguishing characteristic
   - Expression blocks also have visual interfaces (code editors)

2. **Longer names**: `_visual` adds 7 chars, `_gui` adds 4
   - More typing for target audience

3. **Less clear about capability**: Doesn't indicate no expressions
   - "Visual" could mean anything

## Option 4: Symmetric Suffixes - `_nocode` + `_expr`

```r
# Simple variants (no-code)
new_filter_nocode_block()
new_summarize_nocode_block()
new_mutate_nocode_block()

# Advanced variants (expression-based)
new_filter_expr_block()
new_summarize_expr_block()
new_mutate_expr_block()
```

### Arguments For
1. **Explicit symmetry**: Both variants clearly labeled
   - No ambiguity about which is which
   - Self-documenting

2. **Descriptive**: Accurately describes the distinction
   - `nocode` = no expressions needed
   - `expr` = requires expressions

3. **Already used**: `summarize_nocode` exists
   - Some consistency with current naming

4. **Neutral hierarchy**: Neither is "base", both are equal variants
   - No value judgment

### Arguments Against
1. **Verbose**: Both names are long
   - `new_filter_nocode_block()` - 24 chars
   - No short option for common case

2. **"Nocode" is informal**: Not professional terminology
   - Sounds like marketing speak
   - Could age poorly

3. **Doesn't follow R conventions**: Base names exist in R
   - Unusual to have all suffixed variants

4. **What happens with unified blocks?**: When merging in future
   - Which name survives?
   - Another rename needed

## Option 5: Mode Prefix - `basic_` / `expr_`

```r
# Simple variants
basic_filter_block()
basic_summarize_block()
basic_mutate_block()

# Advanced variants
expr_filter_block()
expr_summarize_block()
expr_mutate_block()
```

### Arguments For
1. **Grouped autocomplete**: Related blocks appear together
   - All basics together, all expr together

2. **Clear distinction**: Prefix highlights the mode

### Arguments Against
1. **Breaks blockr convention**: All blocks start with `new_`
   - Inconsistent with ecosystem
   - Prefixes already have meaning in R (S3 methods)

2. **Less intuitive search**: Users search "filter block" not "basic filter"

3. **Awkward function calls**: Doesn't read naturally
   - `basic_filter_block()` vs `new_filter_block()`

## Comparative Analysis

| Scheme | Simple Name Length | Advanced Name Length | Intuitive? | Follows R Convention? | Beginner Friendly? | Self-Documenting? |
|--------|-------------------|---------------------|------------|----------------------|-------------------|-------------------|
| Base + `_expr` | 17-20 chars | 22-27 chars | ✓✓✓ | ✓✓✓ | ✓✓✓ | ✗ |
| Base + `_simple` | 24-29 chars | 17-20 chars | ✗ | ✗ | ✗✗ | ✓ |
| Base + `_visual` | 24-28 chars | 17-20 chars | ✗ | ✗ | ✗ | ✗ |
| `_nocode` + `_expr` | 24-29 chars | 22-27 chars | ✓ | ✗ | ✓ | ✓✓ |
| Prefix mode | 20-25 chars | 19-24 chars | ✗✗ | ✗✗ | ✗ | ✓ |

## Real-World Usage Examples

### Option 1: Base + `_expr`
```r
# Tutorial: "Let's start with filtering"
new_filter_block()  # Clean, simple

# Advanced tutorial: "Now let's use expressions"
new_filter_expr_block()  # Clear progression

# User code
stack |>
  add_block(new_filter_block()) |>  # 90% of users
  add_block(new_summarize_block())
```

### Option 2: Base + `_simple`
```r
# Tutorial: "For beginners, we have simplified blocks"
new_filter_simple_block()  # Sounds condescending?

# Tutorial: "The standard filter block uses expressions"
new_filter_block()

# User code
stack |>
  add_block(new_filter_simple_block()) |>  # Feels second-class
  add_block(new_summarize_simple_block())
```

### Option 4: `_nocode` + `_expr`
```r
# Tutorial: "We have two approaches"
new_filter_nocode_block()  # Explicit but verbose
new_filter_expr_block()

# User code
stack |>
  add_block(new_filter_nocode_block()) |>  # Lots of typing
  add_block(new_summarize_nocode_block())
```

## Consideration: Future Unified Blocks

When eventually merging variants with mode toggle:

### Base + `_expr` → Unified
```r
# Simple migration path
new_filter_block()  # Stays as name, adds mode parameter
new_filter_block(mode = "simple")   # default
new_filter_block(mode = "expression")

# Old _expr variant deprecated
new_filter_expr_block()  # .Deprecated() → new_filter_block(mode = "expression")
```

**Clean transition**: Base name survives naturally

### Base + `_simple` → Unified
```r
# Awkward migration
new_filter_block(mode = "expression")  # default?
new_filter_block(mode = "simple")

# But then we renamed twice:
# new_filter_block() → new_filter_expr_block() → new_filter_block()
```

**Messy**: Base name meaning changes twice

### `_nocode` + `_expr` → Unified
```r
# Need new name entirely
new_filter_block()  # Neither nocode nor expr survives naturally

# Or pick one:
new_filter_nocode_block(mode = "expression")?  # Contradictory
new_filter_expr_block(mode = "nocode")?  # Contradictory
```

**Problematic**: No natural survivor

## Additional Considerations

### Documentation Patterns
```r
# Option 1: Base + _expr
?filter_block      # Shows simple variant prominently
?filter_expr_block # Advanced variant

# Option 2: Base + _simple
?filter_block      # Shows advanced variant prominently (wrong for beginners)
?filter_simple_block
```

### Package Vignette Structure
Option 1 naturally supports:
```
01-getting-started.Rmd     # Uses new_filter_block(), new_summarize_block()
02-advanced-expressions.Rmd # Introduces _expr variants
```

Option 2 would need:
```
01-getting-started.Rmd     # Uses new_filter_simple_block() (awkward)
02-standard-usage.Rmd      # Uses new_filter_block() (should be first?)
```

### Error Messages & Help Text
```r
# Option 1
"new_filter_block() expected columns: ..."  # Clean
"new_filter_expr_block() expected expression: ..." # Clear specialization

# Option 2
"new_filter_simple_block() expected columns: ..."  # Wordy
"new_filter_block() expected expression: ..." # Standard name for advanced?
```

## Alternative Consideration: No Variants at All?

Could we use parameters instead of separate functions?

```r
new_filter_block(mode = "simple")  # or "visual" or "nocode"
new_filter_block(mode = "expression")  # or "expr" or "code"
```

### Arguments For
- Single function to learn
- No naming confusion
- Easy to add more modes later
- Clean future-proofing

### Arguments Against
- Harder discoverability (need to know about mode parameter)
- More complex implementation (switching logic inside)
- Less clear in code what you're getting
- Breaks blockr pattern of separate block constructors
- Can't have different default parameters for each mode

**Verdict**: Not recommended for now, but good target for future unified blocks

## Recommendation: Base + `_expr`

After thorough analysis, **Option 1 (Base + `_expr`)** is the clear winner:

### Primary Reasons
1. **Follows R conventions**: Base = simple, suffix = specialized
2. **Beginner-friendly**: Shortest names for target audience
3. **Natural progression**: Users graduate from base to `_expr`
4. **Clean autocomplete**: Simple variant appears first
5. **Future-proof**: Cleanest path to unified blocks
6. **No negative connotations**: Unlike "simple" which sounds condescending
7. **Clear specialization**: `_expr` explicitly indicates what's different

### Naming Pattern
```r
# The Pattern:
new_<operation>_block()       # Simple, no-code, visual interface
new_<operation>_expr_block()  # Advanced, expression-based

# Applied:
new_filter_block()            # Simple value selection
new_filter_expr_block()       # Expression-based filtering

new_summarize_block()         # Simple dropdown aggregations
new_summarize_expr_block()    # Expression-based summarization

new_mutate_block()            # Simple (future)
new_mutate_expr_block()       # Expression-based mutation
```

### Why Not `_simple`?
- Puts burden on beginners (longer names)
- Sounds condescending ("I need the simple one")
- Contradicts R conventions
- Poor discoverability
- Awkward future migration path

### Why Not `_nocode` + `_expr`?
- Both names are long (no winner)
- "Nocode" is informal marketing term
- Doesn't follow R conventions (base names exist)
- Complicated future migration

### Implementation Note
This means:
- **`new_filter_expr_block()`** keeps its name ✓
- **`new_value_filter_block()`** → **`new_filter_block()`**
- **`new_summarize_block()`** → **`new_summarize_expr_block()`**
- **`new_summarize_nocode_block()`** → **`new_summarize_block()`**
- **`new_mutate_block()`** → **`new_mutate_expr_block()`**

## Deprecation Helper Function

As you suggested, create a reusable deprecation helper:

```r
#' Show deprecation message in Shiny context
#'
#' @param old_name Character, the deprecated function name
#' @param new_name Character, the replacement function name
#' @param version Character, version when deprecated
#' @keywords internal
show_block_deprecation <- function(old_name, new_name, version = NULL) {
  # Build message
  msg <- sprintf(
    "%s is deprecated. Please use %s instead.",
    old_name, new_name
  )

  if (!is.null(version)) {
    msg <- paste0(msg, sprintf(" (deprecated in version %s)", version))
  }

  # Console warning always
  warning(msg, call. = FALSE)

  # Shiny notification if in Shiny context
  if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
    shiny::showNotification(
      ui = shiny::div(
        shiny::tags$strong("Deprecated Block"),
        shiny::tags$br(),
        sprintf("Using deprecated %s", old_name),
        shiny::tags$br(),
        sprintf("Please switch to: %s", new_name)
      ),
      type = "warning",
      duration = 10,  # Show for 10 seconds
      closeButton = TRUE
    )
  }

  invisible(NULL)
}

# Usage in deprecated function:
new_value_filter_block <- function(...) {
  show_block_deprecation(
    "new_value_filter_block()",
    "new_filter_block()",
    version = "0.5.0"
  )
  new_filter_block(...)
}
```

This helper:
- ✓ Shows console warning (always)
- ✓ Shows Shiny notification (when in Shiny)
- ✓ Gracefully handles non-Shiny contexts (like tests)
- ✓ Reusable across all deprecated blocks
- ✓ Provides clear migration path

## Next Steps

1. **Implement `show_block_deprecation()` helper**
2. **Create new block functions with correct names**
3. **Create deprecated aliases using helper**
4. **Remove old names from registry** (so they don't appear in UI)
5. **Update all documentation** to new names
6. **Update internal code** to use new names
7. **Test deprecation flow** in Shiny apps

The registry removal is genius - existing workflows keep working, but new users only see correct names!
