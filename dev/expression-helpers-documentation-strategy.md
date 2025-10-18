# Expression Helpers Documentation Strategy for blockr.dplyr

*Created: October 2025*

## Problem Statement

Users need guidance when writing R expressions in blocks like `mutate`, `summarize`, and `filter`. While the visual UI handles most operations, expression-based blocks provide power and flexibility. The challenge: **How do we help users discover and correctly use R functions without leaving the block interface?**

**Key Use Cases:**
- **mutate block**: lag(), lead(), case_when(), if_else(), string functions
- **summarize block**: n(), mean(), sum(), across(), etc.
- **filter block**: is.na(), between(), %in%, logical operators

## Research Findings: What Commercial Tools Do

### 1. Excel Formula AutoComplete
**Approach:**
- Dynamic dropdown appears while typing
- Icons indicate entry type (function, reference, etc.)
- Detailed ScreenTips show on hover
- Tab key inserts selection
- ALT+DOWN ARROW opens dropdown manually

**Pros:**
- ✅ Non-intrusive, appears only when needed
- ✅ Very fast - minimal friction
- ✅ Familiar to billions of users

**Cons:**
- ❌ Limited space for detailed help
- ❌ Requires typing to trigger

### 2. Tableau Calculated Fields
**Approach:**
- Auto-complete suggestions as you type
- **Function reference panel** on right side of editor
  - Lists all available functions
  - Each shows: syntax, description, example
  - Double-click to insert
- Drag-and-drop fields from data pane
- Comprehensive web documentation

**Pros:**
- ✅ Persistent reference panel = always visible
- ✅ Examples show actual usage
- ✅ Don't need to know function name to discover it

**Cons:**
- ❌ Takes up screen space
- ❌ Can be overwhelming for beginners

### 3. Power BI DAX Intellisense
**Approach:**
- Autocomplete dropdown for functions/tables/columns
- Hover over function in dropdown for brief explanation
- Works mid-formula with nested functions
- Separate comprehensive documentation website

**Pros:**
- ✅ Contextual - only shows relevant functions
- ✅ Hover help doesn't require clicking away

**Cons:**
- ❌ Users report reliability issues
- ❌ Brief explanations sometimes insufficient

### 4. Shiny Tooltips & Popovers (bslib)
**Approach:**
- **Tooltips**: Hover to show brief help (1-2 sentences)
- **Popovers**: Click to show detailed help (paragraphs, examples)
- Non-blocking - can interact with both popover and UI
- Modern bslib package (Bootstrap 5)

**Pros:**
- ✅ Progressive disclosure - brief to detailed
- ✅ Non-blocking interaction
- ✅ Native Shiny integration

**Cons:**
- ❌ Cannot contain Shiny inputs/outputs
- ❌ Requires manual setup for each help item

## FINAL RECOMMENDED STRATEGY (Three-Part System)

Based on discussion, we'll implement a streamlined three-part help system:

### Part 1: ACE Editor Autocomplete with Enhanced Function List
**Status:** ✅ Partially implemented, needs enhancement

**What it does:**
- Column name suggestions (already working)
- Function name autocomplete
- Context-aware suggestions based on block type

**Enhancement needed:**
- **Expand function catalog** in ACE autocomplete for each block type:
  - **mutate block:** lag(), lead(), cumsum(), cummean(), if_else(), case_when(), coalesce(), str_c(), str_sub(), str_detect(), str_replace(), str_to_lower(), str_to_upper(), year(), month(), day(), round(), floor(), ceiling(), abs()
  - **summarize block:** n(), sum(), mean(), median(), min(), max(), sd(), n_distinct(), first(), last(), across()
  - **filter block:** is.na(), between(), %in%, str_detect(), str_starts(), str_ends()
- Show function category in autocomplete (e.g., "Window function", "String function", "Aggregation")
- Brief syntax hint in autocomplete tooltip

**Implementation:**
- Update `initialize_ace_editor()` calls in mod_vexpr, mod_kvexpr modules
- Add function metadata to autocomplete completer

**Priority:** HIGH (for CRAN v1.0)

---

### Part 2: Inline Help Link with Tooltip
**Approach:** Add help icons next to block labels

**Example:**
```
Expression [ⓘ]  ← Hover shows: "Write R expressions to create new columns.
                    Common functions: lag(), lead(), case_when(), if_else()"
```

**Implementation:**
- Use bslib tooltip() function
- Add to label text in UI
- Brief (1-2 sentences)
- Link to full documentation

**Pros:**
- ✅ Very easy to implement
- ✅ Non-intrusive
- ✅ Always visible (help icon)

**Cons:**
- ❌ Limited detail possible in tooltip
- ❌ Only helps if user hovers

**Priority:** HIGH (easy win for CRAN v1.0)

---

### Layer 4: pkgdown Website with Expression Cookbook (ESSENTIAL)
**Approach:** Create comprehensive online documentation

**Structure:**
```
blockr.dplyr Website
├── Home (README)
├── Reference (function docs)
├── Articles (vignettes)
│   ├── Getting Started
│   ├── Expression Helpers Guide ★
│   └── Advanced Patterns
└── News
```

**"Expression Helpers Guide" Vignette Contents:**

1. **Introduction**
   - When to use expressions vs visual UI
   - How to get help within blocks

2. **mutate Block Cookbook**
   - Window functions (lag, lead, cumsum)
   - Conditional logic (if_else, case_when)
   - String manipulation
   - Date/time operations
   - Examples for each with screenshots

3. **summarize Block Cookbook**
   - Basic aggregations (n, mean, sum)
   - Using across() for multiple columns
   - Grouped summaries with .by argument
   - Examples with screenshots

4. **filter Block Cookbook**
   - Missing value filters
   - String pattern matching
   - Complex logical conditions
   - Examples with screenshots

5. **Function Reference Table**
   - Alphabetical list of useful functions
   - Quick syntax reference
   - Links to full R documentation

**Implementation:**
- Use `usethis::use_pkgdown()` to set up
- Use `usethis::use_pkgdown_github_pages()` for auto-deployment
- Create vignette with `usethis::use_vignette("expression-helpers")`
- Include screenshots of blocks using functions

**Pros:**
- ✅ Comprehensive - can include detailed examples
- ✅ Searchable by Google
- ✅ Easy to link to from in-app help
- ✅ Standard R package practice
- ✅ Auto-deploys via GitHub Actions

**Cons:**
- ❌ Requires leaving the app
- ❌ Need to maintain documentation

**Priority:** ESSENTIAL (must have for CRAN submission)

**Answer to your question:** YES, pkgdown is still THE standard (used by 12,000+ packages). No real alternatives. Bootstrap 5, auto-deploys with GitHub Actions, easy to customize.

---

## Final Three-Part Strategy Summary

| Part | What | Implementation Effort | User Experience |
|------|------|---------------------|----------------|
| **1. Enhanced Autocomplete** | Expanded function catalog in ACE editor with categories and syntax hints | 2-4 hours | Immediate, in-app, fast |
| **2. Inline Help Link** | Small link/icon next to expression with tooltip, opens vignette | 30 minutes | One click to full guide |
| **3. pkgdown Vignette** | Comprehensive Expression Helpers guide with examples | 4-6 hours | Detailed, searchable, examples |

**Total implementation time: ~7-11 hours**

---

## Implementation Plan for CRAN v1.0

### Step 1: pkgdown Website Setup (1-2 hours)

```r
# Run these in package root directory
usethis::use_pkgdown()
usethis::use_pkgdown_github_pages()
```

This will:
- Create `_pkgdown.yml` configuration file
- Set up GitHub Actions for auto-deployment
- Create basic website structure

**Customize `_pkgdown.yml`:**
```yaml
url: https://yourorg.github.io/blockr.dplyr/
template:
  bootstrap: 5

articles:
- title: Guides
  navbar: ~
  contents:
  - expression-helpers

reference:
- title: Transform Blocks
  contents:
  - has_concept("transform")
```

### Step 2: Expression Helpers Vignette (4-6 hours)

```r
usethis::use_vignette("expression-helpers",
  title = "Expression Helpers Guide"
)
```

See detailed content outline in section below.

### Step 3: Enhanced ACE Autocomplete (2-4 hours)

Update ACE editor initialization to include full function catalog:

```r
# In ace_utils.R or similar
initialize_ace_editor <- function(session, editor_id, column_names,
                                  block_type = "mutate") {
  # Get functions for this block type
  functions <- get_functions_for_block_type(block_type)

  # Build autocomplete completer
  completer_data <- list(
    columns = column_names,
    functions = functions
  )

  # Send to JavaScript
  session$sendCustomMessage("initializeAceEditor", list(
    editor_id = editor_id,
    completer_data = completer_data
  ))
}

get_functions_for_block_type <- function(block_type) {
  switch(block_type,
    "mutate" = list(
      list(name = "lag", category = "Window", syntax = "lag(x, n = 1)"),
      list(name = "lead", category = "Window", syntax = "lead(x, n = 1)"),
      list(name = "cumsum", category = "Window", syntax = "cumsum(x)"),
      list(name = "if_else", category = "Conditional", syntax = "if_else(condition, true, false)"),
      # ... more functions
    ),
    "summarize" = list(
      list(name = "n", category = "Aggregation", syntax = "n()"),
      list(name = "mean", category = "Aggregation", syntax = "mean(x, na.rm = FALSE)"),
      # ... more functions
    ),
    "filter" = list(
      list(name = "is.na", category = "Missing", syntax = "is.na(x)"),
      list(name = "between", category = "Comparison", syntax = "between(x, left, right)"),
      # ... more functions
    )
  )
}
```

### Step 4: Inline Help Link (30 minutes)

Add to expression block UI (e.g., in mutate block):

```r
function(id) {
  ns <- NS(id)

  div(
    class = "expression-block-container",

    # Expression editor with help link
    div(
      class = "expression-with-help",
      div(
        class = "expression-label",
        "Expression:",
        a(
          href = "https://yourorg.github.io/blockr.dplyr/articles/expression-helpers.html",
          target = "_blank",
          icon("circle-question"),
          class = "expression-help-link"
        ) %>%
          bslib::tooltip("Common functions: lag(), lead(), case_when(). Click for full guide.")
      ),
      # ACE editor here
      setup_ace_editor(ns("expression"))
    )
  )
}
```

**CSS:**
```css
.expression-with-help {
  position: relative;
}

.expression-label {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
}

.expression-help-link {
  color: #6c757d;
  font-size: 0.9rem;
  text-decoration: none;
}

.expression-help-link:hover {
  color: #0d6efd;
}
```

**Total effort:** ~7-11 hours

---

## Example: How It Works for Users

### Scenario: User wants to calculate year-over-year change in mutate block

**Discovery Path:**

1. **User opens mutate block and starts typing**
   - Types "la" in expression editor
   - ACE autocomplete shows:
     ```
     lag    [Window] lag(x, n = 1)
     last   [dplyr]  last(x)
     ```
   - Selects lag() from autocomplete

2. **User wants more information**
   - Sees help link (📘 or "?") next to expression editor
   - Hovers over it → tooltip: "Common functions: lag(), lead(), case_when(). Click for full guide."
   - Clicks link

3. **pkgdown vignette opens in new tab**
   - Scrolls to "mutate Block: Window Functions"
   - Sees comprehensive lag() example:
     ```r
     # Calculate month-over-month change
     mutate(
       previous_sales = lag(sales, 1),
       change = sales - previous_sales
     )
     ```
   - Screenshot shows the block in action

4. **User returns to app and writes correct expression**
   - Uses example from vignette as template
   - Autocomplete helps with syntax

**Result:** User successfully learns and uses lag() with minimal friction.

---

## Content Outline: Expression Helpers Vignette

```markdown
# Expression Helpers Guide

## Introduction
- When to use expressions vs visual blocks
- How to get help (in-app + documentation)

## mutate Block: Creating New Columns

### Window Functions (Access Other Rows)
- lag() - Get previous row value
  - Example: Calculate month-over-month change
- lead() - Get next row value
  - Example: Look ahead to next period
- cumsum() - Running total
  - Example: Year-to-date sales
- cummean() - Running average
  - Example: Moving average

### Conditional Logic (If-Then-Else)
- if_else() - Simple two-way choice
  - Example: Flag high/low values
- case_when() - Multiple conditions
  - Example: Categorize into bins
- coalesce() - First non-NA value
  - Example: Fill missing values from backup column

### String Manipulation
- str_c() - Concatenate strings
  - Example: Combine first and last name
- str_sub() - Extract substring
  - Example: Get first 3 characters
- str_detect() - Pattern matching
  - Example: Find rows containing keyword
- str_to_lower() / str_to_upper() - Change case
  - Example: Normalize text

### Date/Time Operations
- year(), month(), day() - Extract components
  - Example: Get fiscal year
- today(), now() - Current date/time
  - Example: Calculate days since event

## summarize Block: Aggregating Data

### Basic Aggregations
- n() - Count rows
  - Example: Count by group
- mean(), median() - Central tendency
  - Example: Average by category
- sum() - Total
  - Example: Total sales by region
- min(), max() - Extremes
  - Example: Best/worst performance

### Advanced Aggregations
- across() - Apply function to multiple columns
  - Example: Mean of all numeric columns
- n_distinct() - Count unique values
  - Example: Number of unique customers

### Using .by argument
- Group without group_by/ungroup
  - Example: Summary by category inline

## filter Block: Selecting Rows

### Missing Values
- is.na() - Check if missing
  - Example: Find incomplete records
- !is.na() - Check if present
  - Example: Exclude missing

### Comparisons
- between() - Value in range
  - Example: Sales between $100-$500
- %in% - Value in set
  - Example: Filter to specific states

### String Matching
- str_detect() - Contains pattern
  - Example: Find all product names with "Pro"
- str_starts() / str_ends() - Position-specific
  - Example: Codes starting with "A"

## Function Reference Table

| Function | Block | Description | Example |
|----------|-------|-------------|---------|
| lag() | mutate | Previous value | lag(sales, 1) |
| lead() | mutate | Next value | lead(sales, 1) |
| ... | ... | ... | ... |

## Getting More Help

- In-app: Click ⓘ or "Quick Reference" in blocks
- R documentation: ?function_name
- blockr.dplyr website: [link]
- tidyverse documentation: [link]
```

---

## Technical Implementation Notes

### Quick Reference Drawer Component

```r
# Module for function quick reference
mod_function_quick_ref_ui <- function(id, block_type = "mutate") {
  ns <- NS(id)

  # Different function lists based on block type
  functions_list <- get_functions_for_block(block_type)

  div(
    class = "function-quick-ref",
    div(
      class = "quick-ref-toggle",
      actionButton(ns("toggle"), "Quick Reference", icon = icon("book"))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("toggle")),
      div(
        class = "quick-ref-drawer",
        h5("Function Reference"),
        lapply(names(functions_list), function(category) {
          div(
            class = "function-category",
            h6(category),
            lapply(functions_list[[category]], function(func) {
              actionLink(
                ns(paste0("func_", func$name)),
                func$name,
                icon = icon("circle-info")
              ) %>%
                bslib::popover(
                  title = func$name,
                  content = HTML(sprintf("
                    <strong>Syntax:</strong> %s<br>
                    <strong>Description:</strong> %s<br>
                    <strong>Example:</strong> <code>%s</code><br>
                    <a href='%s' target='_blank'>Full docs →</a>
                  ", func$syntax, func$description, func$example, func$docs_url))
                )
            })
          )
        })
      )
    )
  )
}

get_functions_for_block <- function(block_type) {
  if (block_type == "mutate") {
    list(
      "Window Functions" = list(
        list(name = "lag", syntax = "lag(x, n = 1)",
             description = "Get value from previous row",
             example = "lag(sales, 1)",
             docs_url = "https://pkgdown.site/reference/lag.html"),
        # ... more functions
      ),
      "Conditional Logic" = list(
        # ... functions
      )
    )
  }
  # ... other block types
}
```

### CSS for Drawer

```css
.function-quick-ref {
  position: relative;
}

.quick-ref-drawer {
  position: absolute;
  right: 0;
  top: 40px;
  width: 300px;
  max-height: 500px;
  overflow-y: auto;
  background: white;
  border: 1px solid #dee2e6;
  border-radius: 4px;
  padding: 1rem;
  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  z-index: 1000;
}

.function-category {
  margin-bottom: 1rem;
}

.function-category h6 {
  font-size: 0.875rem;
  color: #6c757d;
  text-transform: uppercase;
  margin-bottom: 0.5rem;
}

.function-category a {
  display: block;
  padding: 0.25rem 0;
  font-size: 0.9rem;
}
```

---

## Success Metrics

**User Success:**
- Can users find the functions they need?
- Do users successfully write working expressions?
- Reduction in error messages from malformed expressions?

**Documentation Success:**
- Website traffic to expression helpers vignette
- Time spent on documentation pages
- Search queries leading to documentation

**Development Success:**
- Time to add new function documentation
- Ease of maintaining function catalog
- User feedback/requests for additional help

---

## Conclusion

**Final Three-Part Strategy:**

1. ✅ **Enhanced ACE Autocomplete** - Expanded function catalog with categories and syntax
2. ✅ **Inline Help Link** - Small link/icon next to expression that opens vignette
3. ✅ **pkgdown Vignette** - Comprehensive Expression Helpers guide

**Total implementation: ~7-11 hours**

This streamlined approach:
- ✅ Provides immediate in-app help (autocomplete)
- ✅ One-click access to comprehensive guide (help link)
- ✅ Detailed examples and cookbook (vignette)
- ✅ Minimal UI clutter
- ✅ Easy to maintain
- ✅ Follows R ecosystem standards

**The strategy balances:**
- Discoverability (autocomplete shows what's available)
- Speed (autocomplete is instant)
- Depth (vignette has all details)
- Simplicity (no complex UI components)

**Next Steps for CRAN v1.0:**
1. Set up pkgdown with GitHub Pages
2. Write Expression Helpers vignette with cookbooks
3. Enhance ACE autocomplete with full function catalog
4. Add help link/icon next to expression editors
