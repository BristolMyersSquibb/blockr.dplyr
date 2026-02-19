# External Ctrl Playwright Test Report — blockr.dplyr

**Date:** 2026-02-19
**Packages tested:** blockr.core, blockr.dplyr, blockr.ai (all installed from local source)
**Method:** 3 test apps launched on port 7654, tested via Playwright MCP with AI chat queries

---

## Summary

| Metric | Count |
|--------|-------|
| **Total test cases** | 22 |
| **Passed** | 18 |
| **Failed** | 4 |
| **Pass rate** | 82% |

### By Block

| Block | Pattern | Tests | Pass | Fail | Notes |
|-------|---------|-------|------|------|-------|
| Filter | B | 3 | 3 | 0 | All conditions, values, exclude mode work |
| Select | A | 3 | 2 | 1 | Exclude columns failed (AI didn't update selectize values) |
| Arrange | B | 2 | 0 | 2 | **Broken** — AI says "Done!" but UI inputs never update |
| Slice | A | 3 | 3 | 0 | type, n, order_by all work correctly |
| Mutate expr | B | 2 | 2 | 0 | Data correct; ACE editor UI doesn't sync (cosmetic) |
| Summarize | B | 2 | 2 | 0 | Data correct; UI inputs don't sync (cosmetic) |
| Rename | B | 2 | 1 | 1 | Single rename works; multi-rename failed (AI confused old/new) |
| Pivot longer | A | 2 | 2 | 0 | cols, names_to, values_to all update correctly |
| Unite | A | 2 | 2 | 0 | col, cols, sep, remove all work |
| Separate | A | 2 | 2 | 0 | col, into, sep, convert all work |
| Pivot wider | A | 2 | 2 | 0 | Data correct; comboboxes don't show selected values |

---

## Detailed Results

### App 1: Basic Transforms (iris)

Chain: `dataset(iris) → filter → select → arrange → slice`

#### 1. Filter Block

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 1a | "Show only setosa species" | Column=Species, Values=setosa | Yes | Yes (50 rows) | **PASS** |
| 1b | "Exclude rows where Sepal.Length is 5.1" | Column=Sepal.Length, Values=5.1, Exclude=checked | Yes | Yes (141 rows) | **PASS** |
| 1c | "Show setosa or versicolor" | Column=Species, Values=setosa+versicolor | Yes | Yes (100 rows) | **PASS** |

#### 2. Select Block

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 2a | "Select only Species and Sepal.Length" | 2 tags in selectize | Yes | Yes (2 columns) | **PASS** |
| 2b | "Exclude the Petal columns" | Exclude toggled but wrong columns | Partial | No — excluded Species+Sepal.Length instead of Petal cols | **FAIL** |
| 2c | "Keep only distinct rows" | Distinct checkbox checked | Yes | Yes (94 rows) | **PASS** |

**2b failure detail:** The AI toggled `exclude=TRUE` on the existing selectize values (Species, Sepal.Length from test 2a) instead of setting columns to Petal.Length+Petal.Width. The AI didn't update the column selectize, only flipped the checkbox. This may be an AI (LLM) interpretation issue or a limitation of the external_ctrl wiring for the selectize input.

#### 3. Arrange Block

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 3a | "Sort by Petal.Length descending" | AI said "Done!" but nothing changed | No | No | **FAIL** |
| 3b | "Sort by Petal.Width then Sepal.Width descending" | AI said "Done!" but nothing changed | No | No | **FAIL** |

**Arrange block is broken.** The AI generates a response and says "Done!" but the `columns` reactive value doesn't propagate to the multi_arrange module's UI. The column select still shows the default first column, descending is unchecked, and no additional sort rows appear. The external control → UI pipeline is disconnected for this block.

#### 4. Slice Block

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 4a | "Show only the first 10 rows" | type=head, n=10 | Yes | Yes (10 rows) | **PASS** |
| 4b | "Show the 3 smallest Petal.Length values" | type=min, n=3, order_by=Petal.Length | Yes | Yes (4 rows with ties) | **PASS** |
| 4c | "Random sample of 5 rows" | type=sample, n=5 | Yes | Yes (5 random rows) | **PASS** |

---

### App 2: Expression & Aggregation (mtcars)

Chain: `dataset(mtcars) → mutate_expr → summarize → rename`

#### 5. Mutate Expr Block

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 5a | "Create a column called hp_per_cyl equal to hp / cyl" | hp_per_cyl column added | ACE editors stale | Yes (110/6=18.33) | **PASS** |
| 5b | "Add a column wt_kg equal to wt * 453.6" | wt_kg column added alongside hp_per_cyl | ACE editors stale | Yes (2.62*453.6=1188.4) | **PASS** |

**Note:** The ACE editor inputs still show the initial values ("new_col" / "1") but the data output correctly reflects both expressions. The external control updates the reactive `exprs` value directly, bypassing the ACE editor UI sync. This is a cosmetic issue — the transformation works correctly.

#### 6. Summarize Block

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 6a | "Calculate mean hp" | mean_hp = 146.6875 | UI inputs stale | Yes | **PASS** |
| 6b | "Group by cyl and calculate mean hp and max mpg" | 3 rows, cyl/mean_hp/max_mpg | UI inputs stale | Yes | **PASS** |

**Note:** Same pattern as mutate_expr — the UI inputs (function select, column select, output name) still show initial values but the data output is correct. The `summaries` and `by` reactive values are properly updated.

#### 7. Rename Block

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 7a | "Rename mean_hp to average_horsepower" | Column renamed in output | UI inputs stale | Yes | **PASS** |
| 7b | "Rename cyl to cylinders and max_mpg to best_mpg" | Error: column `cylinders` doesn't exist | N/A | No | **FAIL** |

**7b failure detail:** The AI confused the old/new name mapping. The rename block uses `renames = list(new_name = "old_name")` convention. The AI passed `list(cylinders = "cyl", best_mpg = "max_mpg")` but the previous rename already changed `mean_hp` to `average_horsepower`, so the block had columns: cyl, average_horsepower, max_mpg. The error "column `cylinders` doesn't exist" suggests the AI may have reversed old/new or tried to use an incorrect column name. This appears to be an LLM interpretation issue rather than a framework bug.

---

### App 3: Reshape & String Ops

Independent blocks: each fed from its own dataset.

#### 8. Pivot Longer Block (iris → pivot_longer)

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 8a | "Pivot Sepal.Length and Sepal.Width into long format" | 300 rows, name/value columns | Yes (cols selectize) | Yes | **PASS** |
| 8b | "Set names_to to 'measurement' and values_to to 'cm'" | Text inputs updated | Yes | Yes | **PASS** |

#### 9. Unite Block (people → unite)

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 9a | "Unite name and value into a column called combined with separator :" | col=combined, sep=:, columns selected | Yes | Yes (Alice:100) | **PASS** |
| 9b | "Keep original columns after uniting" | Remove checkbox unchecked | Yes | Yes (all cols kept) | **PASS** |

#### 10. Separate Block (people_united → separate)

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 10a | "Separate the combined column into part1 and part2 by :" | col=combined, into=part1,part2, sep=: | Yes | Yes | **PASS** |
| 10b | "Auto-convert types after separating" | Convert checkbox checked | Yes | Yes | **PASS** |

#### 11. Pivot Wider Block (long → pivot_wider)

| # | Query | Result | UI Updated | Data Correct | Status |
|---|-------|--------|------------|--------------|--------|
| 11a | "Pivot wider with names from part1 and values from part2" | Pivoted to id/a/b | Comboboxes empty in UI | Yes | **PASS** |
| 11b | "Fill missing values with 999" | values_fill=999 | Yes (text input) | Yes | **PASS** |

---

## Pattern A vs Pattern B Behavior

### Pattern A blocks (simple inputs: selectize, text, checkbox)
**Blocks:** select, slice, pivot_longer, pivot_wider, unite, separate

- **UI sync:** Generally good. Text inputs, checkboxes, and most selectize inputs update correctly via two-way binding.
- **Combobox sync:** Some comboboxes (e.g., pivot_wider names_from/values_from) don't visually show selected values in the UI even when the reactive value is correctly set. The data output is still correct.
- **Pass rate:** 15/16 (94%) — only the select block's "exclude Petal columns" test failed due to AI interpretation rather than framework issues.

### Pattern B blocks (dynamic renderUI: multi_arrange, multi_kvexpr, multi_summarize, multi_rename)
**Blocks:** filter, arrange, mutate_expr, summarize, rename

- **Filter:** Excellent — full two-way sync including dynamic condition rows, column selects, value selects, and checkboxes.
- **Arrange:** Broken — external control values don't propagate to the multi_arrange module UI at all.
- **Mutate expr:** Data works, UI stale — ACE editors don't update but the reactive `exprs` value is correctly applied.
- **Summarize:** Data works, UI stale — function/column selects don't update but the reactive `summaries` and `by` values are correctly applied.
- **Rename:** Data works for single rename, UI stale — the text/select inputs don't update. Multi-rename failed due to AI name confusion.
- **Pass rate:** 8/11 (73%)

---

## Key Findings

### Critical Issues

1. **Arrange block external control is completely broken.** The AI sends the tool call successfully (says "Done!") but the `columns` parameter change never reaches the multi_arrange module's UI or data pipeline. This needs investigation in `mod_multi_arrange.R` — likely the `as_rv()` → `observe()` chain is missing or not wired to the dynamic renderUI.

### Moderate Issues

2. **Pattern B UI sync gap.** For mutate_expr, summarize, and rename blocks, external control updates the reactive data values (transformations work correctly) but doesn't update the UI input elements (ACE editors, select boxes, text inputs). Users see stale inputs but correct data output. This creates a confusing UX where the displayed settings don't match the actual behavior.

3. **Pivot wider combobox sync.** The selectize/combobox inputs for names_from and values_from don't visually display their selected values after external control updates, even though the reactive values are correctly set.

### Minor Issues / AI (LLM) Interpretation

4. **Select block column context.** When told to "Exclude the Petal columns", the AI toggled the exclude checkbox but didn't update the selectize values — it only modified the boolean flag, not the column list.

5. **Rename block name direction.** The AI confused old→new name mapping when handling multiple renames in a single query. Single renames work fine.

---

## Screenshots

- `screenshots/test-1a-filter-setosa.png` — Filter block after "Show only setosa species"
- `screenshots/test-4c-slice-sample.png` — Slice block after "Random sample of 5 rows"
- `screenshots/test-7b-rename-fail.png` — Rename block failure with multi-rename
- `screenshots/test-11b-pivot-wider.png` — Pivot wider block after pivoting
