# BlockrInput: Custom code input with autocomplete replacing ACE editor

## Motivation

The unified blocks (filter, join, summarize, mutate) and multi-expr blocks use
ACE editor for R expression inputs. ACE is a ~300KB full-featured code editor,
but blockr only uses a tiny fraction: a text input with autocomplete and
cursor-inside-parens insertion. A lightweight custom component will reduce bundle
size, remove the shinyAce dependency, and give full control over styling —
same rationale as BlockrSelect replacing selectize.

## Current ACE usage

Across all unified blocks, ACE is used identically:

- **Single-line or ~10-line text input** (monospace, no line numbers, no gutter)
- **Live autocomplete** with categorized completions:
  - `column`: dynamic, backtick-quoted if needed
  - `aggregate`: mean, median, sd, sum, min, max, n, n_distinct, first, last
  - `arithmetic`: abs, sign, ceiling, floor, round, trunc, log, log2, log10, exp, sqrt
  - `offset`: lead, lag, cumsum, cumprod, cummin, cummax
  - `logical`: if_else, case_when, between
  - `string`: str_c, paste, paste0, str_sub, str_to_lower, str_to_upper
  - `ranking`: row_number, min_rank, dense_rank, percent_rank, ntile
  - (Each block uses a subset of these categories)
- **Function insertion**: selecting `mean` inserts `mean()` with cursor between parens
- **Enter key** confirms the expression (custom command, not newline)
- **onChange callback** (marks confirm button as unconfirmed)
- **Dynamic column updates**: `editor.completers = [makeCompleter(newCols)]`
- **Cleanup**: `editor.destroy()`

### Files using ACE

| File | Usage | maxLines | Dynamic column updates |
|------|-------|----------|----------------------|
| `filter-unified-input.js` | expr rows only | 10 | No |
| `join-unified-input.js` | expr rows | 10 | Yes |
| `summarize-unified-input.js` | expr rows | 10 | No (gap) |
| `mutate-unified-input.js` | all rows | 10 | Yes |
| `multi-expr-input.js` | all rows | 1 | Yes |

### R-side dependencies

Each `*_unified_dep()` loads ACE via:
```r
htmlDependency(
  name = "ace",
  version = utils::packageVersion("shinyAce"),
  src = system.file("www", package = "shinyAce"),
  script = c("ace/ace.js", "ace/ext-language_tools.js")
)
```

### JS helpers to replace

Each file has identical copies of:
- `withAce(fn)` — polls for ACE availability
- `makeCompleter(cols)` — builds categorized completion list
- `createAceEditor(container, value, cols, onChangeCallback, opts)` — creates editor

---

## V1: Text input with autocomplete (no syntax highlighting)

### Component API

#### `BlockrInput.create(container, config)`

```javascript
var editor = BlockrInput.create(codeDiv, {
  value: "mean(col_a)",           // initial value
  placeholder: "R expression...", // placeholder text
  columns: ["col_a", "col_b"],   // column names for autocomplete
  categories: {                   // function categories (block-specific subset)
    aggregate: ["mean", "sum"],
    arithmetic: ["abs", "round"]
  },
  multiline: false,               // false = single line, true = expandable
  maxLines: 10,                   // max visible lines (multiline only)
  onChange: function() {},         // called on any text change
  onConfirm: function(value) {},  // called on Enter key
});
// Returns: { el, getValue(), setValue(v), setColumns(cols), destroy() }
```

### Returned instance

| Method | Description |
|--------|-------------|
| `el` | Root DOM element |
| `getValue()` | Returns trimmed string |
| `setValue(v)` | Sets input value |
| `setColumns(cols)` | Updates column completions dynamically |
| `destroy()` | Removes DOM and listeners |

### DOM structure

```html
<div class="blockr-input">
  <input class="blockr-input__field" type="text"
         placeholder="R expression..."
         autocomplete="off" spellcheck="false" />
  <!-- or <textarea> if multiline -->
  <div class="blockr-input__popup" role="listbox">
    <div class="blockr-input__group-label">column</div>
    <div class="blockr-input__item blockr-input__item--highlighted"
         role="option" data-value="col_a" data-is-fn="false">
      <span class="blockr-input__item-text">col_a</span>
      <span class="blockr-input__item-meta">column</span>
    </div>
    <div class="blockr-input__group-label">aggregate</div>
    <div class="blockr-input__item" role="option"
         data-value="mean" data-is-fn="true">
      <span class="blockr-input__item-text">mean</span>
      <span class="blockr-input__item-meta">aggregate</span>
    </div>
    <!-- ... -->
    <div class="blockr-input__empty">No matches</div>
  </div>
</div>
```

### Autocomplete behavior

**Trigger**: On every keystroke, extract the **current token** at cursor position
(word boundary = start of input, or after `(`, `,`, ` `, `+`, `-`, `*`, `/`,
`>`, `<`, `=`, `!`, `&`, `|`, `~`).

**Matching**: Case-insensitive prefix match against all completions (columns +
functions). Show popup if there are matches and the token is non-empty.

**Completion list building**:
```javascript
// Columns first (score 1001), then functions (score 1000)
// Within each group, alphabetical
// Show category label as meta text (right-aligned, muted)
```

**Insertion**:
- **Column**: replace current token with the column name (backtick-quoted if needed)
- **Function**: replace current token with `funcname()`, place cursor between parens

**Popup positioning**: Below the input, aligned to cursor position if possible,
or full-width of the input for simplicity.

### Keyboard navigation

| Key | Popup closed | Popup open |
|-----|-------------|------------|
| Any char | Type + maybe open popup | Type + filter |
| ArrowDown | — | Move highlight down |
| ArrowUp | — | Move highlight up |
| Enter | Fire `onConfirm` | Accept highlighted completion |
| Tab | — | Accept highlighted completion |
| Escape | — | Close popup |

When popup is closed, Enter fires `onConfirm(getValue())` — this replaces
ACE's custom "confirmExpr" command.

### Styling

```css
.blockr-input { position: relative; width: 100%; }
.blockr-input__field {
  width: 100%;
  font-family: 'SF Mono', 'Fira Code', 'Consolas', monospace;
  font-size: 14px;
  border: none;
  background: transparent;
  outline: none;
  padding: 0 4px;
}
.blockr-input__popup {
  /* same dropdown styling as BlockrSelect */
  position: absolute;
  z-index: 9999;
  background: #fff;
  border: 1px solid var(--blockr-color-border, #e5e7eb);
  border-radius: 8px;
  box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  max-height: 200px;
  overflow-y: auto;
}
.blockr-input__item-meta {
  color: var(--blockr-grey-400, #9ca3af);
  font-size: 0.75rem;
  margin-left: auto;
}
```

### Migration pattern

```javascript
// BEFORE (ACE):
var exprEl = createAceEditor(codeDiv, value, this.columnNames,
  function() { confirmBtn.classList.remove("confirmed"); },
  { maxLines: 10 });
// read: exprEl._aceEditor.getValue().trim()
// update: exprEl._aceEditor.completers = [makeCompleter(cols)]
// destroy: exprEl._aceEditor.destroy()

// AFTER (BlockrInput):
var exprInput = BlockrInput.create(codeDiv, {
  value: value,
  columns: this.columnNames,
  categories: defaultCategories,
  onChange: function() { confirmBtn.classList.remove("confirmed"); },
  onConfirm: function(val) { confirmBtn.classList.add("confirmed"); self._autoSubmit(); }
});
// read: exprInput.getValue()
// update: exprInput.setColumns(cols)
// destroy: exprInput.destroy()
```

### Files

#### New files

| File | Purpose |
|------|---------|
| `inst/assets/js/blockr-input.js` | IIFE exposing `BlockrInput.create()` |
| `inst/assets/css/blockr-input.css` | Popup + field styling |
| `R/blockr-input-dep.R` | `blockr_input_dep()` htmlDependency helper |

#### Files to modify

| File | Changes |
|------|---------|
| `inst/assets/js/filter-unified-input.js` | Remove `withAce`, `makeCompleter`, `createAceEditor`; replace calls |
| `inst/assets/js/join-unified-input.js` | Same |
| `inst/assets/js/summarize-unified-input.js` | Same |
| `inst/assets/js/mutate-unified-input.js` | Same |
| `inst/assets/js/multi-expr-input.js` | Same (uses slightly different helper names) |
| `R/filter_unified.R` | Replace ACE dep with `blockr_input_dep()` |
| `R/join_unified.R` | Same |
| `R/summarize_unified.R` | Same |
| `R/mutate_unified.R` | Same |

---

## V2: Add syntax highlighting (mirror layer)

### Approach

Use the **transparent-text mirror technique**: the actual `<input>`/`<textarea>`
has `color: transparent` (with a visible `caret-color`), and a `<div>` behind it
renders the same text with colored `<span>` tokens. The two elements are
identically sized and positioned so the highlighted text shows through.

### DOM structure (v2 addition)

```html
<div class="blockr-input blockr-input--highlighted">
  <div class="blockr-input__mirror" aria-hidden="true">
    <span class="blockr-input__tok--func">mean</span>
    <span class="blockr-input__tok--paren">(</span>
    <span class="blockr-input__tok--col">col_a</span>
    <span class="blockr-input__tok--paren">)</span>
  </div>
  <input class="blockr-input__field" ... />
  <!-- field has color:transparent, caret-color: visible -->
  <div class="blockr-input__popup" ...>...</div>
</div>
```

### Tokenizer

A simple state-machine tokenizer for single-line R expressions. No need for a
full parser — just token classification:

| Token type | Pattern | Color |
|-----------|---------|-------|
| `string` | `"..."` or `'...'` | green |
| `number` | `\d+(\.\d+)?` | blue |
| `operator` | `+ - * / > < = ! & \| ~ %` | gray |
| `paren` | `( ) [ ]` | dark gray |
| `known-func` | word matching a category function | purple |
| `column` | word matching a known column name | teal |
| `keyword` | `TRUE FALSE NULL NA NaN Inf` | bold |
| `text` | everything else | default |

The tokenizer is ~50-60 lines: scan character by character, classify runs.

### Mirror sync

On every `input` event:
1. Tokenize the input value
2. Build `<span>` elements for each token
3. Replace `__mirror` innerHTML

For single-line inputs this is fast (< 1ms for typical expression lengths).

### CSS additions (v2)

```css
.blockr-input--highlighted .blockr-input__field {
  color: transparent;
  caret-color: var(--blockr-color-text-primary, #111827);
}
.blockr-input__mirror {
  position: absolute;
  top: 0; left: 0; right: 0; bottom: 0;
  pointer-events: none;
  font: inherit;         /* must match __field exactly */
  padding: inherit;
  white-space: pre;
  overflow: hidden;
}
.blockr-input__tok--func   { color: #8b5cf6; }  /* purple */
.blockr-input__tok--col    { color: #0891b2; }  /* teal */
.blockr-input__tok--string { color: #16a34a; }  /* green */
.blockr-input__tok--number { color: #2563eb; }  /* blue */
.blockr-input__tok--op     { color: #6b7280; }  /* gray */
.blockr-input__tok--kw     { font-weight: 600; }
```

### Key challenge: font alignment

The mirror div and input must have **identical** font metrics (family, size,
letter-spacing, padding). Any mismatch causes visible drift. Use `inherit` and
ensure both elements share the same parent styling. Test with a few edge cases
(backtick-quoted names, long expressions).

### Activation

Syntax highlighting is opt-in via config:

```javascript
BlockrInput.create(container, {
  highlight: true,  // default false in v1, enable in v2
  // ...
});
```

This way v1 consumers don't break when v2 ships.
