# BlockrSelect: Custom select component replacing selectize.js

## Motivation

The unified blocks in blockr.dplyr (filter, join, summarize) use selectize.js for
dropdowns. Selectize looks dated, and the unified blocks already build their entire
UI in JS — they only use selectize through thin wrapper functions
(`createColumnSelectize`, `createValueSelectize`, etc.). The wrappers require
polling for selectize availability (`withSelectize`), heavy CSS `!important`
overrides, and a jQuery UI dependency just for drag-drop. A self-contained custom
component gives full design control, drops dependencies, and is portable to other
packages later.

## Component API

### `BlockrSelect.single(container, config)` — single-select with search

```javascript
var col = BlockrSelect.single(colDiv, {
  options: ["col_a", "col_b"],      // string[]
  selected: "col_a",                // string | null (auto-selects first)
  placeholder: "Column...",         // string
  onChange: function(value) {}       // called with string
});
// Returns: { el, setOptions(opts, selected), getValue(), destroy() }
```

### `BlockrSelect.multi(container, config)` — multi-select with tags, remove, drag reorder

```javascript
var vals = BlockrSelect.multi(valDiv, {
  options: ["a", "b", "c"],         // string[]
  selected: ["a"],                  // string[]
  placeholder: "Select values...",  // string
  reorderable: true,                // enable drag reorder (default true)
  onChange: function(values) {}      // called with string[]
});
// Returns: { el, setOptions(opts, selected), getValue(), destroy() }
```

Both modes: `setOptions()` replaces all options, preserves valid selections.
`destroy()` removes DOM and listeners.

## DOM structure

### Single mode

```html
<div class="blockr-select blockr-select--single" tabindex="0"
     role="combobox" aria-expanded="false" aria-haspopup="listbox">
  <div class="blockr-select__control">
    <span class="blockr-select__value">col_a</span>
    <input class="blockr-select__search" type="text" aria-autocomplete="list" />
    <span class="blockr-select__arrow"></span>
  </div>
  <div class="blockr-select__dropdown" role="listbox">
    <div class="blockr-select__option" role="option" data-value="col_a">col_a</div>
    ...
    <div class="blockr-select__empty">No matches</div>
  </div>
</div>
```

### Multi mode

```html
<div class="blockr-select blockr-select--multi" tabindex="0"
     role="combobox" aria-expanded="false" aria-haspopup="listbox">
  <div class="blockr-select__control">
    <div class="blockr-select__tags">
      <span class="blockr-select__tag" draggable="true" data-value="a">
        a <button class="blockr-select__tag-remove" aria-label="Remove a"></button>
      </span>
    </div>
    <input class="blockr-select__search" type="text" placeholder="Select values..." />
  </div>
  <div class="blockr-select__dropdown" role="listbox">
    <!-- only unselected options shown -->
  </div>
</div>
```

## Internal architecture

### State & rendering

Central `_update()` method:
1. Filters options by search query (case-insensitive substring match)
2. In multi mode, excludes already-selected values from dropdown
3. Rebuilds dropdown options DOM
4. Updates value display (single) or tags (multi)
5. Fires `onChange` synchronously if selection changed

### Drag-and-drop (multi mode, no jQuery UI)

Uses HTML5 Drag and Drop API (~60 lines):
- Tags get `draggable="true"`
- `dragstart`: store dragged value, add `.blockr-select__tag--dragging`
- `dragover`: `preventDefault()`, detect insertion position via `getBoundingClientRect` + `clientX`
- `drop`/`dragend`: reorder `selected` array, call `_update()`

### Dropdown positioning

On open, check if dropdown overflows viewport bottom. If so, add `--above` modifier
to open upward (`bottom: 100%` instead of `top: 100%`).

### Keyboard navigation

| Key | Closed | Open |
|-----|--------|------|
| Enter / Space | Open | Select highlighted |
| ArrowDown / ArrowUp | Open | Move highlight |
| Escape | — | Close |
| Backspace (multi, empty search) | — | Remove last tag |
| Typing | Open + filter | Filter |

### Pending state

The `withSelectize` polling pattern goes away — `BlockrSelect` initializes
synchronously. The existing `_pendingColumns` pattern in Shiny input bindings
(which buffers R→JS messages before the component mounts) stays as-is; it's
outside the select component.

## CSS approach

- BEM naming: `.blockr-select`, `.blockr-select__control`, `.blockr-select__tag`, etc.
- References existing `--blockr-color-*`, `--blockr-grey-*`, `--blockr-font-size-*` tokens with fallbacks
- Control is **transparent by default** (no border/bg) — fits inside `.fu-row`, `.ju-row`, `.su-row` containers
- `.blockr-select--bordered` modifier for standalone use (e.g. summarize group-by picker)
- No `!important` rules — component owns its DOM
- Dropdown: `position: absolute`, `z-index: 9999`, `border-radius: 8px`, shadow

## Migration pattern

The migration is mechanical — the new API matches the old wrapper signatures:

```javascript
// BEFORE:
cond._colSelectize = createColumnSelectize(
  colDiv, this.columnNames, column,
  function(value) { self._onColumnChange(cond, value); }
);

// AFTER:
cond._colSelectize = BlockrSelect.single(colDiv, {
  options: this.columnNames,
  selected: column,
  placeholder: "Column...",
  onChange: function(value) { self._onColumnChange(cond, value); }
});
```

`setOptions()`, `getValue()`, `destroy()` calls in consumers remain unchanged.

## Files

### New files

| File | Purpose |
|------|---------|
| `inst/assets/js/blockr-select.js` | IIFE exposing `BlockrSelect.single()` and `BlockrSelect.multi()` |
| `inst/assets/css/blockr-select.css` | BEM-styled CSS using existing `--blockr-*` design tokens |
| `R/blockr-select-dep.R` | `blockr_select_dep()` — `htmlDependency` helper |

### Files to modify

| File | Changes |
|------|---------|
| `inst/assets/js/filter-unified-input.js` | Remove `withSelectize`, `createColumnSelectize`, `createValueSelectize`; replace calls with `BlockrSelect.single` / `BlockrSelect.multi` |
| `inst/assets/js/join-unified-input.js` | Remove `withSelectize`, `createColumnSelectize`; replace calls with `BlockrSelect.single` |
| `inst/assets/js/summarize-unified-input.js` | Remove `withSelectize`, `createFuncSelectize`, `createColumnSelectize`, `createGroupBySelectize`; replace calls with `BlockrSelect.single` / `BlockrSelect.multi` |
| `inst/assets/css/filter-unified.css` | Remove `.fu-row .selectize-*` override rules |
| `inst/assets/css/join-unified.css` | Remove `.ju-row .selectize-*` override rules |
| `inst/assets/css/summarize-unified.css` | Remove `.su-* .selectize-*` override rules |
| `R/filter_unified.R` | Replace jqueryui dep with `blockr_select_dep()`; remove hidden `selectizeInput` loader |
| `R/join_unified.R` | Replace jqueryui dep with `blockr_select_dep()` |
| `R/summarize_unified.R` | Replace jqueryui dep with `blockr_select_dep()`; remove hidden `selectizeInput` loader |
| `R/mutate_unified.R` | Remove jqueryui dep |
