# blockr Block Design System

Design conventions for JS-driven blocks across blockr packages. The base system lives in blockr.dplyr's shared CSS/JS files. Package-specific blocks extend it.

## Fonts

All block UI inherits from the page font (set by Bootstrap/blockr.dock). Never hardcode font-family on containers — use `font: inherit`.

Monospace font is only used for:
- Code input fields (R expressions): `'SF Mono', 'Fira Code', 'Consolas', monospace`
- Code completion items

Sizes use dock CSS variables:
- `--blockr-font-size-base`: 0.875rem (14px) — body text, inputs
- `--blockr-font-size-sm`: 0.8125rem (13px) — labels, helper text
- `--blockr-font-size-xs`: 0.75rem (12px) — meta tags, subtitles

Weights: `--blockr-font-weight-medium` (500) for labels, `--blockr-font-weight-semibold` (600) for emphasis.

## Colors

Greyscale (Tailwind-aligned):
- `--blockr-grey-50` (#f9fafb) — input backgrounds, subtle bg
- `--blockr-grey-200` (#e5e7eb) — borders
- `--blockr-grey-400` (#9ca3af) — placeholders, muted icons, secondary text
- `--blockr-grey-500` (#6b7280) — labels, descriptions

Semantic:
- `--blockr-color-text-primary` (#111827) — main text
- `--blockr-color-border` (#e5e7eb) — all borders
- `--blockr-color-bg-input` (#f9fafb) — input backgrounds
- `--blockr-color-primary` (#2563eb) — focus rings, active states
- `--blockr-color-danger` (#dc3545) — delete hover

## Sizing

Standard heights:
- **42px** — standalone inputs, bordered selects (the universal input height)
- **30px** — nested inputs within rows, number inputs, add-row bar
- **26px** — icon buttons (gear, remove)
- **24px** — pill toggles, confirm buttons

Border radius: **8px** for inputs/rows/dropdowns, **4px** for pills/buttons.

## Interaction States

**Focus ring:** `border-color: var(--blockr-color-primary)` + `box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1)`. Consistent across all focusable elements.

**Hover:** Background shifts to `--blockr-grey-50` or `--blockr-grey-100`. Transitions 0.15s ease.

**Row remove button:** Hidden (opacity 0), revealed on row hover. Turns red (`--blockr-color-danger`) on button hover.

**Gear button:** Top-right, 26px square. Active state: primary color + subtle primary background.

## Layout Patterns

### Row-based blocks (filter, mutate, arrange)

```
.blockr-row          — flex container, 42px min-height, grey bg, 8px radius
.blockr-row-content  — flex: 1, holds the dynamic content
.blockr-row-remove   — 26px button, auto margin-left, hidden until hover
.blockr-add-row      — footer bar with "+ Add" link
```

Rows have a fixed-width left column (150-160px) for the "key" (column name), a separator (`=`, `→`), and flex content for the "value."

### Gear popover

```
.blockr-gear-header  — flex, justify-content: flex-end
.blockr-gear-btn     — 26px square icon button
.blockr-popover      — absolute positioned panel, 8px radius, shadow
```

### Responsive layout

Blocks have no fixed width — they stretch to fill their stack/card slot, which can range from ~300px (narrow sidebar) to the full workspace width. Input rows must reflow: horizontal when wide, stacked when narrow.

**Use flex-wrap, not grid auto-fit and not media queries.** The idiom is a wrapping flex row with each field claiming an equal share above a soft minimum:

```css
.xxb-grid  { display: flex; flex-wrap: wrap; align-items: flex-end; gap: 6px 12px; }
.xxb-field { flex: 1 1 160px; min-width: 0; display: flex; flex-direction: column; }
```

Rules:
- `flex: 1 1 160px` — fields grow equally, shrink to the ~160px soft minimum, then wrap. Tune the basis per block (shorter labels can go ~120px; long inputs need 200px+).
- `min-width: 0` is mandatory on flex children that contain `Blockr.Select` or other overflowing content — otherwise the intrinsic content width prevents shrinking.
- `align-items: flex-end` keeps inputs bottom-aligned when labels span two lines on one field but not another.
- For a field that should always occupy its own row (e.g. a multi-select with many tags), add a `--full` modifier: `.xxb-field--full { flex: 1 1 100%; }`.

Never use `@media` or `@container` queries for block internals — block width is unknown at author time and can change at runtime as the user resizes panels.

Canonical examples: `pivot-longer-block.css` (`.plb-input-row` / `.plb-field`), `summary-table-block.css` in blockr.bi (`.stb-grid` / `.stb-field` / `.stb-field--full`).

## Column labels in pickers

Column options in `Blockr.Select` may carry an optional human-readable label
alongside the raw column name. Typical source: `attr(col, "label")` on ADaM
or other annotated datasets.

- **Option shape:** `{ value: "AVAL", label: "Analysis Value" }` (bare strings
  still work for unlabelled columns).
- **Slot:** `.blockr-select__opt-label` — a span appended by
  `fillOptContent()` next to the value in both dropdown options and the
  single-mode selected-value display. Multi-mode tag pills stay value-only.
- **Style:** grey-500, `--blockr-font-size-sm`, 8px left margin.
- **Truncation is CSS-only.** No R-side character counting. The parent
  `.blockr-select__value` is `white-space: nowrap; overflow: hidden;
  text-overflow: ellipsis;`, so the label (which sits after the value)
  ellipsizes when space runs out. The value is always preserved.
- **Tooltip:** native browser tooltip via `title` attribute. No custom
  mouseover listeners — the browser handles timing, positioning, and
  cleanup reliably.

The same slot is reused by consumers outside blockr.dplyr (e.g. blockr.dm's
table pickers for dm table labels).

## Shared JS Components

### Blockr.Select (single + multi)
- Single: value display, click to open, search to filter
- Multi: tag pills, drag-to-reorder, backspace to remove last
- API: `Blockr.Select.single(container, config)` / `.multi(container, config)`
- Config: `{ options, selected, placeholder, reorderable, onChange }`
- Options: `string[]` or `{value, label}[]` — labels render via `.blockr-select__opt-label`.

### Blockr.Input (code autocomplete)
- Lightweight code editor with token-based autocomplete
- Shows column names + function categories
- API: `Blockr.Input.create(container, config)`
- Config: `{ value, columns, categories, placeholder, onChange, onConfirm }`

### Blockr.icons
SVG icon strings: `chevron` (12px), `remove` (10px), `x` (14px), `plus` (14px), `confirm` (14px), `code` (14px), `gear` (14px).

## Naming Conventions

**CSS classes:** BEM-like. Component name as block, `__` for elements, `--` for modifiers.
```
.blockr-select                    — component
.blockr-select__control           — element
.blockr-select--open              — modifier
.blockr-select__option--highlighted — element + modifier
```

**Package-specific prefixes:** `fb-` (filter block), `mb-` (mutate block), `plb-` (pivot longer), `sb-` (summarize).

## File Organization

Shared (in blockr.dplyr, copied to other packages until extracted to blockr.core):
- `blockr-core.js` — namespace, icons, utilities
- `blockr-blocks.css` — rows, pills, buttons, popover, gear
- `blockr-select.js/css` — select component
- `blockr-input.js/css` — code input component

Block-specific: one `.js` + one `.css` per block type.

## TODOs

Inconsistencies to address when updating the design system in blockr.dock:

- **Mono font variable:** The mono stack `'SF Mono', 'Fira Code', 'Consolas', 'Monaco', monospace` is hardcoded in 4 places in `blockr-input.css`. Should be a `--blockr-font-mono` CSS variable defined in blockr.dock so all packages reference it.

- **Autocomplete popup font:** `blockr-input.css:62` hardcodes `-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif` on the autocomplete popup. Should use `font-family: inherit` like `blockr-select.css` does.

- **Border radius tiers:** Doc says 8px/4px but code also uses 6px (in separate, bind-rows, pivot-longer, unite for internal card sections). Need a clear three-tier system: 8px (containers/dropdowns) / 6px (sub-sections) / 4px (pills/buttons). Or standardize to two tiers.

- **`sb-` prefix collision:** Both select block and summarize block use the `sb-` prefix. One needs renaming (e.g., `selb-` for select or `smb-` for summarize).

- **Missing from doc:** The `--bordered` modifier pattern (42px standalone variant), the 38px popover control height, and dropdown shadow values (`0 4px 12px rgba(0, 0, 0, 0.1)`) should be documented.

- **Prefix inventory incomplete:** Only 4 of 13 block prefixes were listed. Full set: `ab-` (arrange), `bcb-` (bind-cols), `brb-` (bind-rows), `fb-` (filter), `jb-` (join), `mb-` (mutate), `plb-` (pivot-longer), `pwb-` (pivot-wider), `rb-` (rename), `sb-` (select/summarize), `slb-` (slice), `spb-` (separate), `ub-` (unite).
