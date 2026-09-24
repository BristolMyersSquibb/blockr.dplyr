# Rewrite of Blockr.Select

Plan for rewriting `inst/js/blockr-select.js` before it moves to blockr.ui.
Written 2026-09-24 on branch `feat/design-system`. The work happens on
`feat/select-rewrite`, branched from it.

## Why

`Blockr.Select` is the dropdown every blockr package uses: 92 production call
sites in 14 packages (`Select.single` 57, `Select.multi` 32, `Select.menu` 3).
It moves to blockr.ui next, where it will be reviewed by someone who does not
own it, so it should arrive in its final form.

Today it is one closure, `createSelect`, of 987 lines: 669 lines of code, 218
of comments, 31 inner functions. Three modes (single, multi, and the headless
menu) are woven through it with `if (mode ...)` and `if (headless)` checks.
State is 30 closure variables that each render function reads and writes.

What a rewrite should gain:

- One core for the three modes. They differ in how the pick is shown (a value,
  tags in the control, tags in the menu head) and in what a pick does (close,
  or stay open). Everything else is shared: the list, filtering, keyboard,
  positioning, loading, server search.
- State in one object; events change it, one render draws it. Bugs like "the
  menu opens with the keyboard row on option 0 while the pick is option 5"
  (fixed 2026-09-23) cannot arise when the highlight is derived from state.
- Positioning as a shared routine, `Blockr.place(panel, anchor, opts)` in
  `blockr-ui.js`. The design system asks for it ("One routine for all of them
  (Select's computePosition, moved to blockr.ui)"), and blockr.pharma's patient
  search, blockr.io's path input and blockr.viz's @-menu position their own
  floating panels by hand today.
- The ARIA combobox pattern (WAI-ARIA 1.2) from the start.
- Less code. A guess: 400 to 500 lines.

## What must not change

The public API was settled in commit `4d82f37` and is described in
`inst/js/types.d.ts` (`BlockrSelect*` types). The rewrite keeps it exactly:

- `Blockr.Select.single(container, config)`, `.multi(container, config)`,
  `.menu(anchor, config)`, the flag `Blockr.Select.menuMulti = true`, and the
  test exports `fitCount`, `midTruncate`.
- Config keys: `options`, `selected`, `placeholder`, `onChange`, `onOpen`,
  `onClose`, `onSearch`, `loading`, `allowEmpty` (single), `reorderable`,
  `singleLine`, `maxTagChars` (multi), `bordered`, `search`, and for menus
  `mode`, `title`, `labelFirst`, `searchPlaceholder`.
- Handle: `el`, `getValue()`, `setValue(v)`, `setOptions(opts, sel)`,
  `updateOptions(opts, sel)`, `setLoading(flag)`, `setSearchInfo(info)`,
  `destroy()`. `menu()` returns `{ close }`.
- Option shape: a string, or `{ value, label }`. Values may arrive as numbers
  (numeric value pickers); filtering stringifies them.
- The CSS class names (`blockr-select`, `--single`, `--multi`, `--bordered`,
  `--open`, `--above`, `--headless`, `--single-line`, `--expanded`,
  `__control`, `__value`, `__value--placeholder`, `__arrow`, `__search`,
  `__search--menu`, `__search--offscreen`, `__tags`, `__tags--menu`, `__tag`,
  `__tag-label`, `__tag-remove`, `__tag--hidden`, `__tag--dragging`,
  `__tag--drop-before`, `__tag--drop-after`, `__more`, `__dropdown`,
  `__menu-title`, `__option`, `__option--highlighted`, `__option--selected`,
  `__opt-label`, `__empty`, `blockr-select-menu-host`). CSS in
  `inst/css/blockr-select.css` and in other packages styles them, and two
  packages read the markup (blockr.dm `value-filter-block.js:106` reads
  `.blockr-select__dropdown`, blockr.extra `labeler-block.js:230` reads
  `.blockr-select__value`).
- The dropdown is portalled to `<body>` while open (it has to escape dock
  panels' clipping and stacking contexts).

The long comments in the current file record why each behaviour exists. Read
them; most are constraints the rewrite has to keep, not decoration.

## Behaviour to keep

A checklist for the behaviour suite. Each line is at least one test.

Opening and closing

- A single select opens on a click on its control and closes on a second
  click; a multi select opens on a click and keeps focus in its search input.
- It opens with Enter, Space, ArrowDown or ArrowUp from the keyboard.
- Escape closes; Tab closes; a click outside closes (and collapses an
  expanded single-line multi). A click on a menu's anchor does not count as
  outside: it is the caller's toggle.
- `onOpen` fires on every open; `onClose` fires after the DOM is settled.
- A menu is open from the moment it is created and tears itself down after
  closing (on the next tick, so the click that closed it can finish).

The list

- A multi select lists only what is not picked; a single select lists all
  options and marks the pick (`--selected`, `aria-selected`).
- Typing filters by value and by label, case-insensitive, numbers
  stringified. The filter box of a menu shows past 8 options, never with
  `search: false`, and holds focus either way (arrows and type-ahead need it).
- At most 200 options are rendered; a row says "+N more — type to narrow".
- Empty states: "Loading…" while loading, "No matches" for a query, "All
  selected" for a multi with nothing left, "No options" otherwise. (Their
  look is a design question left open; keep the texts.)
- Server search: after `setSearchInfo({ truncated: true, total })`, typing
  calls `onSearch(query)` debounced 250ms, and the list ends with "N values —
  type to search".

Keyboard

- ArrowDown and ArrowUp move the highlight and wrap; the highlighted row
  scrolls into view; `aria-activedescendant` follows it.
- A single select opens with the highlight on its pick.
- Enter picks the highlighted row. Backspace in an empty search input removes
  the last tag of a multi.

Picking

- Single: a pick closes, renders the value, and calls `onChange` only when
  the value changed.
- Multi: a pick appends, clears the query, keeps the list open, calls
  `onChange` with a copy. Removing a tag (its ×, or Backspace) calls
  `onChange` with a copy.
- Multi menu: picks show as tags in the menu head, above the filter box; the
  filter prompt survives a pick.

Reconciling (the subtle part; read `setOptions`, `updateOptions` and
`Blockr.reconcileColumn` in `blockr-core.js` before touching it)

- `setOptions(opts, sel)`: single falls back to the first option when `sel`
  is absent or unknown, unless `allowEmpty`, where `''` survives and an
  unknown pick clears; omitting `sel` keeps a still-valid pick. Multi keeps
  only picks present in the new options, and coerces a scalar `sel` to an
  array.
- `updateOptions(opts, sel)`: swaps the list without touching the pick; with
  `sel` it forces the pick even if the list lacks it.
- `setValue(v)`: as `setOptions(currentOptions, v)`, and never calls
  `onChange`.
- `getValue()` returns `''` or a string (single), a copy of the array (multi).

Tags (multi)

- A tag shows the value and, muted, its label (`labelFirst` swaps them in the
  list rows). `maxTagChars` cuts the middle of a long value and keeps the full
  value on hover.
- `reorderable` (default true) allows dragging a tag before or after another;
  the drop calls `onChange`.
- `singleLine`: tags past the first row hide behind a "+N" chip whose title
  lists them; a click on the chip expands the control until a click
  elsewhere. Fitting runs after each render and on control resize, and leaves
  every tag visible while the control has no width yet (a deferred panel).

Positioning (browser only; happy-dom has no layout)

- Fixed position under the control (or anchor), 4px gap, the control's width
  (a menu: 190px to 320px, pulled inside the viewport by 8px).
- Flips above when there is no room below and there is room above.
- Follows scroll (capture phase), window resize, and size changes of the
  control and of the list itself (ResizeObserver, next frame).

Lifecycle

- `destroy()` closes, removes the portalled dropdown, disconnects observers
  and removes every document or window listener it added.

## Questions to settle while rewriting

Found reading the current code; check each against a browser, then either keep
the behaviour or fix it and say so in the commit.

- Escape calls `root.focus()`, but the root has no `tabindex`, so focus goes
  nowhere. It should return to the element that opened the list.
- `onRootKeydown` only acts when the root itself has focus, which it cannot
  get. Keyboard opening works through the search input instead. Decide which
  element is the combobox and put the ARIA attributes on it.
- Every instance adds its own capture-phase document click listener. Use
  `Blockr.onDocClick` (blockr-ui.js), which drops entries whose element left
  the document, or keep one listener per open select.
- `render()` rebuilds every tag on every change; with `singleLine` that
  re-measures all widths. Fine at today's sizes; keep it unless it gets in
  the way.

## Steps

1. Behaviour suite against today's Select. New file
   `tests/js/select-behaviour.test.js` (about 50 tests) covering the list
   above except positioning. Test what a caller can see: `getValue`, the
   `onChange` calls, the rows listed, the highlighted row, classes, ARIA
   attributes. No private state. It must pass against the current file.
   Commit.
2. Two implementations under one suite. Copy the current file to
   `tests/js/fixtures/blockr-select-reference.js` (tests only, not shipped).
   The suite loads the implementation named by an env var or runs every test
   against both (`reference` and `current`); the existing
   `select-menu.test.js`, `select-single-line.test.js` and
   `select-api.test.js` join in. Commit.
3. `Blockr.place(panel, anchor, opts)` in `blockr-ui.js`: the positioning and
   following logic, with `{ width: 'anchor' | { min, max }, gap, margin }`.
   Today's Select uses it before the rewrite, so this step is checked on its
   own. Commit.
4. The rewrite, in `inst/js/blockr-select.js`: state object, one render,
   shared core, modes as small differences. Every test passes against both
   implementations, or the difference is a deliberate fix from the questions
   above, noted in the test and the commit. Commit.
5. Browser check. A static page (`dev/select-playground.html`, loading
   `inst/js/blockr-ui.js`, `inst/js/blockr-select.js`, blockr.ui's token CSS
   and `inst/css/blockr-select.css`) with a single, a bordered single, a
   multi with many tags, a single-line multi in a narrow box, a menu near the
   bottom of the window, and a reorderable multi. Check positioning, flip,
   following scroll, tag fitting and dragging in headless Chrome (chromote),
   with screenshots in both schemes. Then the dplyr blocks in a board
   (`_scratch/ds-compact/app.R` has filter, select, arrange, slice, join,
   pivot). Commit.
6. Delete the reference copy once the suite runs against the new file only.
   Commit.

## Acceptance

- `npm test` passes (today 152 tests plus the new suite).
- R tests pass: `Sys.setenv(TESTTHAT_PARALLEL = "false")`, `load_all` of
  `/workspace/blockr.core` and `/workspace/blockr.ui` (branch
  `feat/design-system`), then `devtools::test(filter = "shinytest2", invert =
  TRUE)`. The parallel workers would load the installed, older blockr.ui.
- The browser checks of step 5, with screenshots.
- `inst/js/types.d.ts` still describes the API; `// @ts-check` stays on.
- No change to the public API or the class names above.

## Conventions

- Git: plain `git` is disabled in this container; use
  `/usr/lib/git-core/git`. Commit per step. No `Co-Authored-By` line.
- Any edit under `inst/js` or `inst/css` needs a `Version` bump in
  DESCRIPTION, or browsers keep the cached file.
- Plain JavaScript, no build step, no modules: blockr.ui has no JS toolchain,
  and choosing one is its maintainer's call.
- Comments state why, not what; the current file's comments are the model.
- Writing (comments, commit messages): plainly, facts only, no em dashes. See
  CLAUDE.md, "Writing".
- blockr.admiral is out of scope.

## Not in this rewrite

- Moving Select to blockr.ui (next step, a blockr.ui pull request).
- Switching other packages to the new placement routine.
- The look of empty states (topic 27 was dropped; keep today's texts).
