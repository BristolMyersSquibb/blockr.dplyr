# Flat block arguments — flattening the single `state` blob

_Status: implemented on branch `refactor/dplyr-flat-block-args`, version
`0.1.0.9004`. Not yet merged._

## Motivation

Every blockr.dplyr block used to take a single opaque constructor argument:

```r
new_filter_block(state = list(conditions = list(...), operator = "&"))
```

The AI assistant (blockr.ai) and external controllers discover and set block
arguments from the constructor's **formals**. A single `state = list(...)`
hides every real field behind one nested blob, which:

- made the assistant emit the wrong shape (flat fields vs. a `state` wrapper),
- meant external control could only replace the *whole* state at once, and
- read poorly in `?new_filter_block` and in constructor calls.

blockr.ai had a workaround that "promoted" a single `state` argument's children
to top-level arguments. This refactor removes the need for that by making the
**block API itself flat** — one source of truth.

```r
new_filter_block(conditions = list(...), operator = "&", preserve_order = FALSE)
```

The whole point: external controllers now set **individual fields**
(`state$conditions(...)`, `state$operator(...)`), not the whole `state` blob.

## What changed

### 1. Flat constructor arguments (13 blocks)

Each block's old `state = list(...)` default became flat top-level arguments
with the same names and defaults. The constructor reassembles the `state` list
internally and hands it to the factory:

```r
new_filter_block <- function(conditions = list(),
                             operator = "&",
                             preserve_order = FALSE,
                             ...) {
  new_js_transform_block(
    class = "filter_block",
    name  = "filter",
    state = list(conditions = conditions, operator = operator,
                 preserve_order = preserve_order),
    expr_fn = function(s) { ... },
    ...
  )
}
```

Blocks: `filter`, `select`, `arrange`, `mutate`, `summarize`, `slice`,
`rename`, `pivot_longer`, `pivot_wider`, `unite`, `separate` (via
`new_js_transform_block`), plus the bespoke `join` and `bind_rows` servers.
`bind_cols` has no state.

Two "stray" keys the JS blob carried but that were never real arguments were
promoted to proper flat args (all user-tweakable state should be a documented
argument): filter's `preserveOrder` (renamed to snake_case `preserve_order`,
JS wire key renamed to match, and its `setState` restore — previously missing —
fixed) and pivot_wider's `values_fn`.

### 2. Per-field reactiveVals as the source of truth

`R/js-block.R` gained `js_block_state()`, which holds **one `reactiveVal` per
field** instead of one `reactiveVal` for the whole blob. The JS side still
emits a single state blob; the helper decomposes it into the per-field
reactiveVals (JS → R) and recombines them to push back to JS (R → JS, with the
self-write guard). It returns:

- `fields` — the named list of per-field reactiveVals, returned as the block's
  `state`,
- `state` — a `reactive` recombining them, for `expr_fn` and the JS push.

This is required by blockr.core: for `external_ctrl = TRUE` blocks,
`external_ctrl_vars()` resolves to `block_ctor_inputs()` (the flat formals), and
**every externally controllable variable must be a `reactiveVal`**
(`block-server.R`). Read-only `reactive()`s would abort. It is also exactly what
enables field-level external control.

### 3. `list2env(state, environment())` in the factory

blockr.core's `initial_block_state()` reads each constructor formal **by name
from the (expr-)server's enclosing environment** (`block-class.R`). For
factory-built blocks the flat formals live in the *calling constructor's* frame,
not the factory's, so the factory binds them locally:

```r
list2env(state, environment())   # names(state) == the ctor's flat formals
```

(The bespoke `join` / `bind_rows` constructors don't need this — their flat args
are formals of the same function that defines the server.)

### 4. Restore / back-compat (`R/legacy-deser.R`)

blockr.core restores a block by calling its constructor with the serialized
state fields as named arguments (`do.call(ctor, payload)`). The per-block
`blockr_deser.<class>` methods were inverted to emit **flat args** and to handle
three board generations:

- **gen-1** (oldest, individual params, sometimes different shapes): the
  per-block `state_builder` normalizes them. Builders were made idempotent so
  they don't corrupt already-modern payloads (e.g. filter keeps a condition's
  `type` if present; summarize only migrates a *named* `summaries` list; join
  prefers `keys` over the old `by`).
- **gen-2** (interim single `{state: {...}}` blob): unwrapped to flat fields
  (its contents already use modern shapes).
- **gen-3** (current flat format): passes through.

`blockr_deser.mutate_block` was added (that class only ever existed in gen-2/3,
so it just unwraps/passes through).

**Block attributes** (`block_name`, …) are serialized as sibling payload entries
next to the state fields; `legacy_deser_block` carries them through to the
constructor so custom block names survive restore. (Missing this was a real
restore bug — blocks came back with default names.)

## Decision: no constructor `state=` back-compat shim

A shim was prototyped that let the flat constructors still accept a legacy
`state = list(...)` via `...` (folded into the flat defaults, with a loud
deprecation warning, kept off the formals/registry/assistant surface). It was
**rejected and reverted**: production boards restore from JSON through the
deserialization path above, and the handful of R scripts that called
`new_*_block(state = list(...))` are a quick migration. Keeping the constructors
strictly flat avoids re-polluting the assistant surface.

Downstream callers therefore migrate from `new_*_block(state = list(...))` to
flat args. Done so far: blockr.insurance examples; CEDX (`blockr.cdex`
`cedx_board.R` + the cedx-explorer demos) — migrating **only** the 13
blockr.dplyr constructors; blockr.viz/blockr.extra blocks
(`new_summary_table_block`, `new_value_filter_block`, …) keep their `state=`
API. Other packages (blockr.ai, blockr.csr, blockr.admiral, …) migrate as
needed.

## The invariant

The refactor keeps a three-way identity, by construction:

```
constructor formals  ≡  fields the server returns as `state`  ≡  serialized JSON fields
```

`names(state)` (assembled from the flat args) drives the fan-out, the
serialized keys, and the restore-via-constructor arguments — so they cannot
drift apart.

## Versioning

The package was renumbered to the `0.1.0.9xxx` development line
(`0.1.0.9004`), reserving `0.2.0` for the CRAN release. A different version
string also busts the htmlwidget JS dependency cache (needed for the
`preserve_order` wire-key rename in `filter-block.js`).

## Verification

- 326 package tests pass (constructor calls flattened; `$state$state(list(...))`
  setters rewritten to per-field `$state$<field>(...)`).
- serialize → deserialize → serialize is byte-identical; payloads are flat (no
  `state` key); custom `block_name` round-trips.
- gen-1 / gen-2 / gen-3 payloads all deserialize and build.
- `blockr.insurance/dev/treaty-pricer.R` builds, serializes and restores
  end-to-end on the flat code.
- `check_man()` clean. Full `R CMD check`'s one ERROR (shinytest2 browser launch
  under the check sandbox, `skip_on_cran`) and one WARNING (`rlang::sym`
  undeclared in `expr-builders.R`, since fixed by adding `rlang` to Imports) are
  pre-existing and unrelated.

## Files

- `R/js-block.R` — `new_js_transform_block` factory, `js_block_state` fan-out,
  `list2env`.
- `R/legacy-deser.R` — inverted per-block deserializers (flat output, gen-1/2/3,
  block-attr carry-through).
- `R/*_block.R` — flat constructors.
- `inst/js/filter-block.js`, `inst/js/types.d.ts` — `preserveOrder` →
  `preserve_order` wire key + restore fix.
- `tests/testthat/*` — flat constructor calls and per-field state setters.
