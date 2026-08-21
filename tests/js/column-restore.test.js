/* A block owns its column; the select widget does not.
 *
 * `Blockr.Select` in single mode has no `allowEmpty` for a column picker, so
 * `setOptions(opts, sel)` with a `sel` the new list does not carry slides the
 * widget onto option 0 (blockr-select.js). Four blocks used to read that pick
 * back out of the widget and store it as block state:
 *
 *   const current = widget.getValue();
 *   widget.setOptions(this.columnOptions, current);
 *   <block state> = widget.getValue();          // believe the widget
 *
 * A restore delivers exactly the column list that breaks this: while an
 * upstream is still settling, `data()` is the OLD frame, so R pushes columns
 * that do not include the ones the restored state names. Every pick collapsed
 * onto column 1 and never came back — `setState()` rebuilds from R's message,
 * which does not repeat. rename renamed the wrong column and filter kept its
 * values while re-pointing at another column, both of which change the data
 * that flows downstream, silently.
 *
 * The two cases the reconcile has to tell apart:
 *   pinned   — the board restored this column, or the user picked it. It
 *              survives a list that does not offer it, and lands back on its
 *              option when the real columns arrive.
 *   unpinned — nothing but the select's own auto-pick of option 0. It still
 *              follows the list, so re-pointing the block at different data
 *              moves it.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { mount } = require('./harness.js');

/* The frames in the reproduction: a board saved against `iris` whose upstream
 * has not restored yet, so the first column push is the wrong frame. */
const IRIS = ['Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Species'];
const OTHER = ['a', 'b', 'c'];

/* Same frame, with the value lists a small frame's metadata carries — filter
 * renders its value picker from these without a lazy round trip. */
const IRIS_VALUES = IRIS.map((name) => ({
  name,
  label: '',
  type: name === 'Species' ? 'character' : 'numeric',
  values: name === 'Species' ? ['setosa', 'versicolor', 'virginica'] : undefined,
  uniqueValues: name === 'Species' ? undefined : [1, 2, 3]
}));

/**
 * Each block, its restored state, and how to read the columns back out of
 * what it would send to R.
 */
const BLOCKS = [
  {
    name: 'arrange',
    state: { columns: [{ column: 'Species', direction: 'desc' }] },
    columnsOf: (s) => s.columns.map((c) => c.column),
    picked: ['Species']
  },
  {
    name: 'rename',
    state: { renames: { species_name: 'Species' } },
    columnsOf: (s) => Object.values(s.renames),
    picked: ['Species']
  },
  {
    name: 'separate',
    state: { col: 'Species', into: ['genus', 'epithet'], sep: '_', remove: true, convert: false },
    columnsOf: (s) => (s.col ? [s.col] : []),
    picked: ['Species']
  },
  {
    name: 'filter',
    state: {
      conditions: [
        { type: 'values', column: 'Species', values: ['setosa'], mode: 'include', colType: 'character' }
      ],
      operator: '&',
      preserve_order: false
    },
    columnsOf: (s) => s.conditions.map((c) => c.column),
    picked: ['Species'],
    // A column with no values is not yet a filter — _compose() rightly emits
    // nothing for it — so the pick has to be completed with a value before
    // there is anything to check the column of.
    pickColumns: IRIS_VALUES,
    afterPick: (b) => b.pickNth(1, '1')
  }
];

for (const blk of BLOCKS) {
  const { name, state, columnsOf, picked } = blk;

  test(`${name}: a restored column survives a column list that does not offer it`, () => {
    const b = mount(name);
    b.setState(state);
    // The upstream has not restored yet: the wrong frame's columns.
    b.columns(OTHER);
    assert.deepStrictEqual(
      columnsOf(b.compose()), picked,
      'the block adopted the select\'s fallback instead of keeping its column'
    );
    // ...and the user sees the column the board asked for, not a name
    // nobody picked.
    assert.ok(b.faces().includes('Species'), `the select shows [${b.faces()}]`);
    b.close();
  });

  test(`${name}: the restored column lands back on its option when the real columns arrive`, () => {
    const b = mount(name);
    b.setState(state);
    b.columns(OTHER);
    b.columns(IRIS);
    assert.deepStrictEqual(columnsOf(b.compose()), picked);
    assert.ok(b.faces().includes('Species'));
    b.close();
  });

  test(`${name}: restore round-trips when the columns arrive first`, () => {
    const b = mount(name);
    b.columns(IRIS);
    b.setState(state);
    assert.deepStrictEqual(b.compose(), state);
    b.close();
  });

  test(`${name}: restore round-trips when the state arrives first`, () => {
    const b = mount(name);
    b.setState(state);
    b.columns(IRIS);
    assert.deepStrictEqual(b.compose(), state);
    b.close();
  });

  test(`${name}: a column list still never submits on its own`, () => {
    // R pushes state and columns; neither is a user edit. A submit here
    // would write the client's guess back over R's state — the loop
    // `js_block_state()`'s `self_write` guard exists to prevent.
    const b = mount(name);
    b.setState(state);
    b.columns(OTHER);
    b.columns(IRIS);
    assert.deepStrictEqual(b.submits, []);
    assert.strictEqual(b.value(), null, 'Shiny would have read a value R never asked for');
    b.close();
  });

  test(`${name}: a user pick survives a column list that does not offer it`, () => {
    const b = mount(name);
    b.columns(blk.pickColumns || IRIS);
    b.pickNth(0, 'Petal.Length');
    if (blk.afterPick) blk.afterPick(b);
    assert.deepStrictEqual(columnsOf(b.compose()), ['Petal.Length']);

    b.columns(OTHER);
    assert.deepStrictEqual(
      columnsOf(b.compose()), ['Petal.Length'],
      'a deliberate pick is as pinned as a restored one'
    );
    b.close();
  });

  test(`${name}: an unpicked column still follows the data`, () => {
    // Nothing was ever chosen here — the select's auto-pick of option 0 is
    // not a decision, so re-pointing the block at another frame must move
    // it rather than strand it on a column that no longer exists.
    const b = mount(name);
    b.columns(IRIS);
    const auto = columnsOf(b.compose());
    b.columns(OTHER);
    const moved = columnsOf(b.compose());

    for (const col of moved) {
      assert.ok(OTHER.includes(col), `stranded on "${col}", which the data no longer has`);
    }
    if (auto.length) assert.notDeepStrictEqual(moved, auto);
    b.close();
  });
}

/* --- what each block does with the wrong column, spelled out ----------- */

test('rename does not rename a column the board never named', () => {
  const b = mount('rename');
  b.setState({ renames: { species_name: 'Species' } });
  b.columns(OTHER);
  assert.deepStrictEqual(
    b.compose().renames, { species_name: 'Species' },
    'renaming the wrong column changes the schema every downstream block sees'
  );
  b.close();
});

test('filter keeps its values with its own column, not another one', () => {
  const b = mount('filter');
  b.columns(IRIS);
  b.setState({
    conditions: [
      { type: 'values', column: 'Species', values: ['setosa'], mode: 'include', colType: 'character' }
    ],
    operator: '&',
    preserve_order: false
  });
  b.columns(OTHER);

  const cond = b.compose().conditions[0];
  assert.strictEqual(cond.column, 'Species');
  assert.deepStrictEqual(cond.values, ['setosa']);
  // "Species is setosa" becoming "Sepal.Length is setosa" is an empty result
  // with no error anywhere — the failure this whole file is about.
  b.close();
});

test('filter keeps the column type it was restored with while the column is away', () => {
  // _compose() reads colType from live column metadata, falling back to the
  // restored `_savedColType`. A column the current frame does not carry has
  // no live metadata, so the fallback is what stops R from guessing the type
  // by coercibility ("007" must not become 7).
  const b = mount('filter');
  b.setState({
    conditions: [
      { type: 'values', column: 'id', values: ['007'], mode: 'include', colType: 'character' }
    ],
    operator: '&',
    preserve_order: false
  });
  b.columns(OTHER);
  assert.strictEqual(b.compose().conditions[0].colType, 'character');
  b.close();
});

test('arrange keeps its sort directions with its columns', () => {
  const b = mount('arrange');
  b.setState({
    columns: [
      { column: 'Species', direction: 'desc' },
      { column: 'Petal.Length', direction: 'asc' }
    ]
  });
  b.columns(OTHER);
  assert.deepStrictEqual(b.compose().columns, [
    { column: 'Species', direction: 'desc' },
    { column: 'Petal.Length', direction: 'asc' }
  ]);
  b.close();
});

test('rename keeps one row per column, not two rows on the first one', () => {
  // Two rows collapsing onto the same column is worse than a wrong name:
  // `renames` is keyed by new name, so both rows survive as keys but point
  // at one source column, and the second rename fails.
  const b = mount('rename');
  b.setState({ renames: { len: 'Sepal.Length', sp: 'Species' } });
  b.columns(OTHER);
  assert.deepStrictEqual(b.compose().renames, { len: 'Sepal.Length', sp: 'Species' });
  b.close();
});

/* --- multi column pickers: the same rule, one plural --------------- */

const MULTI = [
  { name: 'select', field: 'columns', state: { columns: ['Species', 'Petal.Length'], exclude: false, distinct: false } },
  { name: 'unite', field: 'cols', state: { col: 'u', cols: ['Species', 'Petal.Length'], sep: '_', remove: true, na_rm: false } },
  { name: 'pivot-longer', field: 'cols', state: { cols: ['Species', 'Petal.Length'], names_to: 'name', values_to: 'value', values_drop_na: false, names_prefix: '' } },
  { name: 'pivot-wider', field: 'names_from', state: { names_from: ['Species', 'Petal.Length'], values_from: ['Sepal.Length'], id_cols: [], values_fill: null, names_sep: '_', names_prefix: '', values_fn: null } },
  { name: 'slice', field: 'by', state: { type: 'head', n: 5, prop: null, order_by: '', with_ties: true, weight_by: '', replace: false, rows: '1:5', by: ['Species', 'Petal.Length'] } },
  { name: 'summarize', field: 'by', state: { summaries: [], by: ['Species', 'Petal.Length'] } },
  { name: 'mutate', field: 'by', state: { mutations: [{ name: 'a', expr: '1' }], by: ['Species', 'Petal.Length'] } }
];

for (const { name, field, state } of MULTI) {
  test(`${name}: a multi picker keeps every restored column through a stale list`, () => {
    // A multi select never auto-picks, so everything in one was restored or
    // chosen. `setOptions` FILTERS against the new list, which during a
    // restore emptied the picker outright — no columns selected, nothing to
    // pivot, no group-by, and no way back.
    const b = mount(name);
    b.setState(state);
    b.columns(OTHER);
    assert.deepStrictEqual(b.compose()[field], state[field]);

    b.columns(IRIS);
    assert.deepStrictEqual(b.compose()[field], state[field]);
    b.close();
  });

  test(`${name}: a multi picker shows the columns the block holds`, () => {
    // The chips are the only place the user can see what is selected; a
    // picker that holds two columns and shows none is worse than either.
    const b = mount(name);
    b.setState(state);
    b.columns(OTHER);
    const holding = b.selects()
      .map((s) => b.chipsOf(s))
      .filter((chips) => chips.length);
    assert.ok(
      holding.some((chips) => state[field].every((c) => chips.includes(c))),
      `no picker shows [${state[field]}]; chips are ${JSON.stringify(holding)}`
    );
    b.close();
  });
}

test('summarize: a row that takes no column is not given one', () => {
  // n() has no column, so "" is this row's answer and not a gap the select
  // may fill from option 0. (Also covered end-to-end in test-shinytest2.R.)
  const b = mount('summarize');
  b.setState({
    summaries: [
      { type: 'simple', name: 'avg', func: 'mean', col: 'Species' },
      { type: 'simple', name: 'n_rows', func: 'n', col: '' }
    ],
    by: []
  });
  b.columns(IRIS);
  b.columns(OTHER);
  b.columns(IRIS);

  assert.deepStrictEqual(
    b.compose().summaries.map((s) => s.col), ['Species', '']
  );
  b.close();
});

/* --- the reconcile must not resurrect a column the data really dropped -- */

test('a pinned column that the new data does not have is still reported as picked', () => {
  // Deliberate: the block keeps showing what the user chose rather than
  // silently re-pointing at another column. R's expr builder is what
  // decides how to fail on a missing column — quietly filtering on the
  // wrong one is not an option available to it.
  const b = mount('arrange');
  b.columns(IRIS);
  b.pickNth(0, 'Species');
  b.columns(OTHER);
  assert.deepStrictEqual(b.compose().columns, [{ column: 'Species', direction: 'asc' }]);
  b.close();
});

test('picking again after the data changed follows the new data', () => {
  const b = mount('arrange');
  b.columns(IRIS);
  b.pickNth(0, 'Species');
  b.columns(OTHER);
  b.pickNth(0, 'b');
  assert.deepStrictEqual(b.compose().columns, [{ column: 'b', direction: 'asc' }]);
  assert.strictEqual(b.submits.length, 2, 'each pick is a submit');
  b.close();
});
