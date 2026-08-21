/* The restore contract, for every block that has one.
 *
 * A board hands a block back its saved state as a `<name>-block-update`
 * message and its column choices as a `<name>-columns` message, in an order
 * nobody controls: both are Shiny observers, and the columns come from
 * `data()`, which is whatever the upstream has managed to produce so far.
 * What a block must guarantee across all of that:
 *
 *   1. round trip     — what R sent is what the block would send back
 *   2. order-free     — state-then-columns and columns-then-state agree
 *   3. silence        — neither message is a user edit, so neither submits
 *   4. no clobber     — a column list from the wrong (still settling) frame
 *                       does not rewrite the state, and the right one
 *                       restores the block intact
 *
 * These are deliberately written against `_compose()` — the JSON R actually
 * receives — and not against block internals, so they keep their meaning
 * through a refactor of how any given block stores its rows.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { mount } = require('./harness.js');

const IRIS = ['Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Species'];
/* The frame an upstream that has not restored yet still shows. */
const STALE = ['x', 'y', 'z'];

/**
 * One entry per block: a state worth restoring, and the columns it names.
 *
 * `roundTrip: false` marks a block whose `_compose()` is not the identity on
 * its own state — noted per block, not waved away.
 */
const BLOCKS = [
  {
    name: 'arrange',
    state: { columns: [{ column: 'Species', direction: 'desc' }] }
  },
  {
    name: 'rename',
    state: { renames: { species_name: 'Species', width: 'Sepal.Width' } }
  },
  {
    name: 'select',
    state: { columns: ['Species', 'Sepal.Length'], exclude: true, distinct: false }
  },
  {
    name: 'separate',
    state: {
      col: 'Species', into: ['genus', 'epithet'], sep: '_',
      remove: true, convert: false
    }
  },
  {
    name: 'unite',
    state: {
      col: 'joined', cols: ['Species', 'Sepal.Length'], sep: '-',
      remove: false, na_rm: true
    }
  },
  {
    name: 'filter',
    state: {
      conditions: [
        { type: 'values', column: 'Species', values: ['setosa'], mode: 'exclude', colType: 'character' }
      ],
      operator: '&',
      preserve_order: false
    }
  },
  {
    name: 'pivot-longer',
    state: {
      cols: ['Sepal.Length', 'Sepal.Width'], names_to: 'measure',
      values_to: 'cm', values_drop_na: true, names_prefix: 'Sepal.'
    }
  },
  {
    name: 'pivot-wider',
    state: {
      names_from: ['Species'], values_from: ['Sepal.Length'], id_cols: [],
      values_fill: null, names_sep: '.', names_prefix: 'v_', values_fn: 'mean'
    }
  },
  {
    name: 'slice',
    state: {
      type: 'max', n: 3, prop: null, order_by: 'Sepal.Length', with_ties: false,
      // '' is not a value slice keeps: a cleared rows field means "use the
      // default", and _compose() sends the default back.
      weight_by: '', replace: false, rows: '1:5', by: ['Species']
    }
  },
  {
    name: 'summarize',
    state: {
      summaries: [{ type: 'simple', name: 'avg', func: 'mean', col: 'Sepal.Length' }],
      by: ['Species']
    }
  },
  {
    name: 'mutate',
    state: {
      mutations: [{ name: 'ratio', expr: 'Sepal.Length / Sepal.Width' }],
      by: ['Species']
    }
  }
];

/** The order R pushes in: state first, then columns (js-block.R priorities). */
const restore = (name, state, columns = IRIS) => {
  const b = mount(name);
  b.setState(state);
  b.columns(columns);
  return b;
};

for (const { name, state } of BLOCKS) {
  test(`${name}: state round-trips`, () => {
    const b = restore(name, state);
    assert.deepStrictEqual(b.compose(), state);
    b.close();
  });

  test(`${name}: the two message orders agree`, () => {
    const first = restore(name, state);

    const second = mount(name);
    second.columns(IRIS);
    second.setState(state);

    assert.deepStrictEqual(second.compose(), first.compose());
    first.close();
    second.close();
  });

  test(`${name}: a restore is silent`, () => {
    // A submit here would write the client's reading of the state back over
    // R's own: `js_block_state()` decomposes an input blob straight into the
    // per-field reactiveVals, and arms `self_write` so the correcting echo
    // never comes. Restoring a board must not talk back.
    const b = restore(name, state);
    assert.deepStrictEqual(b.submits, []);
    assert.strictEqual(b.value(), null);
    b.close();
  });

  test(`${name}: a column list from a still-settling upstream does not rewrite it`, () => {
    const b = mount(name);
    b.setState(state);
    b.columns(STALE);
    b.columns(IRIS);
    assert.deepStrictEqual(b.compose(), state);
    assert.deepStrictEqual(b.submits, []);
    b.close();
  });

  test(`${name}: the restored state survives even if the columns win the race`, () => {
    // R orders its pushes so this does not happen (js-block.R), but the
    // client must not depend on that: a block that has already read a column
    // list still has to take the state that follows.
    const b = mount(name);
    b.columns(IRIS);
    b.setState(state);
    assert.deepStrictEqual(b.compose(), state);
    b.close();
  });

  test(`${name}: a repeated column list does not rewrite it either`, () => {
    // The announce path re-sends both messages, so every block sees its
    // columns at least twice on a deferred dock panel.
    const b = restore(name, state);
    b.columns(IRIS);
    assert.deepStrictEqual(b.compose(), state);
    assert.deepStrictEqual(b.submits, []);
    b.close();
  });
}

/* --- join: two inputs, so two of everything -------------------------- */

const JOIN_STATE = {
  type: 'left',
  keys: [{ xCol: 'Species', op: '==', yCol: 'name' }],
  exprs: [],
  suffix_x: '.x',
  suffix_y: '.y'
};

test('join: state round-trips once both column lists are known', () => {
  const b = mount('join');
  b.joinColumns(IRIS, ['name', 'rank']);
  b.setState(JOIN_STATE);
  assert.deepStrictEqual(b.compose(), JOIN_STATE);
  b.close();
});

test('join: a key survives column lists that do not offer its columns', () => {
  // Either input can still be settling, so a key row has to hold both of its
  // columns through a list that carries neither. A join that quietly re-points
  // at column 1 on each side joins on the wrong thing and produces a table
  // that looks plausible.
  const b = mount('join');
  b.setState(JOIN_STATE);
  b.joinColumns(STALE, STALE);
  assert.deepStrictEqual(b.compose().keys, JOIN_STATE.keys);

  b.joinColumns(IRIS, ['name', 'rank']);
  assert.deepStrictEqual(b.compose(), JOIN_STATE);
  assert.deepStrictEqual(b.submits, []);
  b.close();
});

test('join: one side settling does not disturb the other', () => {
  const b = mount('join');
  b.setState(JOIN_STATE);
  b.joinColumns(IRIS, STALE);
  assert.deepStrictEqual(b.compose().keys, JOIN_STATE.keys);
  b.close();
});

test('join: an unpicked key still follows the data', () => {
  const b = mount('join');
  b.joinColumns(IRIS, ['name', 'rank']);
  const auto = b.compose().keys;
  assert.deepStrictEqual(auto, [{ xCol: 'Sepal.Length', op: '==', yCol: 'name' }]);

  b.joinColumns(STALE, STALE);
  assert.deepStrictEqual(b.compose().keys, [{ xCol: 'x', op: '==', yCol: 'x' }]);
  b.close();
});

/* --- when a block IS allowed to talk back ---------------------------- */

test('a fresh summarize tells R about the column it picked for itself', () => {
  // The other side of "a restore is silent": a block that auto-picks has to
  // submit, or it shows a configured-looking row that R knows nothing about.
  const b = mount('summarize');
  b.columns(IRIS);
  assert.strictEqual(b.submits.length, 1);
  assert.deepStrictEqual(b.submits[0].summaries, [
    { type: 'simple', name: 'mean_Sepal.Length', func: 'mean', col: 'Sepal.Length' }
  ]);
  b.close();
});

test('a fresh join tells R about the keys it picked for itself', () => {
  const b = mount('join');
  b.joinColumns(IRIS, ['name', 'rank']);
  assert.strictEqual(b.submits.length, 1);
  assert.deepStrictEqual(b.submits[0].keys, [
    { xCol: 'Sepal.Length', op: '==', yCol: 'name' }
  ]);
  b.close();
});

test('summarize answers when it had to generate a name R did not send', () => {
  // A normalization the client applies is news; R has to store the name it
  // will see in the output.
  const b = mount('summarize');
  b.columns(IRIS);
  b.submits.length = 0;
  b.setState({ summaries: [{ type: 'simple', name: '', func: 'mean', col: 'Petal.Length' }], by: [] });
  assert.deepStrictEqual(b.submits, [{
    summaries: [{ type: 'simple', name: 'mean_Petal.Length', func: 'mean', col: 'Petal.Length' }],
    by: []
  }]);
  b.close();
});

test('mutate does not answer a restore with a group-by it has not applied yet', () => {
  // The submit used to fire halfway through setState, before `by` was
  // restored, so it shipped the previous (empty) by — and R writes an input
  // blob straight into its per-field reactiveVals, so the board's group-by
  // was gone with no way back.
  const b = mount('mutate');
  b.columns(IRIS);
  b.submits.length = 0;
  b.setState({ mutations: [{ name: 'ratio', expr: 'Sepal.Length / Sepal.Width' }], by: ['Species'] });
  for (const submitted of b.submits) {
    assert.deepStrictEqual(submitted.by, ['Species'], 'submitted a stale group-by');
  }
  b.close();
});

test('a single group-by column arrives unboxed and is still not news', () => {
  // jsonlite auto-unboxes, so R's `by = "Species"` reaches the client as a
  // bare string while `_compose()` holds `["Species"]`. Those are the same
  // value, and one group-by column is the ordinary case -- keying them apart
  // made the guard fire on almost every real restore, echoing the state
  // straight back and re-arming `self_write` against the next genuine push.
  for (const name of ['mutate', 'summarize']) {
    const rows = name === 'mutate'
      ? { mutations: [{ name: 'ratio', expr: 'Sepal.Length / Sepal.Width' }] }
      : { summaries: [{ type: 'simple', name: 'm', func: 'mean', col: 'Petal.Length' }] };
    const b = mount(name);
    b.columns(IRIS);
    b.submits.length = 0;
    b.setState(Object.assign({}, rows, { by: 'Species' }));
    assert.deepStrictEqual(b.submits, [], `${name} answered an unboxed restore`);
    assert.deepStrictEqual(b.compose().by, ['Species'], `${name} lost the group-by`);
    b.close();
  }
});

/* --- blocks with no columns of their own ----------------------------- */

test('bind-rows: state round-trips (it has no column pickers to lose)', () => {
  const b = mount('bind-rows');
  b.setState({ id_name: 'source' });
  assert.deepStrictEqual(b.compose(), { id_name: 'source' });
  assert.deepStrictEqual(b.submits, []);
  b.close();
});

/* --- the announce handshake ------------------------------------------ */

test('a block with nothing queued announces itself so R re-sends', () => {
  // blockr.core#317: on a deferred dock panel the state and columns pushes
  // are dropped before this JS exists, and no client-side queue can catch a
  // message dropped before the queue loaded. The announce is the only way
  // back — see js_block_ready_name().
  const b = mount('arrange');
  assert.strictEqual(b.readyAnnouncements().length, 1);
  b.close();
});

test('a block whose pushes were queued does not announce', () => {
  // Messages that arrive before the element binds are parked by
  // `Blockr._enqueue` and replayed on initialize; asking R to re-send on top
  // of that is a wasted round trip on every restored board.
  const b = mount('arrange', { id: 'arrange_input' });
  b.close();

  const queued = mount('arrange', {
    id: 'arrange_input',
    beforeInit: (send) => send('arrange-block-update', {
      state: { columns: [{ column: 'Species', direction: 'asc' }] }
    })
  });
  assert.deepStrictEqual(queued.readyAnnouncements(), []);
  assert.deepStrictEqual(
    queued.compose().columns, [{ column: 'Species', direction: 'asc' }],
    'the queued state was not replayed on bind'
  );
  queued.close();
});
