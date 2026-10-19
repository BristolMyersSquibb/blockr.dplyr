/* filter's state is the filter step of the block IR; its UI rows are read
 * from it and written back (inst/blocks/filter/filter.js, toIR/fromIR). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { mount } = require('./harness.js');

const json = (x) => JSON.parse(JSON.stringify(x));

const ir = () => {
  const b = mount('filter');
  const def = b.win.Blockr.blocks.filter;
  return { b, toIR: (ui) => json(def.ir.toIR(ui, new Map())), fromIR: (s) => json(def.ir.fromIR(s, new Map())) };
};

const ROWS = [
  { type: 'values', column: 'Species', values: ['setosa', '<NA>'], mode: 'include', colType: 'character' },
  { type: 'values', column: 'cyl', values: ['4', '6'], mode: 'exclude', colType: 'numeric' },
  { type: 'numeric', column: 'mpg', op: '>=', value: 20 },
  { type: 'expr', expr: 'lubridate::year(d) > 2020' }
];

test('rows round-trip through IR, with either operator', () => {
  const { b, toIR, fromIR } = ir();
  for (const operator of ['&', '|']) {
    const state = toIR({ conditions: ROWS, operator, preserve_order: false });
    const back = fromIR(state);
    assert.strictEqual(back.operator, operator);
    assert.deepStrictEqual(back.conditions, ROWS);
  }
  b.close();
});

test('picked values are typed in the IR', () => {
  const { b, toIR } = ir();
  const s = toIR({ conditions: [ROWS[1]], operator: '&' });
  // cyl %in% c(4, 6), negated: numbers, not strings
  assert.deepStrictEqual(s.pred[0], { call: '!', args: [{ call: '%in%', args: [{ col: 'cyl' }, { list: [4, 6] }] }] });
  // "007" on a character column stays a string
  const t = toIR({ conditions: [{ type: 'values', column: 'id', values: ['007'], mode: 'include', colType: 'character' }] });
  assert.deepStrictEqual(t.pred[0].args[1], { list: ['007'] });
  b.close();
});

test('expression rows are code nodes, kept as typed', () => {
  const { b, toIR } = ir();
  const s = toIR({ conditions: [ROWS[3]], operator: '&' });
  assert.deepStrictEqual(s.pred, [{ code: 'lubridate::year(d) > 2020', lang: 'r' }]);
  b.close();
});

test('IR the rows cannot draw shows as an expression and writes back unchanged', () => {
  const b = mount('filter');
  const def = b.win.Blockr.blocks.filter;
  const known = new Map();
  const between = { call: 'between', args: [{ col: 'mpg' }, { lit: 10 }, { lit: 20 }] };
  const ui = def.ir.fromIR({ pred: [between], preserve_order: false }, known);
  assert.strictEqual(json(ui).conditions[0].type, 'expr');
  assert.strictEqual(json(ui).conditions[0].expr, 'dplyr::between(mpg, 10, 20)');
  assert.deepStrictEqual(json(def.ir.toIR(ui, known)).pred, [between]);
  b.close();
});

test('the host gets IR when the user picks a value', () => {
  const b = mount('filter');
  b.columns([{ name: 'Species', type: 'character', label: '' }]);
  b.block._addValueRow('Species', { values: ['setosa'], op: 'is', colType: 'character' });
  b.block._submit();
  const last = b.submits[b.submits.length - 1];
  assert.deepStrictEqual(last.pred, [{ call: '%in%', args: [{ col: 'Species' }, { list: ['setosa'] }] }]);
  b.close();
});

test('removing an expression row is submitted', () => {
  const b = mount('filter');
  b.block.setState({ conditions: [{ type: 'expr', expr: 'x > 1' }, { type: 'expr', expr: 'y > 2' }], operator: '&' });
  b.el.querySelectorAll('.blockr-row-remove')[1].click();
  assert.deepStrictEqual(b.submits[b.submits.length - 1], { pred: [{ code: 'x > 1', lang: 'r' }], preserve_order: false });
  b.close();
});
