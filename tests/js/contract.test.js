/* Blocks on blockr.ui's block contract (inst/blocks): what they ask the host. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { mount } = require('./harness.js');

const IRIS = [
  { name: 'Sepal.Length', type: 'numeric', label: '' },
  { name: 'Species', type: 'character', label: '' }
];

test('filter asks the host for the values when the value list opens', async () => {
  const b = mount('filter');
  b.setState({
    conditions: [
      { type: 'values', column: 'Species', values: ['setosa'], mode: 'include', colType: 'character' }
    ],
    operator: '&',
    preserve_order: false
  });
  b.columns(IRIS);
  assert.deepStrictEqual(b.requests, []);
  // the second select of the row is its values
  b.selects()[1].querySelector('.blockr-select__control').click();

  const asked = b.requests.filter((r) => r.name === 'column_values');
  assert.ok(asked.length >= 1);
  assert.deepStrictEqual(asked[0].args, { input: 'data', column: 'Species', query: '' });

  await b.answer(b.requests.indexOf(asked[0]), {
    name: 'Species', type: 'character', label: '', hasNA: false,
    values: ['setosa', 'versicolor', 'virginica'], hasEmpty: false,
    total: 3, truncated: false
  });
  assert.deepStrictEqual(b.block.columnMeta.Species.values, ['setosa', 'versicolor', 'virginica']);
  assert.deepStrictEqual(b.compose().conditions[0].values, ['setosa']);
  assert.deepStrictEqual(b.submits, []);
  b.close();
});

test('select submits through the host on a pick', () => {
  const b = mount('select');
  b.setState({ columns: [], exclude: false, distinct: false });
  b.columns(IRIS);
  b.pickNth(0, 'Species');
  assert.deepStrictEqual(b.submits, [{ columns: ['Species'], exclude: false, distinct: false }]);
  b.close();
});

test('join waits for both inputs before taking columns', () => {
  const b = mount('join');
  b.setState({ type: 'left_join', keys: [], exprs: [], suffix_x: '.x', suffix_y: '.y' });
  b.context({ x: { columns: IRIS }, y: null });
  assert.deepStrictEqual(b.submits, []);
  b.context({ x: { columns: IRIS }, y: { columns: [{ name: 'Species', type: 'character', label: '' }] } });
  assert.strictEqual(b.submits.length, 1);
  b.close();
});
