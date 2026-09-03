/* The one-row multi select.
 *
 * A multi select in the auto-generated parameter bands (function block, code
 * block) used to wrap: every tag past the first row added 24px, and since the
 * grid sizes a row to its tallest field, one seven-tag pick made the whole
 * band three rows tall. `singleLine` keeps the tags on one row and counts the
 * rest on a "+N" chip.
 *
 * The arithmetic is `Blockr.Select.fitCount`, which is what these tests drive:
 * happy-dom has no layout engine, so every measured width here is 0 and the
 * DOM half can only be checked for the case that matters most anyway — a
 * control that has not been laid out yet must show every tag, not hide them
 * all against a zero width.
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const assert = require('node:assert');
const { Window } = require('happy-dom');

const JS_DIR = path.join(__dirname, '..', '..', 'inst', 'js');
const read = (f) => fs.readFileSync(path.join(JS_DIR, f), 'utf8');

const newWindow = () => {
  const win = new Window({ url: 'http://localhost/' });
  win.eval(read('blockr-core.js'));
  win.eval(read('blockr-select.js'));
  return win;
};

const GAP = 3;

test('fitCount: everything that fits stays', () => {
  const win = newWindow();
  const fit = win.Blockr.Select.fitCount;
  // 3 * 40 + 2 gaps = 126
  assert.strictEqual(fit([40, 40, 40], 200, GAP, 24), 3);
  assert.strictEqual(fit([40, 40, 40], 126, GAP, 24), 3);
  win.close();
});

test('fitCount: the chip has to fit too', () => {
  const win = newWindow();
  const fit = win.Blockr.Select.fitCount;
  // Two tags plus their gap is 83; a third would need 126, so it drops. The
  // chip then needs 3 + 24 on top of 83 = 110, which 126 still allows.
  assert.strictEqual(fit([40, 40, 40], 125, GAP, 24), 2);
  // At 100 the second tag no longer leaves room for the chip either.
  assert.strictEqual(fit([40, 40, 40], 100, GAP, 24), 1);
  win.close();
});

test('fitCount: a control narrower than one tag shows the chip alone', () => {
  const win = newWindow();
  const fit = win.Blockr.Select.fitCount;
  assert.strictEqual(fit([40, 40], 30, GAP, 24), 0);
  win.close();
});

test('fitCount: no tags, no chip', () => {
  const win = newWindow();
  const fit = win.Blockr.Select.fitCount;
  assert.strictEqual(fit([], 200, GAP, 24), 0);
  win.close();
});

test('singleLine marks the root and leaves an unlaid-out control alone', () => {
  const win = newWindow();
  const doc = win.document;
  doc.body.innerHTML = '<div id="host"></div>';

  const sel = win.Blockr.Select.multi(doc.getElementById('host'), {
    options: ['AGE', 'DSDIAG', 'ETHNIC', 'RACE', 'REGION', 'SEX', 'TRT'],
    selected: ['AGE', 'DSDIAG', 'ETHNIC', 'RACE', 'REGION', 'SEX', 'TRT'],
    singleLine: true
  });

  assert.ok(sel.el.classList.contains('blockr-select--single-line'));

  // Zero width means "not laid out yet" (a deferred dock panel, a hidden tab),
  // not "nothing fits": every tag stays visible and the chip stays away until
  // the ResizeObserver delivers a real width.
  const tags = Array.from(sel.el.querySelectorAll('.blockr-select__tag'));
  assert.strictEqual(tags.length, 7);
  assert.strictEqual(tags.filter((t) => t.classList.contains('blockr-select__tag--hidden')).length, 0);
  const chip = sel.el.querySelector('.blockr-select__more');
  assert.ok(chip === null || chip.style.display === 'none');

  sel.destroy();
  win.close();
});

test('a multi select without singleLine is untouched', () => {
  const win = newWindow();
  const doc = win.document;
  doc.body.innerHTML = '<div id="host"></div>';
  const sel = win.Blockr.Select.multi(doc.getElementById('host'), {
    options: ['a', 'b'],
    selected: ['a', 'b']
  });
  assert.ok(!sel.el.classList.contains('blockr-select--single-line'));
  assert.strictEqual(sel.el.querySelector('.blockr-select__more'), null);
  sel.destroy();
  win.close();
});
