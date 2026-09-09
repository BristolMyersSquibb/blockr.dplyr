/* The dropdown on its own: `Blockr.Select.menu`.
 *
 * A word in a block's sentence that IS one of its settings opens the list
 * directly. Before this the caller built a popover holding a single select
 * and clicked the control for the user, which was two boxes deep for a list
 * of six columns. The menu is the same widget with no control of its own, so
 * these tests are about the differences: it opens itself, it hangs off an
 * anchor the caller owns, it can lead with the label instead of the name, and
 * it tears itself down when it closes.
 *
 * happy-dom has no layout engine, so nothing here measures. Position is the
 * one thing these tests cannot check.
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

const anchorIn = (win, text) => {
  const a = win.document.createElement('span');
  a.textContent = text || 'AVAL';
  win.document.body.appendChild(a);
  return a;
};

const COLS = [
  '(none)',
  { value: 'AVAL', label: 'Analysis Value' },
  { value: 'CHG', label: 'Change from Baseline' },
  'BASE'
];

const dropdown = (win) =>
  win.document.querySelector('.blockr-select__dropdown');
const optionTexts = (win) =>
  [...win.document.querySelectorAll('.blockr-select__option')]
    .map((e) => e.textContent);

test('the menu opens itself: no control to click first', () => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(anchorIn(win), { options: COLS, selected: 'AVAL' });
  assert.ok(dropdown(win), 'a dropdown exists');
  assert.strictEqual(dropdown(win).style.display, 'block');
  // The control is never mounted, so there is no second box on screen.
  assert.strictEqual(win.document.querySelectorAll('.blockr-select__control').length, 0);
  m.close();
  win.close();
});

test('the title says which setting the word fills', () => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(anchorIn(win), {
    options: COLS, selected: 'AVAL', title: 'Colour'
  });
  const t = win.document.querySelector('.blockr-select__menu-title');
  assert.strictEqual(t.textContent, 'Colour');
  m.close();
  win.close();
});

test('the option leads with the half the sentence printed', () => {
  const byName = newWindow();
  const a = byName.Blockr.Select.menu(anchorIn(byName), { options: COLS, selected: 'AVAL' });
  assert.deepStrictEqual(optionTexts(byName), [
    '(none)', 'AVALAnalysis Value', 'CHGChange from Baseline', 'BASE'
  ]);
  a.close();
  byName.close();

  const byLabel = newWindow();
  const b = byLabel.Blockr.Select.menu(anchorIn(byLabel), {
    options: COLS, selected: 'AVAL', labelFirst: true
  });
  assert.deepStrictEqual(optionTexts(byLabel), [
    '(none)', 'Analysis ValueAVAL', 'Change from BaselineCHG', 'BASE'
  ]);
  b.close();
  byLabel.close();
});

test('the current option is marked, the same way every select marks it', () => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(anchorIn(win), { options: COLS, selected: 'CHG' });
  const on = win.document.querySelectorAll('.blockr-select__option--selected');
  assert.strictEqual(on.length, 1);
  assert.strictEqual(on[0].getAttribute('data-value'), 'CHG');
  m.close();
  win.close();
});

test('the filter box appears past the threshold, and holds focus below it', () => {
  const short = newWindow();
  const a = short.Blockr.Select.menu(anchorIn(short), { options: COLS, selected: 'AVAL' });
  const hidden = short.document.querySelector('.blockr-select__search--menu');
  assert.ok(hidden, 'the input is always mounted: it is what the arrows type into');
  assert.ok(hidden.className.includes('blockr-select__search--offscreen'));
  a.close();
  short.close();

  const long = newWindow();
  const many = Array.from({ length: 12 }, (_, i) => `COL${i}`);
  const b = long.Blockr.Select.menu(anchorIn(long), { options: many, selected: 'COL0' });
  const shown = long.document.querySelector('.blockr-select__search--menu');
  assert.ok(!shown.className.includes('blockr-select__search--offscreen'));
  b.close();
  long.close();
});

test('typing filters without losing the input', () => {
  const win = newWindow();
  const many = Array.from({ length: 12 }, (_, i) => `COL${i}`);
  const m = win.Blockr.Select.menu(anchorIn(win), { options: many, selected: 'COL0' });
  const input = win.document.querySelector('.blockr-select__search--menu');
  input.value = 'COL1';
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
  // COL1, COL10, COL11 -- and the input is still in the panel, which is what
  // clearing only the options below the head buys.
  assert.deepStrictEqual(optionTexts(win), ['COL1', 'COL10', 'COL11']);
  assert.strictEqual(input.parentElement, dropdown(win));
  m.close();
  win.close();
});

test('a pick reports the value and takes the menu away', () => {
  const win = newWindow();
  const picked = [];
  let closed = 0;
  win.Blockr.Select.menu(anchorIn(win), {
    options: COLS,
    selected: 'AVAL',
    onChange: (v) => picked.push(v),
    onClose: () => { closed++; }
  });
  const chg = [...win.document.querySelectorAll('.blockr-select__option')]
    .find((e) => e.getAttribute('data-value') === 'CHG');
  chg.dispatchEvent(new win.Event('click', { bubbles: true }));
  assert.deepStrictEqual(picked, ['CHG']);
  return new Promise((resolve) => {
    // Teardown is deferred by a tick on purpose: close() runs before the
    // option's own onChange, and destroying synchronously would pull the DOM
    // out from under it.
    win.setTimeout(() => {
      assert.strictEqual(closed, 1);
      assert.strictEqual(win.document.querySelectorAll('.blockr-select__dropdown').length, 0);
      assert.strictEqual(win.document.querySelectorAll('.blockr-select-menu-host').length, 0);
      win.close();
      resolve();
    }, 0);
  });
});

test('a click on the anchor is the caller\'s toggle, not an outside click', () => {
  const win = newWindow();
  const anchor = anchorIn(win);
  let closed = 0;
  const m = win.Blockr.Select.menu(anchor, {
    options: COLS, selected: 'AVAL', onClose: () => { closed++; }
  });
  anchor.dispatchEvent(new win.Event('click', { bubbles: true }));
  assert.strictEqual(closed, 0, 'the menu stays: closing here would have it reopen on the same click');
  assert.ok(dropdown(win), 'and it is still on screen');
  // Anywhere else does close it. Teardown is deferred by a tick, as above.
  const elsewhere = win.document.createElement('div');
  win.document.body.appendChild(elsewhere);
  elsewhere.dispatchEvent(new win.Event('click', { bubbles: true }));
  return new Promise((resolve) => {
    win.setTimeout(() => {
      assert.strictEqual(closed, 1);
      m.close();
      win.close();
      resolve();
    }, 0);
  });
});
