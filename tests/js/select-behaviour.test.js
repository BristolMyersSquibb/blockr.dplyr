/* What Blockr.Select does, seen from a caller.
 *
 * The checklist in dev/select-rewrite-plan.md, one test per line: what
 * getValue() returns, when onChange fires, which rows the list shows, which
 * row carries the keyboard highlight, which classes and ARIA attributes are
 * set. Nothing here reads private state; during the rewrite the same suite
 * ran against the previous implementation too (select-impls.js), and the
 * tests marked "Deliberate difference" are where the two parted.
 *
 * happy-dom has no layout engine: every measured width is 0 and nothing
 * scrolls. Positioning is checked in a browser (dev/select-playground.html);
 * the single-line fit is driven here through a stubbed layout, since the
 * arithmetic is the same whichever numbers feed it.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./select-impls');

/* --- helpers -------------------------------------------------------------- */

const host = (win) => {
  const el = win.document.createElement('div');
  win.document.body.appendChild(el);
  return el;
};

const single = (win, cfg) => win.Blockr.Select.single(host(win), cfg || {});
const multi = (win, cfg) => win.Blockr.Select.multi(host(win), cfg || {});

const search = (sel) => sel.el.querySelector('.blockr-select__search');
const control = (sel) => sel.el.querySelector('.blockr-select__control');
const face = (sel) => sel.el.querySelector('.blockr-select__value');
const isOpen = (sel) => sel.el.classList.contains('blockr-select--open');

/* The list is portalled to <body> while open, so find it by the id the
 * combobox input points at, not by walking down from the root. */
const dropdown = (win, sel) =>
  win.document.getElementById(search(sel).getAttribute('aria-controls'));

const rowsIn = (dd) => [...dd.querySelectorAll('.blockr-select__option')];
const rows = (win, sel) => rowsIn(dropdown(win, sel)).map((r) => r.getAttribute('data-value'));
const highlighted = (win, sel) => {
  const r = dropdown(win, sel).querySelector('.blockr-select__option--highlighted');
  return r ? r.getAttribute('data-value') : null;
};
const emptyTexts = (win, sel) =>
  [...dropdown(win, sel).querySelectorAll('.blockr-select__empty')].map((e) => e.textContent);
const tags = (sel) =>
  [...sel.el.querySelectorAll('.blockr-select__tag')].map((t) => t.getAttribute('data-value'));

/* Arrays built inside the window have that realm's Array prototype, which
 * deepStrictEqual counts as a difference. JSON is what goes on the wire. */
const json = (x) => JSON.parse(JSON.stringify(x));
const values = (sel) => json(sel.getValue());

const click = (win, el) => el.dispatchEvent(new win.MouseEvent('click', { bubbles: true }));
const clickOutside = (win) => click(win, win.document.body);

/* A key the way a browser delivers it: keydown first, and when nothing
 * prevented it, a printable key lands in the input and fires `input`. */
const press = (win, input, key) => {
  const e = new win.KeyboardEvent('keydown', { key, bubbles: true, cancelable: true });
  input.dispatchEvent(e);
  if (!e.defaultPrevented && key.length === 1) {
    input.value += key;
    input.dispatchEvent(new win.Event('input', { bubbles: true }));
  }
  return e;
};
const type = (win, input, text) => {
  input.value = text;
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
};
const clickRow = (win, sel, value) => {
  const row = rowsIn(dropdown(win, sel)).find((r) => r.getAttribute('data-value') === value);
  assert.ok(row, `"${value}" is listed`);
  click(win, row);
};

const ABC = ['a', 'b', 'c', 'd', 'e'];
const LABELLED = [
  { value: 'AVAL', label: 'Analysis Value' },
  { value: 'CHG', label: 'Change from Baseline' },
  'BASE'
];
const wait = (win, ms) => new Promise((resolve) => win.setTimeout(resolve, ms));

/* --- opening and closing -------------------------------------------------- */

test('a single select opens on a click on its control and closes on the next', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC });
  assert.ok(!isOpen(sel));
  click(win, control(sel));
  assert.ok(isOpen(sel));
  assert.strictEqual(dropdown(win, sel).parentElement, win.document.body, 'portalled to body');
  assert.strictEqual(dropdown(win, sel).style.display, 'block');
  click(win, control(sel));
  assert.ok(!isOpen(sel));
  assert.strictEqual(rows(win, sel).length, 0, 'the list is emptied on close');
  win.close();
});

test('a multi select opens on a click and keeps focus in its search input', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC });
  click(win, control(sel));
  assert.ok(isOpen(sel));
  assert.strictEqual(win.document.activeElement, search(sel));
  click(win, control(sel));
  assert.ok(isOpen(sel), 'a second click on a multi does not toggle it shut');
  assert.strictEqual(win.document.activeElement, search(sel));
  win.close();
});

test('a click on a tag\'s x while closed removes it without opening', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = multi(win, { options: ABC, selected: ['a', 'b'], onChange: (v) => seen.push(v) });
  click(win, sel.el.querySelector('.blockr-select__tag[data-value="a"] .blockr-select__tag-remove'));
  assert.deepStrictEqual(tags(sel), ['b']);
  assert.deepStrictEqual(json(seen), [['b']]);
  assert.ok(!isOpen(sel));
  win.close();
});

for (const key of ['Enter', ' ', 'ArrowDown', 'ArrowUp']) {
  test(`the keyboard opens the list with ${JSON.stringify(key)}`, (newWindow) => {
    const win = newWindow();
    const sel = single(win, { options: ABC, selected: 'c' });
    search(sel).focus();
    press(win, search(sel), key);
    assert.ok(isOpen(sel));
    win.close();
  });
}

test('Escape closes', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC });
  click(win, control(sel));
  press(win, search(sel), 'Escape');
  assert.ok(!isOpen(sel));
  win.close();
});

/* Deliberate difference (select-rewrite-plan.md, "Questions"): the reference
 * called root.focus() on Escape. In a browser that is a no-op (the root has
 * no tabindex) and focus stays in the input by accident; happy-dom focuses
 * any element, so the reference fails this here. The rewrite leaves focus
 * on the input, which is the combobox. */
test('Escape leaves focus on the combobox input', (newWindow, t) => {
  const win = newWindow();
  const sel = single(win, { options: ABC });
  click(win, control(sel));
  assert.strictEqual(win.document.activeElement, search(sel));
  press(win, search(sel), 'Escape');
  assert.strictEqual(win.document.activeElement, search(sel));
  win.close();
});

/* Deliberate difference: a menu's filter box is torn down with the menu, so
 * after Escape nothing had focus. It now returns to the anchor when it can
 * take it, and only then; a closing click that landed on another focusable
 * element keeps its focus. */
test('a menu closed by Escape hands focus back to its anchor', async (newWindow, t) => {
  const win = newWindow();
  const anchor = win.document.createElement('button');
  win.document.body.appendChild(anchor);
  win.Blockr.Select.menu(anchor, { options: ABC });
  const box = win.document.querySelector('.blockr-select__search');
  assert.strictEqual(win.document.activeElement, box);
  press(win, box, 'Escape');
  await wait(win, 0);
  assert.strictEqual(win.document.activeElement, anchor);

  const other = win.document.createElement('input');
  win.document.body.appendChild(other);
  win.Blockr.Select.menu(anchor, { options: ABC });
  other.focus();
  click(win, other);
  await wait(win, 0);
  assert.strictEqual(win.document.activeElement, other, 'a click elsewhere keeps its focus');
  win.close();
});

test('Tab closes', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC });
  click(win, control(sel));
  press(win, search(sel), 'Tab');
  assert.ok(!isOpen(sel));
  win.close();
});

test('a click outside closes; one inside the list does not', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC });
  click(win, control(sel));
  click(win, dropdown(win, sel));
  assert.ok(isOpen(sel), 'the list is not outside');
  clickOutside(win);
  assert.ok(!isOpen(sel));
  win.close();
});

test('a click on a menu\'s anchor is the caller\'s toggle, not an outside click', async (newWindow) => {
  const win = newWindow();
  const anchor = host(win);
  let closed = 0;
  win.Blockr.Select.menu(anchor, { options: ABC, onClose: () => { closed++; } });
  click(win, anchor);
  await wait(win, 0);
  assert.strictEqual(closed, 0);
  assert.ok(win.document.querySelector('.blockr-select__dropdown'), 'still on screen');
  clickOutside(win);
  await wait(win, 0);
  assert.strictEqual(closed, 1);
  win.close();
});

test('onOpen fires on every open; onClose after the DOM is settled', (newWindow) => {
  const win = newWindow();
  let opened = 0;
  const closedWith = [];
  const sel = single(win, {
    options: ABC,
    onOpen: () => { opened++; },
    onClose: () => {
      closedWith.push({
        open: isOpen(sel),
        rows: rows(win, sel).length,
        display: dropdown(win, sel).style.display
      });
    }
  });
  click(win, control(sel));
  click(win, control(sel));
  click(win, control(sel));
  press(win, search(sel), 'Escape');
  assert.strictEqual(opened, 2);
  assert.deepStrictEqual(closedWith, [
    { open: false, rows: 0, display: '' },
    { open: false, rows: 0, display: '' }
  ]);
  win.close();
});

test('a menu is open from creation and tears itself down a tick after closing', async (newWindow) => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(host(win), { options: ABC });
  const dd = win.document.querySelector('.blockr-select__dropdown');
  assert.ok(dd && dd.style.display === 'block');
  m.close();
  assert.ok(win.document.querySelector('.blockr-select-menu-host'), 'still there for the closing click');
  await wait(win, 0);
  assert.strictEqual(win.document.querySelector('.blockr-select-menu-host'), null);
  assert.strictEqual(win.document.querySelector('.blockr-select__dropdown'), null);
  win.close();
});

/* --- the list ------------------------------------------------------------- */

test('a multi lists only what is not picked', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC, selected: ['b', 'd'] });
  click(win, control(sel));
  assert.deepStrictEqual(rows(win, sel), ['a', 'c', 'e']);
  win.close();
});

test('a single lists every option and marks the pick', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC, selected: 'c' });
  click(win, control(sel));
  const dd = dropdown(win, sel);
  assert.strictEqual(dd.getAttribute('role'), 'listbox');
  assert.deepStrictEqual(rows(win, sel), ABC);
  const marked = rowsIn(dd).filter((r) => r.classList.contains('blockr-select__option--selected'));
  assert.deepStrictEqual(marked.map((r) => r.getAttribute('data-value')), ['c']);
  assert.deepStrictEqual(
    rowsIn(dd).map((r) => r.getAttribute('aria-selected')),
    ['false', 'false', 'true', 'false', 'false']
  );
  assert.ok(rowsIn(dd).every((r) => r.getAttribute('role') === 'option' && r.id));
  win.close();
});

test('typing filters by value and by label, case-insensitive', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: LABELLED });
  click(win, control(sel));
  type(win, search(sel), 'analysis');
  assert.deepStrictEqual(rows(win, sel), ['AVAL']);
  type(win, search(sel), 'ch');
  assert.deepStrictEqual(rows(win, sel), ['CHG']);
  type(win, search(sel), 'BA');
  assert.deepStrictEqual(rows(win, sel), ['CHG', 'BASE'], 'Change from BAseline and BAse');
  type(win, search(sel), '');
  assert.deepStrictEqual(rows(win, sel), ['AVAL', 'CHG', 'BASE']);
  win.close();
});

test('numeric values are stringified for the filter', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: [1, 10, 25] });
  click(win, control(sel));
  type(win, search(sel), '1');
  assert.deepStrictEqual(rows(win, sel), ['1', '10']);
  win.close();
});

test('typing into a closed select opens it', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC });
  type(win, search(sel), 'b');
  assert.ok(isOpen(sel));
  win.close();
});

/* Deliberate difference: the reference's open() wiped the query, so the
 * letter that opened a closed select was thrown away and the list came up
 * unfiltered. The rewrite keeps it. */
test('typing into a closed select keeps the typed letter as the filter', (newWindow, t) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC });
  type(win, search(sel), 'b');
  assert.strictEqual(search(sel).value, 'b');
  assert.deepStrictEqual(rows(win, sel), ['b']);
  win.close();
});

/* Deliberate difference: the reference handled Space only on the root, which
 * cannot take focus; a space typed into the input opened the list filtered
 * by " ". The rewrite opens a closed select on Space without typing it. */
test('Space opens a closed select without typing a space', (newWindow, t) => {
  const win = newWindow();
  const sel = single(win, { options: ABC, selected: 'c' });
  press(win, search(sel), ' ');
  assert.ok(isOpen(sel));
  assert.strictEqual(search(sel).value, '');
  assert.deepStrictEqual(rows(win, sel), ABC);
  assert.strictEqual(highlighted(win, sel), 'c');
  press(win, search(sel), ' ');
  assert.strictEqual(search(sel).value, ' ', 'once open, a space is typing');
  win.close();
});

test('a menu shows its filter box past 8 options, never with search: false, and holds focus either way', (newWindow) => {
  const win = newWindow();
  const boxOf = (m) => {
    const boxes = win.document.querySelectorAll('.blockr-select__search--menu');
    const box = boxes[boxes.length - 1];
    const out = {
      shown: !box.classList.contains('blockr-select__search--offscreen'),
      focused: win.document.activeElement === box
    };
    m.close();
    return out;
  };
  const nine = ABC.concat(['f', 'g', 'h', 'i']);
  assert.deepStrictEqual(boxOf(win.Blockr.Select.menu(host(win), { options: ABC })),
    { shown: false, focused: true });
  assert.deepStrictEqual(boxOf(win.Blockr.Select.menu(host(win), { options: nine })),
    { shown: true, focused: true });
  assert.deepStrictEqual(boxOf(win.Blockr.Select.menu(host(win), { options: nine, search: false })),
    { shown: false, focused: true });
  win.close();
});

test('at most 200 rows are rendered, and a row counts the rest', (newWindow) => {
  const win = newWindow();
  const many = Array.from({ length: 250 }, (_, i) => `v${i}`);
  const sel = single(win, { options: many });
  click(win, control(sel));
  assert.strictEqual(rows(win, sel).length, 200);
  assert.deepStrictEqual(emptyTexts(win, sel), ['+50 more — type to narrow']);
  type(win, search(sel), 'v24');
  // v24, v240..v249: narrowed under the cap, so the row goes away.
  assert.strictEqual(rows(win, sel).length, 11);
  assert.deepStrictEqual(emptyTexts(win, sel), []);
  win.close();
});

test('empty states: Loading, No matches, All selected, No options', (newWindow) => {
  const win = newWindow();
  const loading = single(win, { options: ABC, loading: true });
  click(win, control(loading));
  assert.deepStrictEqual(emptyTexts(win, loading), ['Loading…']);
  assert.deepStrictEqual(rows(win, loading), []);
  loading.setLoading(false);
  assert.deepStrictEqual(rows(win, loading), ABC);
  type(win, search(loading), 'zzz');
  assert.deepStrictEqual(emptyTexts(win, loading), ['No matches']);

  const full = multi(win, { options: ['a'], selected: ['a'] });
  click(win, control(full));
  assert.deepStrictEqual(emptyTexts(win, full), ['All selected']);

  const none = single(win, { options: [] });
  click(win, control(none));
  assert.deepStrictEqual(emptyTexts(win, none), ['No options']);
  win.close();
});

test('server search: typing calls onSearch once the user pauses, and the list says so', async (newWindow) => {
  const win = newWindow();
  const queries = [];
  const sel = multi(win, { options: ['ab', 'ac'], onSearch: (q) => queries.push(q) });
  click(win, control(sel));
  type(win, search(sel), 'a');
  await wait(win, 300);
  assert.deepStrictEqual(queries, [], 'not in server-search mode yet');

  sel.setSearchInfo({ truncated: true, total: 999 });
  assert.deepStrictEqual(emptyTexts(win, sel), ['999 values — type to search']);
  type(win, search(sel), 'ab');
  type(win, search(sel), 'abc');
  await wait(win, 100);
  assert.deepStrictEqual(queries, [], 'debounced');
  await wait(win, 250);
  assert.deepStrictEqual(queries, ['abc'], 'one call, the last query');

  sel.setSearchInfo(null);
  type(win, search(sel), 'abcd');
  await wait(win, 300);
  assert.deepStrictEqual(queries, ['abc'], 'left server-search mode');
  win.close();
});

/* --- keyboard ------------------------------------------------------------- */

test('ArrowDown and ArrowUp move the highlight and wrap; aria-activedescendant follows', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ['a', 'b', 'c'] });
  click(win, control(sel));
  const input = search(sel);
  const active = () => {
    const id = input.getAttribute('aria-activedescendant');
    const row = win.document.getElementById(id);
    return row && row.getAttribute('data-value');
  };
  assert.strictEqual(highlighted(win, sel), 'a');
  assert.strictEqual(active(), 'a');
  press(win, input, 'ArrowDown');
  assert.strictEqual(highlighted(win, sel), 'b');
  assert.strictEqual(active(), 'b');
  press(win, input, 'ArrowDown');
  press(win, input, 'ArrowDown');
  assert.strictEqual(highlighted(win, sel), 'a', 'wraps at the end');
  press(win, input, 'ArrowUp');
  assert.strictEqual(highlighted(win, sel), 'c', 'wraps at the start');
  assert.strictEqual(active(), 'c');
  win.close();
});

test('the highlighted row is scrolled into view', (newWindow) => {
  const win = newWindow();
  const scrolled = [];
  // happy-dom shares prototypes between windows, so put it back afterwards.
  const proto = win.Element.prototype;
  const original = proto.scrollIntoView;
  proto.scrollIntoView = function () {
    scrolled.push(this.getAttribute('data-value'));
  };
  try {
    const sel = single(win, { options: ABC });
    click(win, control(sel));
    press(win, search(sel), 'ArrowDown');
    press(win, search(sel), 'ArrowDown');
    assert.deepStrictEqual(scrolled.slice(-2), ['b', 'c']);
  } finally {
    proto.scrollIntoView = original;
    win.close();
  }
});

/* Deliberate difference: the reference set the highlight on the pick at
 * open but scrolled only on arrow moves, so a pick far down a long column
 * list opened out of view. */
test('opening scrolls the pick into view', (newWindow, t) => {
  const win = newWindow();
  const scrolled = [];
  const proto = win.Element.prototype;
  const original = proto.scrollIntoView;
  proto.scrollIntoView = function () { scrolled.push(this.getAttribute('data-value')); };
  try {
    const sel = single(win, { options: ABC, selected: 'd' });
    click(win, control(sel));
    assert.deepStrictEqual(scrolled, ['d']);
  } finally {
    proto.scrollIntoView = original;
    win.close();
  }
});

test('a single select opens with the highlight on its pick', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC, selected: 'd' });
  click(win, control(sel));
  assert.strictEqual(highlighted(win, sel), 'd');
  press(win, search(sel), 'ArrowDown');
  assert.strictEqual(highlighted(win, sel), 'e', 'and the arrows start from there');
  press(win, search(sel), 'Escape');
  press(win, search(sel), 'ArrowUp');
  assert.strictEqual(highlighted(win, sel), 'd', 'a keyboard open too');
  win.close();
});

test('the highlight moves to the first row when the filter changes', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ['ab', 'cd', 'ce'], selected: 'ce' });
  click(win, control(sel));
  assert.strictEqual(highlighted(win, sel), 'ce');
  type(win, search(sel), 'c');
  assert.strictEqual(highlighted(win, sel), 'cd');
  win.close();
});

test('Enter picks the highlighted row', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = single(win, { options: ABC, selected: 'a', onChange: (v) => seen.push(v) });
  click(win, control(sel));
  press(win, search(sel), 'ArrowDown');
  press(win, search(sel), 'Enter');
  assert.strictEqual(sel.getValue(), 'b');
  assert.deepStrictEqual(seen, ['b']);
  assert.ok(!isOpen(sel));
  win.close();
});

test('Enter on an empty list picks nothing', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = single(win, { options: ABC, selected: 'a', onChange: (v) => seen.push(v) });
  click(win, control(sel));
  type(win, search(sel), 'zzz');
  press(win, search(sel), 'Enter');
  assert.strictEqual(sel.getValue(), 'a');
  assert.deepStrictEqual(seen, []);
  win.close();
});

test('Backspace in an empty search input removes the last tag', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = multi(win, { options: ABC, selected: ['a', 'b'], onChange: (v) => seen.push(v) });
  click(win, control(sel));
  type(win, search(sel), 'c');
  press(win, search(sel), 'Backspace');
  assert.deepStrictEqual(tags(sel), ['a', 'b'], 'with text in the box, Backspace edits the text');
  type(win, search(sel), '');
  press(win, search(sel), 'Backspace');
  assert.deepStrictEqual(tags(sel), ['a']);
  assert.deepStrictEqual(json(seen), [['a']]);
  assert.deepStrictEqual(rows(win, sel), ['b', 'c', 'd', 'e'], 'the value is offered again');
  win.close();
});

/* --- picking -------------------------------------------------------------- */

test('single: a pick closes, shows the value, and reports only a change', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = single(win, { options: LABELLED, selected: 'AVAL', onChange: (v) => seen.push(v) });
  click(win, control(sel));
  clickRow(win, sel, 'CHG');
  assert.ok(!isOpen(sel));
  assert.strictEqual(sel.getValue(), 'CHG');
  assert.strictEqual(face(sel).textContent, 'CHGChange from Baseline');
  assert.strictEqual(face(sel).querySelector('.blockr-select__opt-label').textContent, 'Change from Baseline');
  assert.strictEqual(control(sel).title, 'CHG — Change from Baseline', 'full text on hover');
  click(win, control(sel));
  clickRow(win, sel, 'CHG');
  assert.deepStrictEqual(seen, ['CHG'], 'the same value again is not a change');
  win.close();
});

test('single: the placeholder shows while nothing is picked', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC, allowEmpty: true, placeholder: 'Pick one' });
  assert.strictEqual(face(sel).textContent, 'Pick one');
  assert.ok(face(sel).classList.contains('blockr-select__value--placeholder'));
  assert.strictEqual(search(sel).getAttribute('placeholder'), 'Pick one');
  sel.setValue('b');
  assert.strictEqual(face(sel).textContent, 'b');
  assert.ok(!face(sel).classList.contains('blockr-select__value--placeholder'));
  win.close();
});

test('single: while open, the search box shows the pick as its placeholder', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC, selected: 'b' });
  click(win, control(sel));
  assert.strictEqual(search(sel).getAttribute('placeholder'), 'b');
  assert.strictEqual(search(sel).value, '');
  win.close();
});

test('multi: a pick appends, clears the query, keeps the list open, and reports a copy', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = multi(win, { options: ABC, selected: ['a'], onChange: (v) => seen.push(v) });
  click(win, control(sel));
  type(win, search(sel), 'c');
  clickRow(win, sel, 'c');
  assert.ok(isOpen(sel));
  assert.strictEqual(search(sel).value, '');
  assert.deepStrictEqual(rows(win, sel), ['b', 'd', 'e']);
  assert.deepStrictEqual(tags(sel), ['a', 'c']);
  assert.deepStrictEqual(json(seen), [['a', 'c']]);
  seen[0].push('zzz');
  assert.deepStrictEqual(values(sel), ['a', 'c'], 'the caller got a copy');
  win.close();
});

test('multi: the search placeholder shows only while there are no tags', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC, placeholder: 'Columns' });
  assert.strictEqual(search(sel).getAttribute('placeholder'), 'Columns');
  sel.setValue(['a']);
  assert.strictEqual(search(sel).getAttribute('placeholder'), '');
  sel.setValue([]);
  assert.strictEqual(search(sel).getAttribute('placeholder'), 'Columns');
  win.close();
});

test('multi: removing a tag by its x reports a copy', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = multi(win, { options: ABC, selected: ['a', 'b', 'c'], onChange: (v) => seen.push(v) });
  click(win, sel.el.querySelector('.blockr-select__tag[data-value="b"] .blockr-select__tag-remove'));
  assert.deepStrictEqual(tags(sel), ['a', 'c']);
  assert.deepStrictEqual(json(seen), [['a', 'c']]);
  seen[0].length = 0;
  assert.deepStrictEqual(values(sel), ['a', 'c']);
  win.close();
});

test('multi menu: picks are tags in the head, above the filter box, and the prompt survives', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const nine = ABC.concat(['f', 'g', 'h', 'i']);
  win.Blockr.Select.menu(host(win), {
    mode: 'multi', options: nine, selected: ['b'], searchPlaceholder: 'Filter columns',
    onChange: (v) => seen.push(v)
  });
  const dd = win.document.querySelector('.blockr-select__dropdown');
  const tagsEl = dd.querySelector('.blockr-select__tags');
  const box = dd.querySelector('.blockr-select__search');
  assert.ok(tagsEl && box, 'both in the panel');
  assert.ok(tagsEl.compareDocumentPosition(box) & win.Node.DOCUMENT_POSITION_FOLLOWING, 'tags first');
  click(win, dd.querySelector('.blockr-select__option[data-value="d"]'));
  assert.deepStrictEqual(json(seen), [['b', 'd']]);
  assert.deepStrictEqual(
    [...dd.querySelectorAll('.blockr-select__tag')].map((t) => t.getAttribute('data-value')),
    ['b', 'd']
  );
  assert.strictEqual(box.getAttribute('placeholder'), 'Filter columns');
  assert.ok(dd.style.display === 'block', 'still open');
  win.close();
});

/* --- reconciling ---------------------------------------------------------- */

test('setOptions on a single falls back to the first option unless sel is known', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = single(win, { options: ABC, selected: 'c', onChange: (v) => seen.push(v) });
  sel.setOptions(['x', 'y', 'c']);
  assert.strictEqual(sel.getValue(), 'x', 'no sel: the first option, even though c is still there');
  sel.setOptions(['x', 'y', 'c'], 'c');
  assert.strictEqual(sel.getValue(), 'c');
  sel.setOptions(['x', 'y'], 'c');
  assert.strictEqual(sel.getValue(), 'x', 'unknown sel: the first option');
  sel.setOptions([]);
  assert.strictEqual(sel.getValue(), '');
  sel.setOptions('only');
  assert.strictEqual(sel.getValue(), 'only', 'a scalar option list is wrapped');
  assert.deepStrictEqual(seen, [], 'never reported');
  win.close();
});

test('setOptions with allowEmpty keeps an empty or still-valid pick and clears an unknown one', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC, allowEmpty: true });
  assert.strictEqual(sel.getValue(), '', 'no first-option pick');
  sel.setOptions(['x', 'y']);
  assert.strictEqual(sel.getValue(), '', 'empty survives a refresh');
  sel.setOptions(['x', 'y'], 'y');
  assert.strictEqual(sel.getValue(), 'y');
  sel.setOptions(['y', 'z']);
  assert.strictEqual(sel.getValue(), 'y', 'omitting sel keeps a still-valid pick');
  sel.setOptions(['p', 'q']);
  assert.strictEqual(sel.getValue(), '', 'a pick the list lost clears');
  sel.setOptions(['p', 'q'], 'nope');
  assert.strictEqual(sel.getValue(), '', 'an unknown sel clears too');
  win.close();
});

test('setOptions on a multi keeps only picks the new list carries, and coerces a scalar', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC, selected: ['a', 'b', 'c'] });
  sel.setOptions(['b', 'c', 'x']);
  assert.deepStrictEqual(values(sel), ['b', 'c']);
  sel.setOptions(['b', 'c', 'x'], 'x');
  assert.deepStrictEqual(values(sel), ['x']);
  sel.setOptions(['b', 'c', 'x'], '');
  assert.deepStrictEqual(values(sel), []);
  sel.setOptions(['b', 'c', 'x'], ['c', 'nope', 'b']);
  assert.deepStrictEqual(values(sel), ['c', 'b'], 'in the order given');
  win.close();
});

test('setOptions redraws an open list', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC, selected: ['a'] });
  click(win, control(sel));
  sel.setOptions(['a', 'x', 'y']);
  assert.deepStrictEqual(rows(win, sel), ['x', 'y']);
  assert.deepStrictEqual(tags(sel), ['a']);
  win.close();
});

test('updateOptions swaps the list without touching the pick', (newWindow) => {
  const win = newWindow();
  const one = single(win, { options: ABC, selected: 'c' });
  one.updateOptions(['x', 'y']);
  assert.strictEqual(one.getValue(), 'c');
  assert.strictEqual(face(one).textContent, 'c', 'the face keeps showing it');
  one.updateOptions(['x', 'y'], 'y');
  assert.strictEqual(one.getValue(), 'y');
  one.updateOptions(['x', 'y'], 'not-offered');
  assert.strictEqual(one.getValue(), 'not-offered', 'sel is forced, list or not');

  const many = multi(win, { options: ABC, selected: ['a', 'b'] });
  many.updateOptions(['x']);
  assert.deepStrictEqual(values(many), ['a', 'b']);
  assert.deepStrictEqual(tags(many), ['a', 'b']);
  many.updateOptions(['x'], 'z');
  assert.deepStrictEqual(values(many), ['z'], 'a scalar is wrapped');
  many.updateOptions(['x'], ['q', 'r']);
  assert.deepStrictEqual(values(many), ['q', 'r']);
  win.close();
});

test('updateOptions redraws an open list', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC, selected: 'c' });
  click(win, control(sel));
  sel.updateOptions(['x', 'c']);
  assert.deepStrictEqual(rows(win, sel), ['x', 'c']);
  win.close();
});

test('setValue reconciles like setOptions and never reports', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const one = single(win, { options: ABC, selected: 'a', onChange: (v) => seen.push(v) });
  one.setValue('d');
  assert.strictEqual(one.getValue(), 'd');
  one.setValue('nope');
  assert.strictEqual(one.getValue(), 'a', 'unknown: the first option');
  const empty = single(win, { options: ABC, allowEmpty: true, onChange: (v) => seen.push(v) });
  empty.setValue('b');
  empty.setValue(null);
  assert.strictEqual(empty.getValue(), 'b', 'null leaves a valid pick alone');
  empty.setValue('nope');
  assert.strictEqual(empty.getValue(), '');
  const many = multi(win, { options: ABC, selected: ['a'], onChange: (v) => seen.push(v) });
  many.setValue(['c', 'nope', 'a']);
  assert.deepStrictEqual(values(many), ['c', 'a']);
  assert.deepStrictEqual(tags(many), ['c', 'a']);
  assert.deepStrictEqual(seen, []);
  win.close();
});

test('getValue returns a string or an independent copy of the array', (newWindow) => {
  const win = newWindow();
  const one = single(win, { options: [] });
  assert.strictEqual(one.getValue(), '');
  const many = multi(win, { options: ABC, selected: ['a', 'b'] });
  const got = many.getValue();
  got.push('c');
  assert.deepStrictEqual(values(many), ['a', 'b']);
  win.close();
});

test('the initial selection is copied, not aliased', (newWindow) => {
  const win = newWindow();
  const initial = ['a', 'b'];
  const sel = multi(win, { options: ABC, selected: initial });
  initial.push('c');
  assert.deepStrictEqual(values(sel), ['a', 'b']);
  win.close();
});

/* --- tags ----------------------------------------------------------------- */

test('a tag shows the value and, muted, its label; full text on hover', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: LABELLED, selected: ['AVAL', 'BASE'] });
  const labels = [...sel.el.querySelectorAll('.blockr-select__tag-label')];
  assert.strictEqual(labels[0].textContent, 'AVALAnalysis Value');
  assert.strictEqual(labels[0].querySelector('.blockr-select__opt-label').textContent, 'Analysis Value');
  assert.strictEqual(labels[0].title, 'AVAL — Analysis Value');
  assert.strictEqual(labels[1].textContent, 'BASE');
  assert.strictEqual(labels[1].title, 'BASE');
  const x = sel.el.querySelector('.blockr-select__tag[data-value="AVAL"] .blockr-select__tag-remove');
  assert.strictEqual(x.getAttribute('aria-label'), 'Remove AVAL');
  win.close();
});

test('a tag for a value the list does not carry still shows the value', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC, selected: [] });
  sel.updateOptions(ABC, ['zz']);
  const label = sel.el.querySelector('.blockr-select__tag-label');
  assert.strictEqual(label.textContent, 'zz');
  assert.strictEqual(label.title, 'zz');
  win.close();
});

test('maxTagChars cuts the middle of a long value and keeps it whole on hover', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, {
    options: [{ value: 'Xanomeline High Dose', label: 'Arm' }],
    selected: ['Xanomeline High Dose'], maxTagChars: 12
  });
  const label = sel.el.querySelector('.blockr-select__tag-label');
  assert.strictEqual(label.textContent, 'Xanome… Dose');
  assert.strictEqual(label.title, 'Xanomeline High Dose — Arm');
  assert.deepStrictEqual(values(sel), ['Xanomeline High Dose']);
  win.close();
});

test('labelFirst leads the rows with the label', (newWindow) => {
  const win = newWindow();
  win.Blockr.Select.menu(host(win), { options: LABELLED, labelFirst: true });
  const texts = [...win.document.querySelectorAll('.blockr-select__option')].map((r) => r.textContent);
  assert.deepStrictEqual(texts, ['Analysis ValueAVAL', 'Change from BaselineCHG', 'BASE']);
  win.close();
});

/* Drag events, the way a browser delivers them: dragstart on the tag under
 * the pointer, dragover on whichever tag it crosses (with the pointer's x
 * against the tag's midpoint, which is 0 here), then drop. MouseEvent, not
 * DragEvent: happy-dom's DragEvent carries no clientX. */
const drag = (win, sel, from, to, side) => {
  const tagEl = (v) => sel.el.querySelector(`.blockr-select__tag[data-value="${v}"]`);
  const dt = { effectAllowed: '', dropEffect: '', setData() {} };
  const start = new win.MouseEvent('dragstart', { bubbles: true });
  Object.defineProperty(start, 'dataTransfer', { value: dt });
  tagEl(from).dispatchEvent(start);
  assert.ok(tagEl(from).classList.contains('blockr-select__tag--dragging'));
  const over = new win.MouseEvent('dragover', {
    bubbles: true, cancelable: true, clientX: side === 'before' ? -1 : 1
  });
  Object.defineProperty(over, 'dataTransfer', { value: dt });
  tagEl(to).dispatchEvent(over);
  assert.ok(over.defaultPrevented, 'dragover is accepted');
  assert.ok(tagEl(to).classList.contains(`blockr-select__tag--drop-${side}`));
  const drop = new win.MouseEvent('drop', { bubbles: true, cancelable: true });
  Object.defineProperty(drop, 'dataTransfer', { value: dt });
  tagEl(to).dispatchEvent(drop);
};

test('reorderable: dragging a tag before or after another reorders and reports', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = multi(win, { options: ABC, selected: ['a', 'b', 'c'], onChange: (v) => seen.push(v) });
  assert.ok(sel.el.querySelectorAll('.blockr-select__tag[draggable="true"]').length === 3);
  drag(win, sel, 'a', 'c', 'after');
  assert.deepStrictEqual(tags(sel), ['b', 'c', 'a']);
  drag(win, sel, 'a', 'b', 'before');
  assert.deepStrictEqual(tags(sel), ['a', 'b', 'c']);
  assert.deepStrictEqual(json(seen), [['b', 'c', 'a'], ['a', 'b', 'c']]);
  assert.strictEqual(sel.el.querySelector('.blockr-select__tag--dragging'), null, 'indicators cleared');
  assert.strictEqual(sel.el.querySelector('.blockr-select__tag--drop-before, .blockr-select__tag--drop-after'), null);
  win.close();
});

test('reorderable: false leaves the tags in place', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = multi(win, { options: ABC, selected: ['a', 'b'], reorderable: false, onChange: (v) => seen.push(v) });
  assert.strictEqual(sel.el.querySelectorAll('.blockr-select__tag[draggable]').length, 0);
  win.close();
});

/* A layout for the single-line fit: the tag row is 200px wide, every tag
 * 60px, the chip 24px, the gap 3px. Three tags need 186px and fit; five need
 * the chip, which pushes the count down to two (2 * 60 + 3 + 3 + 24 = 150).
 * Same numbers as the fitCount tests, fed through the DOM this time. */
const stubLayout = (win) => {
  // happy-dom shares prototypes between windows: restore them afterwards.
  const widthDesc = Object.getOwnPropertyDescriptor(win.HTMLElement.prototype, 'clientWidth');
  const rect = win.Element.prototype.getBoundingClientRect;
  Object.defineProperty(win.HTMLElement.prototype, 'clientWidth', {
    configurable: true,
    get() { return this.classList.contains('blockr-select__tags') ? 200 : 0; }
  });
  win.Element.prototype.getBoundingClientRect = function () {
    const w = this.classList.contains('blockr-select__tag') ? 60
      : this.classList.contains('blockr-select__more') ? 24 : 0;
    return { width: w, height: 0, left: 0, right: w, top: 0, bottom: 0, x: 0, y: 0 };
  };
  return () => {
    Object.defineProperty(win.HTMLElement.prototype, 'clientWidth', widthDesc);
    win.Element.prototype.getBoundingClientRect = rect;
  };
};

test('singleLine: tags past the first row hide behind a +N chip that lists them', (newWindow, t) => {
  const win = newWindow();
  t.after(stubLayout(win));
  const three = multi(win, { options: ABC, selected: ['a', 'b', 'c'], singleLine: true });
  const hiddenIn = (sel) =>
    [...sel.el.querySelectorAll('.blockr-select__tag--hidden')].map((t) => t.getAttribute('data-value'));
  const chipOf = (sel) => {
    const chip = sel.el.querySelector('.blockr-select__more');
    return chip && chip.style.display !== 'none' ? { text: chip.textContent, title: chip.title } : null;
  };
  assert.deepStrictEqual(hiddenIn(three), []);
  assert.strictEqual(chipOf(three), null);

  const five = multi(win, { options: ABC, selected: ABC, singleLine: true });
  assert.deepStrictEqual(hiddenIn(five), ['c', 'd', 'e']);
  assert.deepStrictEqual(chipOf(five), { text: '+3', title: 'c, d, e' });
  assert.deepStrictEqual(values(five), ABC, 'hidden is not removed');

  // Widening (or narrowing) refits: a removal gives room back.
  click(win, five.el.querySelector('.blockr-select__tag[data-value="d"] .blockr-select__tag-remove'));
  assert.deepStrictEqual(hiddenIn(five), ['c', 'e']);
  assert.deepStrictEqual(chipOf(five), { text: '+2', title: 'c, e' });
  win.close();
});

test('singleLine: the chip expands the control until a click elsewhere', (newWindow, t) => {
  const win = newWindow();
  t.after(stubLayout(win));
  const sel = multi(win, { options: ABC, selected: ABC, singleLine: true });
  const hidden = () => sel.el.querySelectorAll('.blockr-select__tag--hidden').length;
  assert.strictEqual(hidden(), 3);
  click(win, sel.el.querySelector('.blockr-select__more'));
  assert.ok(sel.el.classList.contains('blockr-select--expanded'));
  assert.strictEqual(hidden(), 0, 'every tag is reachable');
  assert.ok(!isOpen(sel), 'the chip does not open the list');
  clickOutside(win);
  assert.ok(!sel.el.classList.contains('blockr-select--expanded'));
  assert.strictEqual(hidden(), 3);
  win.close();
});

test('singleLine: a control without a width yet leaves every tag visible', (newWindow) => {
  const win = newWindow();
  const sel = multi(win, { options: ABC, selected: ABC, singleLine: true });
  assert.ok(sel.el.classList.contains('blockr-select--single-line'));
  assert.strictEqual(sel.el.querySelectorAll('.blockr-select__tag--hidden').length, 0);
  const chip = sel.el.querySelector('.blockr-select__more');
  assert.ok(chip === null || chip.style.display === 'none');
  win.close();
});

/* --- lifecycle ------------------------------------------------------------ */

test('destroy closes, removes the portalled list and the root, and unhooks the document', (newWindow) => {
  const win = newWindow();
  const counts = { add: 0, remove: 0 };
  for (const target of [win.document, win]) {
    const add = target.addEventListener.bind(target);
    const remove = target.removeEventListener.bind(target);
    target.addEventListener = (...a) => { counts.add++; add(...a); };
    target.removeEventListener = (...a) => { counts.remove++; remove(...a); };
  }
  let closed = 0;
  const container = host(win);
  const sel = win.Blockr.Select.multi(container, {
    options: ABC, selected: ['a'], singleLine: true, onClose: () => { closed++; }
  });
  click(win, control(sel));
  const dd = dropdown(win, sel);
  sel.destroy();
  assert.strictEqual(closed, 1);
  assert.ok(!dd.isConnected, 'the list left the body');
  assert.ok(!sel.el.isConnected);
  assert.strictEqual(container.childElementCount, 0);
  assert.strictEqual(counts.remove, counts.add, 'every document and window listener came off');
  sel.destroy();
  assert.strictEqual(closed, 1, 'a second destroy is a no-op');
  clickOutside(win);
  win.close();
});

test('a destroyed select ignores its own late handle calls', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC });
  sel.destroy();
  sel.setOptions(['x']);
  sel.setLoading(true);
  sel.setValue('x');
  assert.strictEqual(sel.getValue(), 'x');
  win.close();
});

test('the combobox input names its list and its role', (newWindow) => {
  const win = newWindow();
  const sel = single(win, { options: ABC });
  const input = search(sel);
  assert.strictEqual(input.getAttribute('aria-autocomplete'), 'list');
  assert.strictEqual(input.getAttribute('autocomplete'), 'off');
  assert.ok(input.getAttribute('aria-controls'));
  assert.strictEqual(win.document.getElementById(input.getAttribute('aria-controls')), dropdown(win, sel));
  win.close();
});
