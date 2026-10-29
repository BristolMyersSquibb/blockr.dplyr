/* Blockr.Pick's state rules: All, None, `min`, a burst of picks in place, and
 * what a push from R does to picks the caller has not heard yet.
 *
 * The picker runs from inst/js/blockr-pick.js on blockr.ui's blockr-ui.js, in
 * happy-dom, with a fake clock owning setTimeout so a test says `tick(1000)`
 * instead of waiting out the report delay.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { Window } = require('happy-dom');

const JS_DIR = path.join(__dirname, '..', '..', 'inst', 'js');
let uiDir = process.env.BLOCKR_UI_JS;
const UI_DIR = () => uiDir || (uiDir = require('node:child_process').execFileSync(
  'Rscript', ['-e', 'cat(system.file("assets", "js", package = "blockr.ui", mustWork = TRUE))'],
  { encoding: 'utf8' }
).trim());

const json = (x) => JSON.parse(JSON.stringify(x));

/** A picker of `form` ('multi' or 'list') on `options`, and a way to drive it. */
function mount(form, config) {
  const win = new Window({ url: 'http://localhost/' });
  // The fake clock, installed before the scripts read setTimeout.
  win.eval(`
    window.__now = 0;
    window.__timers = [];
    window.__seq = 0;
    window.setTimeout = function (fn, ms) {
      var id = ++window.__seq;
      window.__timers.push({ id: id, at: window.__now + (ms || 0), fn: fn });
      return id;
    };
    window.clearTimeout = function (id) {
      window.__timers = window.__timers.filter(function (t) { return t.id !== id; });
    };
    window.Blockr = { icons: { chevron: '', confirm: '<svg></svg>', grip: '<svg></svg>' } };
  `);
  win.eval(fs.readFileSync(path.join(UI_DIR(), 'blockr-ui.js'), 'utf8'));
  win.eval(fs.readFileSync(path.join(JS_DIR, 'blockr-pick.js'), 'utf8'));
  const doc = win.document;
  doc.body.innerHTML = '<div id="host"></div>';
  const changes = [];
  const handle = win.Blockr.Pick[form](doc.getElementById('host'), Object.assign({
    onChange: (v) => changes.push(json(v))
  }, config));
  const panel = () => doc.querySelector('.blockr-pick__panel');
  const btn = (text) => Array.from(panel().querySelectorAll('.blockr-pick__bulk-btn'))
    .find((b) => b.textContent.startsWith(text));
  const api = {
    win, doc, handle, changes,
    value: () => json(handle.getValue()),
    tick(ms) {
      const until = win.__now + ms;
      for (;;) {
        const due = win.__timers.filter((t) => t.at <= until).sort((a, b) => a.at - b.at)[0];
        if (!due) break;
        win.__timers = win.__timers.filter((t) => t !== due);
        win.__now = due.at;
        due.fn();
      }
      win.__now = until;
    },
    open() { doc.querySelector('.blockr-select__control').click(); },
    close() { doc.querySelector('.blockr-select__control').click(); },
    row: (v) => panel().querySelector(`.blockr-select__option[data-value="${v}"]`),
    tickRow(v) { api.row(v).click(); },
    all() { btn('All').click(); },
    none() { btn('None').click(); },
    noneBtn: () => btn('None'),
    filter(q) {
      const input = panel().querySelector('.blockr-select__search');
      input.value = q;
      input.dispatchEvent(new win.Event('input'));
    },
    count: () => panel().querySelector('.blockr-pick__count').textContent
  };
  return api;
}

const OPTS = ['CR', 'NE', 'PD', 'PR', 'SD'];

test('All ticks every option and a dropdown reports it once, on close', () => {
  const p = mount('multi', { options: OPTS, selected: ['PD'] });
  p.open();
  p.all();
  assert.deepStrictEqual(p.value(), ['PD', 'CR', 'NE', 'PR', 'SD']);
  assert.deepStrictEqual(p.changes, []);
  p.close();
  assert.deepStrictEqual(p.changes, [['PD', 'CR', 'NE', 'PR', 'SD']]);
});

test('None in place empties the list and reports [] after the pause', () => {
  const p = mount('list', { options: OPTS, selected: OPTS.slice() });
  p.none();
  assert.deepStrictEqual(p.value(), []);
  assert.match(p.count(), /^0 of 5/);
  p.tick(999);
  assert.deepStrictEqual(p.changes, []);
  p.tick(1);
  assert.deepStrictEqual(p.changes, [[]]);
  p.tick(5000);
  assert.deepStrictEqual(p.value(), []);
});

test('with a filter typed, All and None act on the matching rows only', () => {
  const p = mount('multi', { options: OPTS, selected: ['CR', 'SD'] });
  p.open();
  p.filter('P');
  p.all();
  assert.deepStrictEqual(p.value(), ['CR', 'SD', 'PD', 'PR']);
  p.none();
  assert.deepStrictEqual(p.value(), ['CR', 'SD']);
});

test('min: None is held back where it would leave too few, and says why', () => {
  const p = mount('list', { options: OPTS, selected: ['CR', 'PD'], min: 1, minHint: 'Keep one' });
  assert.strictEqual(p.noneBtn().getAttribute('aria-disabled'), 'true');
  p.none();
  p.tick(2000);
  assert.deepStrictEqual(p.value(), ['CR', 'PD']);
  assert.deepStrictEqual(p.changes, []);
  // Filtered to PD alone, None would leave CR: allowed.
  p.filter('PD');
  assert.strictEqual(p.noneBtn().getAttribute('aria-disabled'), 'false');
  p.none();
  assert.deepStrictEqual(p.value(), ['CR']);
});

test('min: the last pick does not untick, and its row says it is locked', () => {
  const p = mount('list', { options: OPTS, selected: ['CR', 'PD'], min: 1 });
  p.tickRow('PD');
  assert.deepStrictEqual(p.value(), ['CR']);
  assert.strictEqual(p.row('CR').getAttribute('aria-disabled'), 'true');
  p.tickRow('CR');
  assert.deepStrictEqual(p.value(), ['CR']);
  p.tick(1000);
  assert.deepStrictEqual(p.changes, [['CR']]);
  // Ticking another unlocks both.
  p.tickRow('SD');
  assert.strictEqual(p.row('CR').getAttribute('aria-disabled'), null);
});

test('a burst of picks in place is one report, of where the burst ended', () => {
  const p = mount('list', { options: OPTS, selected: ['CR'] });
  p.tickRow('PD');
  p.tick(300);
  p.tickRow('SD');
  p.tick(300);
  p.tickRow('PD');
  p.tick(300);
  assert.deepStrictEqual(p.changes, []);
  p.tick(1000);
  assert.deepStrictEqual(p.changes, [['CR', 'SD']]);
});

test('in place, a push of the old picks during the pause does not undo new ones', () => {
  const p = mount('list', { options: OPTS, selected: ['CR', 'PD'] });
  p.tickRow('SD');
  // R echoes what it had before it heard of SD.
  p.handle.setOptions(OPTS, ['CR', 'PD']);
  assert.deepStrictEqual(p.value(), ['CR', 'PD', 'SD']);
  p.tick(1000);
  assert.deepStrictEqual(p.changes, [['CR', 'PD', 'SD']]);
});

test('in place, a push that matches the report is quiet', () => {
  const p = mount('list', { options: OPTS, selected: ['CR'] });
  p.tickRow('PD');
  p.tick(1000);
  p.handle.setOptions(OPTS, ['CR', 'PD']);
  p.tick(5000);
  assert.deepStrictEqual(p.value(), ['CR', 'PD']);
  assert.deepStrictEqual(p.changes, [['CR', 'PD']]);
});

test('in place, a push that differs after the report is taken, and not sent back', () => {
  const p = mount('list', { options: OPTS, selected: ['CR'] });
  p.tickRow('PD');
  p.tick(1000);
  // R refused PD and says CR alone.
  p.handle.setOptions(OPTS, ['CR']);
  p.tick(5000);
  assert.deepStrictEqual(p.value(), ['CR']);
  assert.deepStrictEqual(p.changes, [['CR', 'PD']]);
});

test('a pending pick no longer offered is dropped from the report', () => {
  const p = mount('list', { options: OPTS, selected: ['CR'] });
  p.tickRow('SD');
  p.handle.setOptions(['CR', 'PD'], ['CR']);
  p.tick(1000);
  assert.deepStrictEqual(p.value(), ['CR']);
  assert.deepStrictEqual(p.changes, []);
});

test('an open dropdown keeps its changes against a push of the old picks', () => {
  const p = mount('multi', { options: OPTS, selected: ['CR'] });
  p.open();
  p.tickRow('PR');
  p.handle.setOptions(OPTS, ['CR']);
  assert.deepStrictEqual(p.value(), ['CR', 'PR']);
  p.close();
  assert.deepStrictEqual(p.changes, [['CR', 'PR']]);
});

test('an open dropdown with no changes takes a push, and does not send it back', () => {
  const p = mount('multi', { options: OPTS, selected: ['CR'] });
  p.open();
  p.handle.setOptions(OPTS, ['NE']);
  assert.deepStrictEqual(p.value(), ['NE']);
  p.close();
  assert.deepStrictEqual(p.changes, []);
});

test('a closed dropdown takes a push of new choices and picks', () => {
  const p = mount('multi', { options: OPTS, selected: ['CR'] });
  p.handle.setOptions(['A', 'B'], ['B']);
  assert.deepStrictEqual(p.value(), ['B']);
  p.handle.setValue(null);
  assert.deepStrictEqual(p.value(), ['B']);
  p.handle.setValue([]);
  assert.deepStrictEqual(p.value(), []);
});

test('a dropdown destroyed while open reports the picks made since it opened', () => {
  const p = mount('multi', { options: OPTS, selected: ['CR'] });
  p.open();
  p.tickRow('SD');
  p.handle.destroy();
  assert.deepStrictEqual(p.changes, [['CR', 'SD']]);
  const q = mount('multi', { options: OPTS, selected: ['CR'] });
  q.open();
  q.handle.destroy();
  assert.deepStrictEqual(q.changes, []);
});
