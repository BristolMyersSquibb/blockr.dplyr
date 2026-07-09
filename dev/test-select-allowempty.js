// Blockr.Select single-mode selection semantics, exercised in a real DOM.
//
// Focus: `allowEmpty` (blockr-select.md / ux-principles.md). With it, '' means
// "nothing selected" and survives setOptions(), and a selection that is no
// longer among the options CLEARS rather than sliding onto option 0. The
// `default:` cases below are regression guards: allowEmpty defaults to false
// and every pre-existing block must behave exactly as it did.
//
// jsdom is not a package dependency; install it ad hoc. Node 18 needs jsdom@22
// (later majors are ESM-only and break `require`). Run from the workspace root:
//
//   npm i --no-save jsdom@22
//   node blockr.dplyr/dev/test-select-allowempty.js
const { JSDOM } = require('jsdom');
const fs = require('fs');

const path = require('path');
const JS = path.join(__dirname, '..', 'inst', 'js');
const dom = new JSDOM('<!doctype html><body></body>', {
  pretendToBeVisual: true,
  runScripts: 'dangerously'
});
const w = dom.window;
const run = (f) => {
  const el = w.document.createElement('script');
  el.textContent = fs.readFileSync(path.join(JS, f), 'utf8');
  w.document.body.appendChild(el);
};
run('blockr-core.js');
run('blockr-select.js');

let pass = 0, fail = 0;
const ok = (label, got, want) => {
  const good = JSON.stringify(got) === JSON.stringify(want);
  if (good) pass++; else fail++;
  console.log(`${good ? 'PASS ' : 'FAIL '} ${label}${good ? '' : `  got ${JSON.stringify(got)} want ${JSON.stringify(want)}`}`);
};

const mk = (config) => {
  const el = w.document.createElement('div');
  w.document.body.appendChild(el);
  return w.Blockr.Select.single(el, config);
};

const A = [{ value: 'a', label: 'Arm A' }, { value: 'b', label: 'Arm B' }];
const B = [{ value: 'c', label: 'Arm C' }, { value: 'd', label: 'Arm D' }];

// ---- allowEmpty: true --------------------------------------------------
ok('allowEmpty: no `selected` starts empty',
  mk({ options: A, allowEmpty: true }).getValue(), '');

ok('allowEmpty: explicit selected is honoured',
  mk({ options: A, selected: 'b', allowEmpty: true }).getValue(), 'b');

{
  const s = mk({ options: A, allowEmpty: true });
  s.setOptions(B, '');
  ok("allowEmpty: '' survives setOptions", s.getValue(), '');
}
{
  // The pharma stale-pick case: cohort changes, picked patient is gone.
  const s = mk({ options: A, selected: 'a', allowEmpty: true });
  s.setOptions(B, 'a');
  ok('allowEmpty: stale selection CLEARS, no auto-pick', s.getValue(), '');
}
{
  const s = mk({ options: A, selected: 'a', allowEmpty: true });
  s.setOptions(B, 'd');
  ok('allowEmpty: valid new selection applies', s.getValue(), 'd');
}
{
  // Steppers path: options unchanged, value pushed from the server.
  const s = mk({ options: A, allowEmpty: true });
  s.setOptions(A, 'b');
  ok('allowEmpty: value-only push selects', s.getValue(), 'b');
}
{
  const s = mk({ options: A, selected: 'b', allowEmpty: true });
  s.setOptions(A);
  ok('allowEmpty: omitted `sel` keeps a still-valid selection', s.getValue(), 'b');
}
{
  const s = mk({ options: A, selected: 'b', allowEmpty: true });
  s.setOptions([]);
  ok('allowEmpty: emptying the options clears', s.getValue(), '');
}

// ---- allowEmpty: false (default) — MUST be unchanged -------------------
ok('default: no `selected` falls back to option 0',
  mk({ options: A }).getValue(), 'a');

ok('default: no options yields empty',
  mk({ options: [] }).getValue(), '');

ok('default: selected:"" is honoured at construct time',
  mk({ options: A, selected: '' }).getValue(), '');

{
  const s = mk({ options: A, selected: 'b' });
  s.setOptions(B, 'b');
  ok('default: stale selection falls back to option 0', s.getValue(), 'c');
}
{
  const s = mk({ options: A, selected: 'a' });
  s.setOptions(B, 'd');
  ok('default: valid new selection applies', s.getValue(), 'd');
}
{
  const s = mk({ options: A, selected: 'b' });
  s.setOptions(B);
  ok('default: omitted `sel` resets to option 0', s.getValue(), 'c');
}
{
  const s = mk({ options: A, selected: 'a' });
  s.setOptions([]);
  ok('default: emptying the options clears', s.getValue(), '');
}

// ---- multi mode must ignore allowEmpty ---------------------------------
{
  const el = w.document.createElement('div');
  w.document.body.appendChild(el);
  const m = w.Blockr.Select.multi(el, { options: A, selected: ['a'], allowEmpty: true });
  m.setOptions(B, ['c']);
  ok('multi: allowEmpty is inert', m.getValue(), ['c']);
}

console.log(`\n${pass} passed, ${fail} failed`);
process.exit(fail ? 1 : 0);
