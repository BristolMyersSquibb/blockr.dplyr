/* Load Blockr.Select into a fresh happy-dom window, once per implementation.
 *
 * During the rewrite two implementations exist: the shipped
 * `inst/js/blockr-select.js` ("current") and, while it exists, the frozen copy
 * in `tests/js/fixtures/blockr-select-reference.js` ("reference"). Every
 * Select test runs against each one, so a behaviour the reference had and the
 * rewrite lost fails by name. `BLOCKR_SELECT_IMPL=current npm test` narrows
 * the run to one.
 *
 * Usage:
 *   const { test } = require('./select-impls');
 *   test('what it does', (newWindow, impl, t) => {
 *     const win = newWindow();
 *     ...
 *     win.close();
 *   });
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const nodeTest = require('node:test');
const { Window } = require('happy-dom');

const JS_DIR = path.join(__dirname, '..', '..', 'inst', 'js');
const FIXTURES = path.join(__dirname, 'fixtures');

const FILES = {
  current: path.join(JS_DIR, 'blockr-select.js'),
  reference: path.join(FIXTURES, 'blockr-select-reference.js')
};

const available = Object.keys(FILES).filter((k) => fs.existsSync(FILES[k]));
const wanted = process.env.BLOCKR_SELECT_IMPL;
const impls = wanted ? [wanted] : available;
for (const impl of impls) {
  if (!FILES[impl] || !fs.existsSync(FILES[impl])) {
    throw new Error(`no Select implementation "${impl}" (have ${available.join(', ')})`);
  }
}

const sources = {};
const source = (impl) => {
  if (!sources[impl]) sources[impl] = fs.readFileSync(FILES[impl], 'utf8');
  return sources[impl];
};
const ui = fs.readFileSync(path.join(JS_DIR, 'blockr-ui.js'), 'utf8');

const newWindow = (impl) => {
  const win = new Window({ url: 'http://localhost/' });
  win.eval(ui);
  win.eval(source(impl));
  return win;
};

/**
 * Register `name` once per implementation. `fn` receives a window factory
 * bound to that implementation, the implementation's name (for a test that
 * pins a deliberate difference) and node's test context.
 */
const test = (name, fn) => {
  for (const impl of impls) {
    nodeTest(`${name} [${impl}]`, (t) => fn(() => newWindow(impl), impl, t));
  }
};

module.exports = { test, impls, newWindow };
