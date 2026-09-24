/* Load Blockr.Select into a fresh happy-dom window for each test.
 *
 * During the rewrite (dev/select-rewrite-plan.md) this loaded two
 * implementations, the shipped file and a frozen copy of the previous one,
 * and registered every test once per implementation. The copy is gone; the
 * shape stays so a future rewrite can do the same again.
 *
 * Usage:
 *   const { test } = require('./select-impls');
 *   test('what it does', (newWindow, t) => {
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
const read = (f) => fs.readFileSync(path.join(JS_DIR, f), 'utf8');
const ui = read('blockr-ui.js');
const select = read('blockr-select.js');

const newWindow = () => {
  const win = new Window({ url: 'http://localhost/' });
  win.eval(ui);
  win.eval(select);
  return win;
};

/** Register `name`; `fn` receives the window factory and node's test context. */
const test = (name, fn) => nodeTest(name, (t) => fn(newWindow, t));

module.exports = { test, newWindow };
