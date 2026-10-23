/* The operator pills in filter and join open a menu; a second click on the
 * pill closes it. blockr.ui leaves a click on a menu's anchor to the caller,
 * so without this a second click opened a second menu on top of the first.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { mount } = require('./harness.js');

const IRIS = ['Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Species'];

/* A menu tears itself down on a timer, after the closing click. */
const settle = (b) => new Promise((resolve) => b.win.setTimeout(resolve, 10));

const menus = (b) => b.doc.querySelectorAll('.blockr-select-menu-host').length;

const pills = {
  filter: () => {
    const b = mount('filter');
    b.columns(IRIS);
    b.setState({
      conditions: [
        { type: 'values', column: 'Species', values: ['setosa'], mode: 'include', colType: 'character' }
      ],
      operator: '&',
      preserve_order: false
    });
    return { b, pill: b.el.querySelector('.fb-op-btn') };
  },
  join: () => {
    const b = mount('join');
    b.joinColumns(IRIS, ['name', 'rank']);
    return { b, pill: b.el.querySelector('.jb-op-btn') };
  }
};

for (const [name, setup] of Object.entries(pills)) {
  test(`${name}: a second click on the operator pill closes its menu`, async () => {
    const { b, pill } = setup();
    assert.ok(pill, 'operator pill rendered');

    pill.click();
    assert.strictEqual(menus(b), 1);

    pill.click();
    await settle(b);
    assert.strictEqual(menus(b), 0);

    pill.click();
    assert.strictEqual(menus(b), 1);
    b.close();
  });

  test(`${name}: the pill opens a fresh menu after a pick closed the last one`, async () => {
    const { b, pill } = setup();
    pill.click();
    const opt = b.doc.querySelectorAll('.blockr-select__dropdown--menu .blockr-select__option')[1];
    opt.click();
    await settle(b);
    assert.strictEqual(menus(b), 0);

    pill.click();
    assert.strictEqual(menus(b), 1);
    b.close();
  });
}
