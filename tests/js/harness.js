/* Mount a block's JavaScript in a headless DOM and drive it the way R does.
 *
 * The blocks in inst/js are the half of this package that R tests cannot
 * reach: `testServer()` sees the custom messages R sends, never what the
 * client does with them. The restore bugs live exactly there — in how a
 * block reconciles a state push against a column-metadata push that may
 * arrive before, after, or (while an upstream is still settling) with the
 * wrong columns in it. So the tests run the real files, through the real
 * `Blockr.registerBlock()` wiring, against a real DOM.
 *
 * What is stubbed is only Shiny itself:
 *   - `Shiny.inputBindings.register` captures the binding, so `initialize`,
 *     `getValue` and `subscribe` are the ones R would call.
 *   - `Shiny.addCustomMessageHandler` captures the handlers, so `send()`
 *     dispatches by the same channel names R sends on (js-block.R).
 *   - `Shiny.setInputValue` records `<id>_ready` announcements and lazy
 *     value requests.
 *
 * Usage:
 *   const b = mount('arrange');
 *   b.setState({ columns: [{ column: 'Species', direction: 'asc' }] });
 *   b.columns(['Sepal.Length', 'Species']);
 *   assert.deepStrictEqual(b.compose().columns, [...]);
 *   b.close();
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { Window } = require('happy-dom');

const JS_DIR = path.join(__dirname, '..', '..', 'inst', 'js');

/* blockr-ui.js, blockr-select.js and blockr-input.js are blockr.ui's
 * (controls_dep()). The tests read the installed copy, the one R serves;
 * BLOCKR_UI_JS points them at a source tree instead. */
const UI_FILES = ['blockr-ui.js', 'blockr-select.js', 'blockr-input.js'];
let uiDir = process.env.BLOCKR_UI_JS;
const UI_DIR = () => uiDir || (uiDir = require('node:child_process').execFileSync(
  'Rscript', ['-e', 'cat(system.file("assets", "js", package = "blockr.ui", mustWork = TRUE))'],
  { encoding: 'utf8' }
).trim());

const read = (file) => fs.readFileSync(
  path.join(UI_FILES.includes(file) ? UI_DIR() : JS_DIR, file), 'utf8'
);

/* The small icons, Blockr.icons, are not in blockr-ui.js: controls_dep()
 * writes them into the page's head ahead of it, built from blockr.ui's icon
 * files, one SVG each in the directory next to its scripts. The tests build
 * the same statement from those files, the way blockr.ui's own tests do: a
 * file holds the icon's note as a comment, so the icon is the file without
 * the comment and the whitespace between tags. */
const iconsScript = () => {
  const dir = path.join(UI_DIR(), '..', 'icons');
  const icons = Object.fromEntries(
    fs.readdirSync(dir).filter((f) => f.endsWith('.svg')).map((f) => [
      path.basename(f, '.svg'),
      fs.readFileSync(path.join(dir, f), 'utf8')
        .replace(/<!--[\s\S]*?-->/g, '')
        .replace(/>\s+</g, '><')
        .trim()
    ])
  );
  return `(window.Blockr = window.Blockr || {}).icons = ${JSON.stringify(icons)};`;
};

/* Objects built inside the window's realm have that realm's prototypes, which
 * `deepStrictEqual` counts as a difference. Serializing is also the honest
 * comparison: JSON is what Shiny puts on the wire. */
const json = (x) => (x === undefined ? undefined : JSON.parse(JSON.stringify(x)));

/**
 * The scripts a block loads with, in R's order (js_block_ui): the two files
 * every block gets, then the shared ones the block declares in its own
 * "Depends on:" header. Reading the header rather than keeping a list here
 * means a block that grows a dependency and forgets to declare it fails
 * loudly in its own test instead of quietly loading something the browser
 * would not have served it.
 */
const ALWAYS = ['blockr-ui.js', 'blockr-core.js'];

const scriptsFor = (src) => {
  const m = /Depends on:\s*(.+)/.exec(src);
  const declared = m
    ? m[1].split(',').map((s) => s.trim()).filter((s) => /\.js$/.test(s))
    : [];
  return ALWAYS.concat(declared.filter((f) => !ALWAYS.includes(f)));
};

/** Column metadata in the shape build_column_picker_meta() sends. */
const columnMeta = (cols) => cols.map((c) => (
  typeof c === 'string' ? { name: c, label: '', type: 'character' } : c
));

/**
 * @param {string} name kebab-case block name, e.g. 'arrange' or 'filter'
 * @param {{id?: string, columns?: Array<string|object>, state?: object,
 *          beforeInit?: (send: Function) => void}} [opts]
 */
function mount(name, opts = {}) {
  const id = opts.id || `${name}_input`;
  const win = new Window({ url: 'http://localhost/' });

  // Shiny + jQuery stand-ins, in the window's own realm so the block files
  // (which are browser IIFEs reaching for globals) load unmodified.
  win.eval(`
    window.__handlers = {};
    window.__inputs = [];
    window.__binding = null;
    window.Shiny = {
      InputBinding: function () {},
      inputBindings: { register: function (b) { window.__binding = b; } },
      addCustomMessageHandler: function (n, f) { window.__handlers[n] = f; },
      setInputValue: function (n, v) { window.__inputs.push({ name: n, value: v }); }
    };
    window.$ = window.jQuery = function () { return { find: function () { return []; } }; };
  `);

  const blockSrc = read(`${name}-block.js`);
  win.eval(iconsScript());
  for (const dep of scriptsFor(blockSrc)) win.eval(read(dep));
  win.eval(blockSrc);

  const doc = win.document;
  doc.body.innerHTML = `<div id="${id}" class="${name}-block-container"></div>`;
  const el = doc.getElementById(id);

  const binding = win.__binding;
  if (!binding) throw new Error(`${name}-block.js registered no input binding`);

  /** Values Shiny would have received, one per submit. */
  const submits = [];

  const api = {
    win,
    doc,
    el,
    binding,
    submits,

    /** The block instance (only after mount() returns; see initialize below). */
    get block() {
      return el._block;
    },

    /** What the block would send to R right now, submitted or not. */
    compose() {
      return json(el._block._compose());
    },

    /** What Shiny reads off the binding — null until the block has submitted. */
    value() {
      return json(binding.getValue(el));
    },

    /** Dispatch a custom message on the channel R sends it on. */
    send(channel, message) {
      const handler = win.__handlers[channel];
      if (!handler) throw new Error(`no handler registered for "${channel}"`);
      handler(Object.assign({ id }, message));
      return api;
    },

    /** R's `<name>-block-update` (js_block_state()). */
    setState(state) {
      return api.send(`${name}-block-update`, { state });
    },

    /** R's `<name>-columns` (build_column_picker_meta()). */
    columns(cols) {
      return api.send(`${name}-columns`, { columns: columnMeta(cols) });
    },

    /** join takes one column list per input rather than one per block. */
    joinColumns(xCols, yCols) {
      return api.send('join-columns', {
        xColumns: columnMeta(xCols),
        yColumns: columnMeta(yCols)
      });
    },

    /** `<id>_ready` announcements the client made (js_block_ready_name()). */
    readyAnnouncements() {
      return json(Array.from(win.__inputs).filter((i) => i.name === `${id}_ready`));
    },

    /** Everything the client pushed back as a Shiny input. */
    inputs() {
      return json(Array.from(win.__inputs));
    },

    /**
     * Every `.blockr-select` root in the block, in document order. Note that
     * this includes value pickers, not just column pickers — in filter, index
     * 0 is a row's column and index 1 its values.
     */
    selects() {
      return Array.from(el.querySelectorAll('.blockr-select'));
    },

    /** What a single select shows — the label the user reads off the face. */
    faceOf(select) {
      return select.querySelector('.blockr-select__value').textContent.trim();
    },

    /** Faces of every single select (column pickers), in document order. */
    faces() {
      return Array.from(el.querySelectorAll('.blockr-select--single')).map(api.faceOf);
    },

    /** The chips a multi select shows, in order. */
    chipsOf(select) {
      return Array.from(select.querySelectorAll('.blockr-select__tag'))
        .map((t) => t.getAttribute('data-value'));
    },

    /**
     * Pick an option the way a user does: open the control, click the option.
     * Goes through the widget's own event handlers, so it fires `onChange`
     * exactly as a real pick would.
     */
    pick(select, value) {
      select.querySelector('.blockr-select__control').click();
      // The list is portalled to <body>; the combobox input names it.
      const dropdown = doc.getElementById(
        select.querySelector('.blockr-select__search').getAttribute('aria-controls')
      );
      const opt = dropdown.querySelector(`.blockr-select__option[data-value="${value}"]`);
      if (!opt) {
        const offered = Array.from(dropdown.querySelectorAll('.blockr-select__option'))
          .map((o) => o.getAttribute('data-value'));
        throw new Error(`"${value}" is not offered; the select lists [${offered}]`);
      }
      opt.click();
      return api;
    },

    /** Pick in the nth select of the block (0-based). */
    pickNth(n, value) {
      return api.pick(api.selects()[n], value);
    },

    close() {
      win.close();
    }
  };

  // Messages sent before the element binds take the `Blockr._enqueue` path
  // and are replayed by `initialize` — the deferred dock panel case.
  if (opts.beforeInit) opts.beforeInit(api.send);

  binding.initialize(el);
  binding.subscribe(el, () => submits.push(json(binding.getValue(el))));

  if (opts.columns) api.columns(opts.columns);
  if (opts.state) api.setState(opts.state);

  return api;
}

module.exports = { mount, columnMeta };
