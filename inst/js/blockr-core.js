// @ts-check
/**
 * Blockr — shared namespace and utilities for blockr JS components.
 * Must be loaded before any component files (blockr-select.js, etc.).
 */
window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({});

Blockr.uid = (() => {
  let counter = 0;
  return (prefix = 'b') => `${prefix}-${++counter}`;
})();

Blockr.escapeHtml = (s) => {
  const div = document.createElement('div');
  div.appendChild(document.createTextNode(s));
  return div.innerHTML;
};

Blockr.removeNode = (node) => {
  node?.parentNode?.removeChild(node);
};

/**
 * Natural (nowrap, unconstrained) width of an element's rendered content.
 *
 * Clones the element's innerHTML into a hidden shrink-wrapped measurer in
 * the document, so class-based styles (e.g. .blockr-select__opt-label)
 * still apply. Row factories use this to size a shared left column to the
 * widest committed value instead of a fixed pixel width — the source
 * element itself is usually ellipsized (flex: 1 + overflow: hidden), so
 * its own scrollWidth can't be trusted for shrinking.
 *
 * @param {Element} el
 * @returns {number}
 */
Blockr.contentWidth = (el) => {
  let m = Blockr._measureEl;
  if (!m || !m.isConnected) {
    m = document.createElement('div');
    m.style.cssText =
      'position:absolute;left:-9999px;top:0;visibility:hidden;' +
      'white-space:nowrap;width:auto;pointer-events:none;';
    document.body.appendChild(m);
    Blockr._measureEl = m;
  }
  const cs = window.getComputedStyle(el);
  m.style.fontFamily = cs.fontFamily;
  m.style.fontSize = cs.fontSize;
  m.style.fontWeight = cs.fontWeight;
  m.innerHTML = el.innerHTML;
  const w = Math.ceil(m.getBoundingClientRect().width);
  m.innerHTML = '';
  return w;
};

/**
 * Document-click registry — one document-level listener for all blocks.
 *
 * Per-instance `document.addEventListener('click', ...)` calls leak: the
 * closure retains the block and its detached DOM forever once the block is
 * removed from the board. Entries here are dropped automatically when their
 * anchor element leaves the document, so removed blocks become collectable.
 *
 * Blockr.onDocClick(anchorEl, cb) -> calls cb(event) for every document
 * click while `anchorEl` is connected. The callback does its own
 * containment checks (e.g. close a popover unless the click hit it).
 */
Blockr._docClick = new Set();
document.addEventListener('click', (e) => {
  for (const entry of Blockr._docClick) {
    if (!entry.el.isConnected) {
      Blockr._docClick.delete(entry);
    } else {
      entry.cb(e);
    }
  }
});
Blockr.onDocClick = (el, cb) => {
  Blockr._docClick.add({ el, cb });
};

/**
 * Messages that arrived before their target element was created/bound.
 * Keyed by element id -> Map(channel -> latest fn), replayed on initialize.
 *
 * Shiny has no client-side queue for messages to unbound inputs, so this is
 * the queue that delivers a block's initial/restore state when its element
 * is inserted dynamically (add-block, board restore, on-demand dock tabs)
 * and binds after the message arrives.
 *
 * Entries are keyed by channel and keep only the LATEST message per channel
 * (state and columns pushes are full idempotent snapshots), so a per-id entry
 * is bounded by the number of channels and a block that binds arbitrarily
 * late still replays its current state. (A previous wall-clock 30s expiry
 * silently dropped the init state of blocks that bound >30s late -- e.g. a
 * dock tab opened minutes after construction -- leaving them blank.)
 */
Blockr._pending = new Map();
Blockr._enqueue = (id, channel, fn) => {
  const queue = Blockr._pending.get(id) || new Map();
  queue.set(channel, fn);
  Blockr._pending.set(id, queue);
};
Blockr._replayPending = (el) => {
  const queue = Blockr._pending.get(el.id);
  if (!queue) return;
  Blockr._pending.delete(el.id);
  for (const fn of queue.values()) fn(el._block);
};

/**
 * Register a JS-driven block: input binding + custom message handlers.
 *
 * The block class must implement:
 *   getValue()           -> state JSON after first submit, else null
 *   setState(state)      -> rebuild UI from state, never fires the callback
 * and exposes user changes by calling `this._callback?.(true)` (a `_submit`).
 *
 * config:
 *   name           kebab-case block name; binding id `blockr.<name>`,
 *                  container class `<name>-block-container`
 *   Block          the block class, constructed as `new Block(el)`
 *   messages       { 'msg-name': (block, msg) => ... } — handlers are
 *                  dispatched by msg.id and queued until the element binds
 */
Blockr.registerBlock = ({ name, Block, messages = {} }) => {
  const containerClass = `${name}-block-container`;

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    /** @param {HTMLElement} scope */
    find: (scope) => $(scope).find(`.${containerClass}`),
    /** @param {BlockrBlockHost} el */
    getId: (el) => el.id || null,
    /** @param {BlockrBlockHost} el */
    getValue: (el) => el._block?.getValue() ?? null,
    /** @param {BlockrBlockHost} el @param {unknown} value */
    setValue: (el, value) => el._block?.setState(value),
    /** @param {BlockrBlockHost} el @param {{state?: unknown}} data */
    receiveMessage: (el, data) => {
      if (data.state) el._block?.setState(data.state);
    },
    /** @param {BlockrBlockHost} el @param {(value: boolean) => void} callback */
    subscribe: (el, callback) => {
      if (el._block) el._block._callback = () => callback(true);
    },
    /** @param {BlockrBlockHost} el */
    unsubscribe: (el) => {
      if (el._block) el._block._callback = null;
    },
    /** @param {BlockrBlockHost} el */
    initialize: (el) => {
      if (!el._block) el._block = new Block(el);
      Blockr._replayPending(el);
    }
  });
  Shiny.inputBindings.register(binding, `blockr.${name}`);

  for (const [msgName, handler] of Object.entries(messages)) {
    Shiny.addCustomMessageHandler(msgName, (msg) => {
      const el = /** @type {BlockrBlockHost | null} */ (
        document.getElementById(msg.id)
      );
      if (el?._block) {
        handler(el._block, msg);
      } else {
        Blockr._enqueue(msg.id, msgName, (block) => handler(block, msg));
      }
    });
  }
};

Blockr.icons = {
  chevron:
    '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" stroke="currentColor" ' +
    'stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round">' +
    '<polyline points="3 4.5 6 7.5 9 4.5"></polyline></svg>',
  remove:
    '<svg width="10" height="10" viewBox="0 0 10 10" fill="none" stroke="currentColor" ' +
    'stroke-width="1.5" stroke-linecap="round">' +
    '<line x1="2.5" y1="2.5" x2="7.5" y2="7.5"></line>' +
    '<line x1="7.5" y1="2.5" x2="2.5" y2="7.5"></line></svg>',
  x:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 ' +
    '.708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>',
  plus:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 ' +
    '0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>',
  confirm:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 ' +
    '1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>',
  code:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M10.478 1.647a.5.5 0 1 0-.956-.294l-4 13a.5.5 0 0 0 .956.294z' +
    'M4.854 4.146a.5.5 0 0 1 0 .708L1.707 8l3.147 3.146a.5.5 0 0 1-.708.708l-3.5-3.5a.5.5 0 0 1 ' +
    '0-.708l3.5-3.5a.5.5 0 0 1 .708 0m6.292 0a.5.5 0 0 0 0 .708L14.293 8l-3.147 3.146a.5.5 0 0 0 ' +
    '.708.708l3.5-3.5a.5.5 0 0 0 0-.708l-3.5-3.5a.5.5 0 0 0-.708 0"/></svg>',
  gear:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 ' +
    '1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82' +
    '.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 ' +
    '.872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 ' +
    '2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31' +
    '.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-' +
    '.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-' +
    '1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 ' +
    '2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
};

/**
 * Toggle the canonical required-empty amber cue (blockr-blocks.css
 * .blockr-field--required-empty) on a field wrapper or standalone input.
 * One name keeps call sites greppable for the blockr.ui move.
 * @param {Element} el
 * @param {boolean} empty
 */
Blockr.setRequiredEmpty = (el, empty) => {
  el.classList.toggle('blockr-field--required-empty', !!empty);
};

/**
 * Commit-on-Enter text input (design-system §5.5): typing never submits —
 * a chip arms with "Enter ↵" while the value is dirty, the value commits on
 * Enter, blur or the chip (which then fades to the ✓ icon), and Escape
 * reverts to the last committed value.
 *
 * The input must already sit in its parent: the chip is inserted directly
 * after it. Programmatic value changes (setState restores, mode switches)
 * go through the returned `sync(value)`, which resets the committed
 * baseline so a restored value never shows an armed chip.
 *
 * @param {HTMLInputElement} input
 * @param {{ onCommit: (value: string) => void, compact?: boolean }} opts
 *   `compact` renders a bare ↵ chip for tight in-row contexts.
 * @returns {{ chip: HTMLButtonElement, commit: () => void,
 *             sync: (value: string) => void }}
 */
Blockr.textCommit = (input, opts) => {
  const compact = !!opts.compact;
  const chip = document.createElement('button');
  chip.type = 'button';
  chip.className = 'blockr-expr-confirm';
  chip.title = 'Apply (Enter)';
  chip.setAttribute('aria-label', 'Apply (Enter)');
  chip.style.display = 'none';
  let committed = input.value;
  let everCommitted = false;
  const armed = compact ?
    '<span class="blockr-kbd">↵</span>' :
    'Enter <span class="blockr-kbd">↵</span>';
  const syncChip = () => {
    if (input.value !== committed) {
      chip.style.display = '';
      chip.classList.remove('confirmed');
      chip.innerHTML = armed;
    } else if (everCommitted) {
      chip.style.display = '';
      chip.classList.add('confirmed');
      chip.innerHTML = Blockr.icons.confirm;
    } else {
      chip.style.display = 'none';
    }
  };
  const commit = () => {
    if (input.value === committed) return;
    committed = input.value;
    everCommitted = true;
    opts.onCommit(input.value);
    syncChip();
  };
  input.addEventListener('input', syncChip);
  input.addEventListener('keydown', (e) => {
    if (e.key === 'Enter') { e.preventDefault(); commit(); }
    else if (e.key === 'Escape') { input.value = committed; syncChip(); }
  });
  input.addEventListener('blur', commit);
  // Keep focus on the input so the chip click doesn't race blur-commit.
  chip.addEventListener('mousedown', (e) => e.preventDefault());
  chip.addEventListener('click', commit);
  /** @type {Element} */ (input.parentElement).insertBefore(chip, input.nextSibling);
  return {
    chip,
    commit,
    sync: (value) => {
      input.value = value;
      committed = value;
      everCommitted = false;
      syncChip();
    }
  };
};
