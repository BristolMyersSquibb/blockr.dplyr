// @ts-check
/**
 * blockr-core.js — the block protocol for JS-driven blocks: the Shiny input
 * binding (Blockr.registerBlock), the queue that replays messages sent before
 * a block's element bound, and the helpers that keep a restored column pick
 * (Blockr.reconcileColumn(s), Blockr.stateKey).
 *
 * Depends on: blockr-ui.js (the namespace, DOM helpers, icons).
 */
window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({});

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
  if (!queue) return false;
  Blockr._pending.delete(el.id);
  for (const fn of queue.values()) fn(el._block);
  return true;
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
      // Anything R sent before this element bound was parked by _enqueue.
      const replayed = Blockr._replayPending(el);
      // Nothing parked means one of two things: a genuinely fresh block, or a
      // block whose state and columns were pushed before THIS script existed
      // -- the deferred dock panel case, where the block's JS is delivered
      // with its panel on first visit and Shiny drops custom messages that
      // have no handler yet. No client-side queue can catch a message dropped
      // before the queue itself loaded, so announce instead and let R re-send
      // (js-block.R, `js_block_ready_name()`). The extra round trip is cheap
      // and idempotent when the block really is fresh.
      if (!replayed) {
        Shiny.setInputValue(`${el.id}_ready`, Date.now(), { priority: 'event' });
      }
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

/**
 * Point a column picker at a new option list without losing a deliberate pick.
 *
 * The BLOCK owns its column; the widget does not. Reading the pick back out of
 * the select after `setOptions()` and storing that as block state is what used
 * to lose a restored board: a column picker has no `allowEmpty`, so a list that
 * does not carry the block's column slides the widget onto option 0 — and
 * during a restore that list arrives routinely, because an upstream still
 * settling means `data()` is the OLD frame. The block adopted the fallback,
 * and nothing recovered it: `setState()` rebuilds from R's message, which does
 * not repeat. rename renamed a column nobody named, filter kept its values
 * while re-pointing at another column.
 *
 * So the two cases have to be told apart:
 *   pinned   — the board restored this column, or the user picked it. It
 *              survives a list that does not offer it (`updateOptions` swaps
 *              the list without touching the selection, so the face keeps
 *              showing it) and lands back on its option when the real columns
 *              arrive.
 *   unpinned — nothing but the select's own auto-pick of option 0, which is
 *              not a decision. It still follows the list, so re-pointing the
 *              block at different data moves it — and the face keeps matching
 *              the value.
 *
 * @param {BlockrSelectSingleHandle} select
 * @param {BlockrSelectOption[]} options
 * @param {string} column The owner's column, not the widget's.
 * @param {boolean} pinned
 * @returns {string} The column the owner should hold from here on.
 */
Blockr.reconcileColumn = (select, options, column, pinned) => {
  const offered = (options || []).map(
    (o) => (o && typeof o === 'object' ? o.value : o)
  );
  if (pinned && column && offered.indexOf(column) < 0) {
    select.updateOptions(options, column);
    return column;
  }
  select.setOptions(options, column || null);
  return select.getValue() || '';
};

/**
 * A comparable key for a block state, insensitive to key order and to the
 * scalar/length-1-array distinction.
 *
 * For deciding whether a block has anything new to tell R. A submit is not
 * free: `js_block_state()` writes the blob into the per-field reactiveVals and
 * arms `self_write` to swallow the echo, so a submit that says nothing costs a
 * round trip and leaves the guard armed against the next real change. What R
 * sent us is not news.
 *
 * A length-1 array and its bare element are the SAME value on the wire, so
 * they have to key the same: jsonlite auto-unboxes, and a single group-by
 * column reaches the client as `by: "Species"` while `_compose()` holds
 * `["Species"]`. Keying those apart made every restore with one group-by
 * column echo itself straight back at R, which is the case this guard exists
 * to stop. Unwrapping both sides cannot hide a real difference: the values
 * underneath are still compared.
 *
 * @param {unknown} state
 * @returns {string}
 */
Blockr.stateKey = (state) => JSON.stringify(state, (_key, value) => {
  if (Array.isArray(value)) return value.length === 1 ? value[0] : value;
  return value && typeof value === 'object'
    ? Object.keys(value).sort().reduce((sorted, k) => {
      sorted[k] = value[k];
      return sorted;
    }, /** @type {Record<string, unknown>} */ ({}))
    : value;
});

/**
 * The same, for a multi column picker.
 *
 * There is no auto-pick to tell apart here — a multi select never chooses
 * anything on its own, so everything in it was restored or picked, and all of
 * it is pinned. `setOptions()` would FILTER the selection against the new
 * list, which during a restore means a block whose columns the current frame
 * does not carry yet comes back EMPTY: no columns selected, no columns to
 * pivot, no group-by. Nothing brings them back either, so the next submit
 * writes the empty version over the board's.
 *
 * @param {BlockrSelectMultiHandle} select
 * @param {BlockrSelectOption[]} options
 * @param {string[]} columns The owner's columns, not the widget's.
 * @returns {string[]} The columns the owner should hold from here on.
 */
Blockr.reconcileColumns = (select, options, columns) => {
  const cols = (columns || []).slice();
  select.updateOptions(options, cols);
  return cols;
};
