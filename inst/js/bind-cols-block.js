/**
 * BindColsBlock — JS-driven dplyr::bind_cols block input binding.
 *
 * Minimal UI — just a label. No user-configurable inputs.
 * Variadic block — no column message handler needed.
 *
 * Depends on: blockr-core.js
 */
(() => {
  'use strict';

  class BindColsBlock {
    constructor(el) {
      this.el = el;
      this._callback = null;
      this._submitted = false;

      this._buildDOM();
      // Auto-submit immediately since there are no inputs
      this._submit();
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'bcb-card';
      this.el.appendChild(this.card);

      const label = document.createElement('div');
      label.className = 'blockr-label';
      label.textContent = 'Combining columns side-by-side';
      this.card.appendChild(label);
    }

    _compose() {
      return {};
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    setState(state, silent) {
      // No state to set
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.bind-cols-block-container'),
    getId: (el) => el.id || null,
    getValue: (el) => el._block?.getValue() ?? null,
    setValue: (el, value) => el._block?.setState(value),
    subscribe: (el, callback) => {
      if (el._block) el._block._callback = () => callback(true);
    },
    unsubscribe: (el) => {
      if (el._block) el._block._callback = null;
    },
    initialize: (el) => {
      el._block = new BindColsBlock(el);
    },
    receiveMessage: (el, data) => {
      if (data.state) el._block?.setState(data.state);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.bind-cols');

  // External control state update handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('bind-cols-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state, true);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
