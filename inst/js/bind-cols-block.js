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

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'bind-cols',
    Block: BindColsBlock,
    messages: {
      'bind-cols-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
