// @ts-check
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

  /**
   * Block state as exchanged with R — mirrors make_bind_cols_expr() in
   * R/expr-builders.R, which takes no JS-driven parameters (arg_names is
   * supplied R-side). _compose() sends and setState() receives an empty
   * object.
   * @typedef {{}} BindColsState
   */

  class BindColsBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {((value: boolean) => void) | null} */
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

    /** @returns {BindColsState} */
    _compose() {
      return {};
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    /** @returns {BindColsState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {BindColsState | null | undefined} state
     * @param {boolean} [silent]
     */
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
