// @ts-check
/**
 * BindRowsBlock — JS-driven dplyr::bind_rows block input binding.
 *
 * Simple: just a text input for id_name (optional).
 * Auto-submits on any change (300ms debounce).
 * Variadic block — no column message handler needed.
 *
 * Depends on: blockr-core.js
 */
(() => {
  'use strict';

  /**
   * Block state as exchanged with R — mirrors make_bind_rows_expr() in
   * R/expr-builders.R (what _compose() sends and setState() receives;
   * arg_names is supplied R-side).
   * @typedef {Object} BindRowsState
   * @property {string} id_name Optional .id column name ('' = none)
   */

  class BindRowsBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      this.id_name = '';
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {ReturnType<typeof setTimeout> | null} */
      this._debounceTimer = null;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(/** @type {ReturnType<typeof setTimeout>} */ (this._debounceTimer));
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'brb-card';
      this.el.appendChild(this.card);

      // id_name input
      const fieldWrap = document.createElement('div');
      fieldWrap.className = 'brb-field';
      const label = document.createElement('label');
      label.className = 'blockr-label';
      label.textContent = '.id column name (optional)';
      fieldWrap.appendChild(label);

      this._idInput = document.createElement('input');
      this._idInput.type = 'text';
      this._idInput.className = 'blockr-text-input brb-text-input';
      this._idInput.value = '';
      this._idInput.placeholder = 'e.g. source';
      this._idInput.addEventListener('input', () => {
        this.id_name = /** @type {HTMLInputElement} */ (this._idInput).value;
        this._autoSubmit();
      });
      fieldWrap.appendChild(this._idInput);
      this.card.appendChild(fieldWrap);
    }

    /** @returns {BindRowsState} */
    _compose() {
      return {
        id_name: this.id_name
      };
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    /** @returns {BindRowsState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {BindRowsState | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      this.id_name = state?.id_name || '';
      /** @type {HTMLInputElement} */ (this._idInput).value = this.id_name;
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'bind-rows',
    Block: BindRowsBlock,
    messages: {
      'bind-rows-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
