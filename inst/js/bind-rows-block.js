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

  class BindRowsBlock {
    constructor(el) {
      this.el = el;
      this.id_name = '';
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
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
        this.id_name = this._idInput.value;
        this._autoSubmit();
      });
      fieldWrap.appendChild(this._idInput);
      this.card.appendChild(fieldWrap);
    }

    _compose() {
      return {
        id_name: this.id_name
      };
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
      this.id_name = state?.id_name || '';
      this._idInput.value = this.id_name;
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.bind-rows-block-container'),
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
      el._block = new BindRowsBlock(el);
      if (el._pendingState) {
        el._block.setState(el._pendingState);
        delete el._pendingState;
      }
    },
    receiveMessage: (el, data) => {
      if (data.state) el._block?.setState(data.state);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.bind-rows');

  // External control state update handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('bind-rows-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state, true);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
