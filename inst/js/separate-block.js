/**
 * SeparateBlock — JS-driven tidyr::separate block input binding.
 *
 * Main UI: source column single-select (bordered), into text input
 * (comma-separated names), separator text input, remove/convert toggle pills.
 * Auto-submits on any change (300ms debounce).
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  class SeparateBlock {
    constructor(el) {
      this.el = el;
      this.col = '';
      this.into = [];
      this.sep = '[^[:alnum:]]+';
      this.remove = true;
      this.convert = false;
      this.columnNames = [];
      this.columnOptions = [];
      this.columnMeta = {};
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._colSelect = null;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'spb-card';
      this.el.appendChild(this.card);

      // Source column picker (bordered)
      const colWrap = document.createElement('div');
      colWrap.className = 'spb-col-wrap blockr-select--bordered';
      const colLabel = document.createElement('label');
      colLabel.className = 'blockr-label';
      colLabel.textContent = 'Column';
      colWrap.appendChild(colLabel);
      this._colSelect = Blockr.Select.single(colWrap, {
        options: this.columnOptions,
        selected: null,
        placeholder: 'Select column\u2026',
        onChange: (value) => {
          this.col = value;
          this._autoSubmit();
        }
      });
      this.card.appendChild(colWrap);

      // Text inputs row: into + sep
      const inputRow = document.createElement('div');
      inputRow.className = 'spb-input-row';

      // into (comma-separated names)
      const intoWrap = document.createElement('div');
      intoWrap.className = 'spb-field';
      const intoLabel = document.createElement('label');
      intoLabel.className = 'blockr-label';
      intoLabel.textContent = 'Into (comma-separated)';
      intoWrap.appendChild(intoLabel);
      this._intoInput = document.createElement('input');
      this._intoInput.type = 'text';
      this._intoInput.className = 'blockr-text-input spb-text-input';
      this._intoInput.value = '';
      this._intoInput.placeholder = 'col1, col2, col3';
      this._intoInput.addEventListener('input', () => {
        this.into = this._parseInto(this._intoInput.value);
        this._autoSubmit();
      });
      intoWrap.appendChild(this._intoInput);
      inputRow.appendChild(intoWrap);

      // sep
      const sepWrap = document.createElement('div');
      sepWrap.className = 'spb-field spb-field-narrow';
      const sepLabel = document.createElement('label');
      sepLabel.className = 'blockr-label';
      sepLabel.textContent = 'Separator';
      sepWrap.appendChild(sepLabel);
      this._sepInput = document.createElement('input');
      this._sepInput.type = 'text';
      this._sepInput.className = 'blockr-text-input spb-text-input';
      this._sepInput.value = this.sep;
      this._sepInput.placeholder = '[^[:alnum:]]+';
      this._sepInput.addEventListener('input', () => {
        this.sep = this._sepInput.value;
        this._autoSubmit();
      });
      sepWrap.appendChild(this._sepInput);
      inputRow.appendChild(sepWrap);

      this.card.appendChild(inputRow);

      // Toggle bar: remove + convert pills
      const toggleBar = document.createElement('div');
      toggleBar.className = 'blockr-add-row';

      this._removeToggle = document.createElement('button');
      this._removeToggle.type = 'button';
      this._removeToggle.className = 'blockr-pill sb-toggle sb-toggle-active';
      this._removeToggle.textContent = 'remove original';
      this._removeToggle.title = 'Toggle whether the source column is removed after splitting';
      this._removeToggle.addEventListener('click', () => {
        this.remove = !this.remove;
        this._removeToggle.textContent = this.remove ? 'remove original' : 'keep original';
        this._removeToggle.classList.toggle('sb-toggle-active', this.remove);
        this._autoSubmit();
      });
      toggleBar.appendChild(this._removeToggle);

      this._convertToggle = document.createElement('button');
      this._convertToggle.type = 'button';
      this._convertToggle.className = 'blockr-pill sb-toggle';
      this._convertToggle.textContent = 'keep types';
      this._convertToggle.title = 'Toggle whether split values are auto-converted to numbers or logicals';
      this._convertToggle.addEventListener('click', () => {
        this.convert = !this.convert;
        this._convertToggle.textContent = this.convert ? 'auto-convert' : 'keep types';
        this._convertToggle.classList.toggle('sb-toggle-active', this.convert);
        this._autoSubmit();
      });
      toggleBar.appendChild(this._convertToggle);

      this.card.appendChild(toggleBar);
    }

    _parseInto(text) {
      return text.split(',')
        .map(s => s.trim())
        .filter(s => s.length > 0);
    }

    _compose() {
      return {
        col: this.col,
        into: this.into.slice(),
        sep: this.sep,
        remove: this.remove,
        convert: this.convert
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
      this.col = state?.col || '';
      this.into = (state?.into || []).slice();
      this.sep = state?.sep ?? '[^[:alnum:]]+';
      this.remove = state?.remove !== false;
      this.convert = !!state?.convert;

      // Update column select
      if (this._colSelect) {
        this._colSelect.setOptions(this.columnOptions, this.col || null);
      }

      // Update text inputs
      this._intoInput.value = this.into.join(', ');
      this._sepInput.value = this.sep;

      // Update toggles
      this._removeToggle.textContent = this.remove ? 'remove original' : 'keep original';
      this._removeToggle.classList.toggle('sb-toggle-active', this.remove);
      this._convertToggle.textContent = this.convert ? 'auto-convert' : 'keep types';
      this._convertToggle.classList.toggle('sb-toggle-active', this.convert);
    }

    updateColumns(meta) {
      this.columnMeta = {};
      this.columnNames = [];
      this.columnOptions = [];
      for (const col of (meta || [])) {
        this.columnMeta[col.name] = col;
        this.columnNames.push(col.name);
        this.columnOptions.push({ value: col.name, label: col.label || '' });
      }
      if (this._colSelect) {
        const current = this._colSelect.getValue();
        this._colSelect.setOptions(this.columnOptions, current);
        this.col = this._colSelect.getValue();
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.separate-block-container'),
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
      el._block = new SeparateBlock(el);
      if (el._pendingColumns) {
        el._block.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
      if (el._pendingState) {
        el._block.setState(el._pendingState);
        delete el._pendingState;
      }
    },
    receiveMessage: (el, data) => {
      if (data.state) el._block?.setState(data.state);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.separate');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('separate-columns', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.updateColumns(msg.columns);
    } else if (el) {
      el._pendingColumns = msg.columns;
    } else {
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) { el2._block.updateColumns(msg.columns); clearInterval(t); }
        else if (el2) { el2._pendingColumns = msg.columns; clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });

  // External control state update handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('separate-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state, true);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
