/**
 * PivotWiderBlock — JS-driven pivot_wider block input binding.
 *
 * Main UI: names_from multi-select (bordered), values_from multi-select (bordered),
 * id_cols multi-select (bordered).
 * Gear popover: values_fill, names_sep, names_prefix text inputs.
 * Auto-submits on any change (300ms debounce).
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  class PivotWiderBlock {
    constructor(el) {
      this.el = el;
      this.names_from = [];
      this.values_from = [];
      this.id_cols = [];
      this.values_fill = '';
      this.names_sep = '_';
      this.names_prefix = '';
      this.columnNames = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._namesFromSelect = null;
      this._valuesFromSelect = null;
      this._idColsSelect = null;
      this._popoverOpen = false;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'pwb-card';
      this.el.appendChild(this.card);

      // Gear header (top-right)
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = Blockr.icons.gear;
      this.gearBtn.title = 'Advanced settings';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // names_from picker (bordered)
      const namesFromWrap = document.createElement('div');
      namesFromWrap.className = 'pwb-picker-wrap blockr-select--bordered';
      const namesFromLabel = document.createElement('label');
      namesFromLabel.className = 'blockr-label';
      namesFromLabel.textContent = 'names_from';
      namesFromWrap.appendChild(namesFromLabel);
      this._namesFromSelect = Blockr.Select.multi(namesFromWrap, {
        options: this.columnNames,
        selected: [],
        placeholder: 'Name columns\u2026',
        reorderable: false,
        onChange: (selected) => {
          this.names_from = selected;
          this._autoSubmit();
        }
      });
      this.card.appendChild(namesFromWrap);

      // values_from picker (bordered)
      const valuesFromWrap = document.createElement('div');
      valuesFromWrap.className = 'pwb-picker-wrap blockr-select--bordered';
      const valuesFromLabel = document.createElement('label');
      valuesFromLabel.className = 'blockr-label';
      valuesFromLabel.textContent = 'values_from';
      valuesFromWrap.appendChild(valuesFromLabel);
      this._valuesFromSelect = Blockr.Select.multi(valuesFromWrap, {
        options: this.columnNames,
        selected: [],
        placeholder: 'Value columns\u2026',
        reorderable: false,
        onChange: (selected) => {
          this.values_from = selected;
          this._autoSubmit();
        }
      });
      this.card.appendChild(valuesFromWrap);

      // id_cols picker (bordered)
      const idColsWrap = document.createElement('div');
      idColsWrap.className = 'pwb-picker-wrap blockr-select--bordered';
      const idColsLabel = document.createElement('label');
      idColsLabel.className = 'blockr-label';
      idColsLabel.textContent = 'id_cols (optional)';
      idColsWrap.appendChild(idColsLabel);
      this._idColsSelect = Blockr.Select.multi(idColsWrap, {
        options: this.columnNames,
        selected: [],
        placeholder: 'ID columns\u2026',
        reorderable: false,
        onChange: (selected) => {
          this.id_cols = selected;
          this._autoSubmit();
        }
      });
      this.card.appendChild(idColsWrap);

      // Settings popover
      this._buildPopover();

      // Close popover on outside click
      document.addEventListener('click', (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(e.target) &&
            !this.gearBtn.contains(e.target)) {
          this._closePopover();
        }
      });
    }

    // --- Settings popover ---

    _buildPopover() {
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-popover';
      this.popoverEl.style.display = 'none';

      // values_fill
      const fillRow = document.createElement('div');
      fillRow.className = 'blockr-popover-row';
      const fillLabel = document.createElement('label');
      fillLabel.className = 'blockr-popover-label';
      fillLabel.textContent = 'Fill:';
      fillRow.appendChild(fillLabel);
      this._fillInput = document.createElement('input');
      this._fillInput.type = 'text';
      this._fillInput.className = 'blockr-popover-input';
      this._fillInput.value = this.values_fill;
      this._fillInput.placeholder = '(optional)';
      this._fillInput.addEventListener('input', () => {
        this.values_fill = this._fillInput.value;
        this._autoSubmit();
      });
      fillRow.appendChild(this._fillInput);
      this.popoverEl.appendChild(fillRow);

      // names_sep
      const sepRow = document.createElement('div');
      sepRow.className = 'blockr-popover-row';
      const sepLabel = document.createElement('label');
      sepLabel.className = 'blockr-popover-label';
      sepLabel.textContent = 'Separator:';
      sepRow.appendChild(sepLabel);
      this._sepInput = document.createElement('input');
      this._sepInput.type = 'text';
      this._sepInput.className = 'blockr-popover-input';
      this._sepInput.value = this.names_sep;
      this._sepInput.placeholder = '_';
      this._sepInput.addEventListener('input', () => {
        this.names_sep = this._sepInput.value;
        this._autoSubmit();
      });
      sepRow.appendChild(this._sepInput);
      this.popoverEl.appendChild(sepRow);

      // names_prefix
      const prefixRow = document.createElement('div');
      prefixRow.className = 'blockr-popover-row';
      const prefixLabel = document.createElement('label');
      prefixLabel.className = 'blockr-popover-label';
      prefixLabel.textContent = 'Prefix:';
      prefixRow.appendChild(prefixLabel);
      this._prefixInput = document.createElement('input');
      this._prefixInput.type = 'text';
      this._prefixInput.className = 'blockr-popover-input';
      this._prefixInput.value = this.names_prefix;
      this._prefixInput.placeholder = '(optional)';
      this._prefixInput.addEventListener('input', () => {
        this.names_prefix = this._prefixInput.value;
        this._autoSubmit();
      });
      prefixRow.appendChild(this._prefixInput);
      this.popoverEl.appendChild(prefixRow);

      this.card.appendChild(this.popoverEl);
    }

    _togglePopover() {
      if (this._popoverOpen) {
        this._closePopover();
      } else {
        this._openPopover();
      }
    }

    _openPopover() {
      this.popoverEl.style.display = 'block';
      this._popoverOpen = true;
      this.gearBtn.classList.add('blockr-gear-active');
    }

    _closePopover() {
      this.popoverEl.style.display = 'none';
      this._popoverOpen = false;
      this.gearBtn.classList.remove('blockr-gear-active');
    }

    _compose() {
      return {
        names_from: this.names_from.slice(),
        values_from: this.values_from.slice(),
        id_cols: this.id_cols.slice(),
        values_fill: this.values_fill || null,
        names_sep: this.names_sep,
        names_prefix: this.names_prefix
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

    setState(state) {
      this.names_from = (state?.names_from || []).slice();
      this.values_from = (state?.values_from || []).slice();
      this.id_cols = (state?.id_cols || []).slice();
      this.values_fill = state?.values_fill || '';
      this.names_sep = state?.names_sep ?? '_';
      this.names_prefix = state?.names_prefix || '';

      // Update popover text inputs
      this._fillInput.value = this.values_fill;
      this._sepInput.value = this.names_sep;
      this._prefixInput.value = this.names_prefix;

      // Update multi-selects
      if (this._namesFromSelect) {
        this._namesFromSelect.setOptions(this.columnNames, this.names_from);
      }
      if (this._valuesFromSelect) {
        this._valuesFromSelect.setOptions(this.columnNames, this.values_from);
      }
      if (this._idColsSelect) {
        this._idColsSelect.setOptions(this.columnNames, this.id_cols);
      }
    }

    updateColumns(names) {
      this.columnNames = names || [];
      if (this._namesFromSelect) {
        this._namesFromSelect.setOptions(this.columnNames, this.names_from);
        this.names_from = this._namesFromSelect.getValue();
      }
      if (this._valuesFromSelect) {
        this._valuesFromSelect.setOptions(this.columnNames, this.values_from);
        this.values_from = this._valuesFromSelect.getValue();
      }
      if (this._idColsSelect) {
        this._idColsSelect.setOptions(this.columnNames, this.id_cols);
        this.id_cols = this._idColsSelect.getValue();
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.pivot-wider-block-container'),
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
      el._block = new PivotWiderBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.pivot-wider');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('pivot-wider-columns', (msg) => {
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
  Shiny.addCustomMessageHandler('pivot-wider-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
