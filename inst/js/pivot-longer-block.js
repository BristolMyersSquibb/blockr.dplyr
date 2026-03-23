/**
 * PivotLongerBlock — JS-driven pivot_longer block input binding.
 *
 * Main UI: column multi-select (bordered), names_to text input, values_to text input.
 * Gear popover: values_drop_na toggle, names_prefix text input.
 * Auto-submits on any change (300ms debounce).
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  class PivotLongerBlock {
    constructor(el) {
      this.el = el;
      this.cols = [];
      this.names_to = 'name';
      this.values_to = 'value';
      this.values_drop_na = false;
      this.names_prefix = '';
      this.columnNames = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._multiSelect = null;
      this._popoverOpen = false;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'plb-card';
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

      // Column picker (bordered)
      const pickerWrap = document.createElement('div');
      pickerWrap.className = 'plb-picker-wrap blockr-select--bordered';
      const pickerLabel = document.createElement('label');
      pickerLabel.className = 'blockr-label';
      pickerLabel.textContent = 'Columns';
      pickerWrap.appendChild(pickerLabel);
      this.card.appendChild(pickerWrap);

      this._multiSelect = Blockr.Select.multi(pickerWrap, {
        options: this.columnNames,
        selected: [],
        placeholder: 'Select columns\u2026',
        reorderable: true,
        onChange: (selected) => {
          this.cols = selected;
          this._autoSubmit();
        }
      });

      // Text inputs row: names_to + values_to
      const inputRow = document.createElement('div');
      inputRow.className = 'plb-input-row';

      // names_to
      const namesToWrap = document.createElement('div');
      namesToWrap.className = 'plb-field';
      const namesToLabel = document.createElement('label');
      namesToLabel.className = 'blockr-label';
      namesToLabel.textContent = 'Names to';
      namesToWrap.appendChild(namesToLabel);
      this._namesToInput = document.createElement('input');
      this._namesToInput.type = 'text';
      this._namesToInput.className = 'blockr-text-input plb-text-input';
      this._namesToInput.value = this.names_to;
      this._namesToInput.placeholder = 'name';
      this._namesToInput.addEventListener('input', () => {
        this.names_to = this._namesToInput.value;
        this._autoSubmit();
      });
      namesToWrap.appendChild(this._namesToInput);
      inputRow.appendChild(namesToWrap);

      // values_to
      const valuesToWrap = document.createElement('div');
      valuesToWrap.className = 'plb-field';
      const valuesToLabel = document.createElement('label');
      valuesToLabel.className = 'blockr-label';
      valuesToLabel.textContent = 'Values to';
      valuesToWrap.appendChild(valuesToLabel);
      this._valuesToInput = document.createElement('input');
      this._valuesToInput.type = 'text';
      this._valuesToInput.className = 'blockr-text-input plb-text-input';
      this._valuesToInput.value = this.values_to;
      this._valuesToInput.placeholder = 'value';
      this._valuesToInput.addEventListener('input', () => {
        this.values_to = this._valuesToInput.value;
        this._autoSubmit();
      });
      valuesToWrap.appendChild(this._valuesToInput);
      inputRow.appendChild(valuesToWrap);

      this.card.appendChild(inputRow);

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

      // values_drop_na toggle
      this._dropNaToggle = document.createElement('button');
      this._dropNaToggle.type = 'button';
      this._dropNaToggle.className = 'blockr-pill blockr-popover-toggle';
      this._dropNaToggle.textContent = 'Drop NA';
      this._dropNaToggle.addEventListener('click', () => {
        this.values_drop_na = !this.values_drop_na;
        this._dropNaToggle.classList.toggle('blockr-popover-toggle-active', this.values_drop_na);
        this._autoSubmit();
      });
      this.popoverEl.appendChild(this._dropNaToggle);

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
        cols: this.cols.slice(),
        names_to: this.names_to || 'name',
        values_to: this.values_to || 'value',
        values_drop_na: this.values_drop_na,
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
      this.cols = (state?.cols || []).slice();
      this.names_to = state?.names_to || 'name';
      this.values_to = state?.values_to || 'value';
      this.values_drop_na = !!state?.values_drop_na;
      this.names_prefix = state?.names_prefix || '';

      // Update text inputs
      this._namesToInput.value = this.names_to;
      this._valuesToInput.value = this.values_to;

      // Update popover controls
      this._prefixInput.value = this.names_prefix;
      this._dropNaToggle.classList.toggle('blockr-popover-toggle-active', this.values_drop_na);

      // Update multi-select
      if (this._multiSelect) {
        this._multiSelect.setOptions(this.columnNames, this.cols);
      }
    }

    updateColumns(names) {
      this.columnNames = names || [];
      if (this._multiSelect) {
        this._multiSelect.setOptions(this.columnNames, this.cols);
        this.cols = this._multiSelect.getValue();
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.pivot-longer-block-container'),
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
      el._block = new PivotLongerBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.pivot-longer');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('pivot-longer-columns', (msg) => {
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
  Shiny.addCustomMessageHandler('pivot-longer-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
