/**
 * UniteBlock — JS-driven tidyr::unite block input binding.
 *
 * Main UI: new column name text input, columns multi-select (bordered),
 * separator text input.
 * Gear popover: remove toggle, na.rm toggle.
 * Auto-submits on any change (300ms debounce).
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  class UniteBlock {
    constructor(el) {
      this.el = el;
      this.col = 'united';
      this.cols = [];
      this.sep = '_';
      this.remove = true;
      this.na_rm = false;
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
      this.card.className = 'ub-card';
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

      // Top row: new column name + separator
      const topRow = document.createElement('div');
      topRow.className = 'ub-input-row';

      // col (new column name)
      const colWrap = document.createElement('div');
      colWrap.className = 'ub-field';
      const colLabel = document.createElement('label');
      colLabel.className = 'blockr-label';
      colLabel.textContent = 'New column';
      colWrap.appendChild(colLabel);
      this._colInput = document.createElement('input');
      this._colInput.type = 'text';
      this._colInput.className = 'blockr-text-input ub-text-input';
      this._colInput.value = this.col;
      this._colInput.placeholder = 'united';
      this._colInput.addEventListener('input', () => {
        this.col = this._colInput.value;
        this._autoSubmit();
      });
      colWrap.appendChild(this._colInput);
      topRow.appendChild(colWrap);

      // sep
      const sepWrap = document.createElement('div');
      sepWrap.className = 'ub-field ub-field-narrow';
      const sepLabel = document.createElement('label');
      sepLabel.className = 'blockr-label';
      sepLabel.textContent = 'Separator';
      sepWrap.appendChild(sepLabel);
      this._sepInput = document.createElement('input');
      this._sepInput.type = 'text';
      this._sepInput.className = 'blockr-text-input ub-text-input';
      this._sepInput.value = this.sep;
      this._sepInput.placeholder = '_';
      this._sepInput.addEventListener('input', () => {
        this.sep = this._sepInput.value;
        this._autoSubmit();
      });
      sepWrap.appendChild(this._sepInput);
      topRow.appendChild(sepWrap);

      this.card.appendChild(topRow);

      // Column picker (bordered)
      const pickerWrap = document.createElement('div');
      pickerWrap.className = 'ub-picker-wrap blockr-select--bordered';
      const pickerLabel = document.createElement('label');
      pickerLabel.className = 'blockr-label';
      pickerLabel.textContent = 'Columns';
      pickerWrap.appendChild(pickerLabel);
      this.card.appendChild(pickerWrap);

      this._multiSelect = Blockr.Select.multi(pickerWrap, {
        options: this.columnNames,
        selected: [],
        placeholder: 'Columns to unite\u2026',
        reorderable: true,
        onChange: (selected) => {
          this.cols = selected;
          this._autoSubmit();
        }
      });

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

      // remove toggle
      const removeRow = document.createElement('div');
      removeRow.className = 'blockr-popover-row';
      const removeLabel = document.createElement('label');
      removeLabel.className = 'blockr-popover-label';
      removeLabel.textContent = 'Remove:';
      removeRow.appendChild(removeLabel);
      this._removeToggle = document.createElement('button');
      this._removeToggle.type = 'button';
      this._removeToggle.className = 'blockr-pill ub-popover-toggle ub-popover-toggle-active';
      this._removeToggle.textContent = 'on';
      this._removeToggle.addEventListener('click', () => {
        this.remove = !this.remove;
        this._removeToggle.textContent = this.remove ? 'on' : 'off';
        this._removeToggle.classList.toggle('ub-popover-toggle-active', this.remove);
        this._autoSubmit();
      });
      removeRow.appendChild(this._removeToggle);
      this.popoverEl.appendChild(removeRow);

      // na.rm toggle
      const naRmRow = document.createElement('div');
      naRmRow.className = 'blockr-popover-row';
      const naRmLabel = document.createElement('label');
      naRmLabel.className = 'blockr-popover-label';
      naRmLabel.textContent = 'na.rm:';
      naRmRow.appendChild(naRmLabel);
      this._naRmToggle = document.createElement('button');
      this._naRmToggle.type = 'button';
      this._naRmToggle.className = 'blockr-pill ub-popover-toggle';
      this._naRmToggle.textContent = 'off';
      this._naRmToggle.addEventListener('click', () => {
        this.na_rm = !this.na_rm;
        this._naRmToggle.textContent = this.na_rm ? 'on' : 'off';
        this._naRmToggle.classList.toggle('ub-popover-toggle-active', this.na_rm);
        this._autoSubmit();
      });
      naRmRow.appendChild(this._naRmToggle);
      this.popoverEl.appendChild(naRmRow);

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
        col: this.col || 'united',
        cols: this.cols.slice(),
        sep: this.sep,
        remove: this.remove,
        na_rm: this.na_rm
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
      this.col = state?.col || 'united';
      this.cols = (state?.cols || []).slice();
      this.sep = state?.sep ?? '_';
      this.remove = state?.remove !== false;
      this.na_rm = !!state?.na_rm;

      // Update text inputs
      this._colInput.value = this.col;
      this._sepInput.value = this.sep;

      // Update popover toggles
      this._removeToggle.textContent = this.remove ? 'on' : 'off';
      this._removeToggle.classList.toggle('ub-popover-toggle-active', this.remove);
      this._naRmToggle.textContent = this.na_rm ? 'on' : 'off';
      this._naRmToggle.classList.toggle('ub-popover-toggle-active', this.na_rm);

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
    find: (scope) => $(scope).find('.unite-block-container'),
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
      el._block = new UniteBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.unite');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('unite-columns', (msg) => {
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
  Shiny.addCustomMessageHandler('unite-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
