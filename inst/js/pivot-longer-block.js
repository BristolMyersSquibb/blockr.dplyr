// @ts-check
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

  /**
   * Block state as exchanged with R — mirrors the parameters of
   * make_pivot_longer_expr() in R/expr-builders.R (what _compose()
   * sends and setState() receives).
   * @typedef {Object} PivotLongerState
   * @property {string[]} cols Columns to pivot into longer format
   * @property {string} names_to Name of the new names column (default "name")
   * @property {string} values_to Name of the new values column (default "value")
   * @property {boolean} values_drop_na Drop rows whose value is NA
   * @property {string} names_prefix Prefix stripped from column names ("" = none)
   */

  class PivotLongerBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {string[]} */
      this.cols = [];
      this.names_to = 'name';
      this.values_to = 'value';
      this.values_drop_na = false;
      this.names_prefix = '';
      /** @type {string[]} */
      this.columnNames = [];
      /** @type {BlockrSelectOption[]} */
      this.columnOptions = [];
      /** @type {Record<string, BlockrPickerColumn>} */
      this.columnMeta = {};
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {ReturnType<typeof setTimeout> | null} */
      this._debounceTimer = null;
      /** @type {BlockrSelectMultiHandle | null} */
      this._multiSelect = null;
      this._popoverOpen = false;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(/** @type {ReturnType<typeof setTimeout>} */ (this._debounceTimer));
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

      this._multiSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(pickerWrap, {
        options: this.columnOptions,
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
        this.names_to = /** @type {HTMLInputElement} */ (this._namesToInput).value;
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
        this.values_to = /** @type {HTMLInputElement} */ (this._valuesToInput).value;
        this._autoSubmit();
      });
      valuesToWrap.appendChild(this._valuesToInput);
      inputRow.appendChild(valuesToWrap);

      this.card.appendChild(inputRow);

      // Settings popover
      this._buildPopover();

      // Close popover on outside click
      Blockr.onDocClick(this.el, (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(/** @type {Node | null} */ (e.target)) &&
            !(/** @type {HTMLButtonElement} */ (this.gearBtn)).contains(/** @type {Node | null} */ (e.target))) {
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
      this._dropNaToggle.textContent = 'Keep NA';
      this._dropNaToggle.title = 'Toggle whether rows with NA values are dropped from the result';
      this._dropNaToggle.addEventListener('click', () => {
        this.values_drop_na = !this.values_drop_na;
        /** @type {HTMLButtonElement} */ (this._dropNaToggle).textContent = this.values_drop_na ? 'Drop NA' : 'Keep NA';
        /** @type {HTMLButtonElement} */ (this._dropNaToggle).classList.toggle('blockr-popover-toggle-active', this.values_drop_na);
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
        this.names_prefix = /** @type {HTMLInputElement} */ (this._prefixInput).value;
        this._autoSubmit();
      });
      prefixRow.appendChild(this._prefixInput);
      this.popoverEl.appendChild(prefixRow);

      /** @type {HTMLDivElement} */ (this.card).appendChild(this.popoverEl);
    }

    _togglePopover() {
      if (this._popoverOpen) {
        this._closePopover();
      } else {
        this._openPopover();
      }
    }

    _openPopover() {
      /** @type {HTMLDivElement} */ (this.popoverEl).style.display = 'block';
      this._popoverOpen = true;
      /** @type {HTMLButtonElement} */ (this.gearBtn).classList.add('blockr-gear-active');
    }

    _closePopover() {
      /** @type {HTMLDivElement} */ (this.popoverEl).style.display = 'none';
      this._popoverOpen = false;
      /** @type {HTMLButtonElement} */ (this.gearBtn).classList.remove('blockr-gear-active');
    }

    /** @returns {PivotLongerState} */
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

    /** @returns {PivotLongerState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {Partial<PivotLongerState> | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      this.cols = (state?.cols || []).slice();
      this.names_to = state?.names_to || 'name';
      this.values_to = state?.values_to || 'value';
      this.values_drop_na = !!state?.values_drop_na;
      this.names_prefix = state?.names_prefix || '';

      // Update text inputs
      /** @type {HTMLInputElement} */ (this._namesToInput).value = this.names_to;
      /** @type {HTMLInputElement} */ (this._valuesToInput).value = this.values_to;

      // Update popover controls
      /** @type {HTMLInputElement} */ (this._prefixInput).value = this.names_prefix;
      /** @type {HTMLButtonElement} */ (this._dropNaToggle).textContent = this.values_drop_na ? 'Drop NA' : 'Keep NA';
      /** @type {HTMLButtonElement} */ (this._dropNaToggle).classList.toggle('blockr-popover-toggle-active', this.values_drop_na);

      // Update multi-select
      if (this._multiSelect) {
        this._multiSelect.setOptions(this.columnOptions, this.cols);
      }
    }

    /** @param {BlockrPickerColumn[] | null | undefined} meta */
    updateColumns(meta) {
      this.columnMeta = {};
      this.columnNames = [];
      this.columnOptions = [];
      for (const col of (meta || [])) {
        this.columnMeta[col.name] = col;
        this.columnNames.push(col.name);
        this.columnOptions.push({ value: col.name, label: col.label || '' });
      }
      if (this._multiSelect) {
        this._multiSelect.setOptions(this.columnOptions, this.cols);
        this.cols = this._multiSelect.getValue();
      }
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'pivot-longer',
    Block: PivotLongerBlock,
    messages: {
      'pivot-longer-columns': (block, msg) => block.updateColumns(msg.columns),
      'pivot-longer-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
