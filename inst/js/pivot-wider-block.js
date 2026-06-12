// @ts-check
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

  /**
   * Block state as exchanged with R — mirrors the parameters of
   * make_pivot_wider_expr() in R/expr-builders.R (what _compose()
   * sends and setState() receives).
   * @typedef {Object} PivotWiderState
   * @property {string[]} names_from Columns whose values become new column names
   * @property {string[]} values_from Columns whose values fill the new columns
   * @property {string[]} id_cols ID columns (optional; R excludes names/values_from)
   * @property {string | null} values_fill Fill value for missing cells (null = none)
   * @property {string} names_sep Separator joining names_from values (default "_")
   * @property {string} names_prefix Prefix for new column names ("" = none)
   * @property {string | null} values_fn Aggregation function for duplicates (null = none)
   */

  class PivotWiderBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {string[]} */
      this.names_from = [];
      /** @type {string[]} */
      this.values_from = [];
      /** @type {string[]} */
      this.id_cols = [];
      this.values_fill = '';
      this.names_sep = '_';
      this.names_prefix = '';
      this.values_fn = '';
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
      this._namesFromSelect = null;
      /** @type {BlockrSelectMultiHandle | null} */
      this._valuesFromSelect = null;
      /** @type {BlockrSelectMultiHandle | null} */
      this._idColsSelect = null;
      this._popoverOpen = false;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(/** @type {ReturnType<typeof setTimeout>} */ (this._debounceTimer));
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
      namesFromLabel.textContent = 'Names from';
      namesFromWrap.appendChild(namesFromLabel);
      this._namesFromSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(namesFromWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select columns\u2026',
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
      valuesFromLabel.textContent = 'Values from';
      valuesFromWrap.appendChild(valuesFromLabel);
      this._valuesFromSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(valuesFromWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select columns\u2026',
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
      idColsLabel.textContent = 'ID columns (optional)';
      idColsWrap.appendChild(idColsLabel);
      this._idColsSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(idColsWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select columns\u2026',
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
        this.values_fill = /** @type {HTMLInputElement} */ (this._fillInput).value;
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
        this.names_sep = /** @type {HTMLInputElement} */ (this._sepInput).value;
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
        this.names_prefix = /** @type {HTMLInputElement} */ (this._prefixInput).value;
        this._autoSubmit();
      });
      prefixRow.appendChild(this._prefixInput);
      this.popoverEl.appendChild(prefixRow);

      // values_fn (aggregation function for duplicates)
      const fnRow = document.createElement('div');
      fnRow.className = 'blockr-popover-row';
      const fnLabel = document.createElement('label');
      fnLabel.className = 'blockr-popover-label';
      fnLabel.textContent = 'Aggregation:';
      fnRow.appendChild(fnLabel);
      const fnWrap = document.createElement('div');
      fnWrap.className = 'blockr-popover-select-wrap';
      fnRow.appendChild(fnWrap);
      const fnOptions = ['', 'mean', 'median', 'sum', 'min', 'max', 'first', 'last', 'n_distinct'];
      this._valuesFnSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(fnWrap, {
        options: fnOptions,
        selected: this.values_fn || '',
        placeholder: '(none)',
        onChange: (value) => {
          this.values_fn = value;
          this._autoSubmit();
        }
      });
      this.popoverEl.appendChild(fnRow);

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

    /** @returns {PivotWiderState} */
    _compose() {
      return {
        names_from: this.names_from.slice(),
        values_from: this.values_from.slice(),
        id_cols: this.id_cols.slice(),
        values_fill: this.values_fill || null,
        names_sep: this.names_sep,
        names_prefix: this.names_prefix,
        values_fn: this.values_fn || null
      };
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    /** @returns {PivotWiderState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {Partial<PivotWiderState> | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      this.names_from = (state?.names_from || []).slice();
      this.values_from = (state?.values_from || []).slice();
      this.id_cols = (state?.id_cols || []).slice();
      this.values_fill = state?.values_fill || '';
      this.names_sep = state?.names_sep ?? '_';
      this.names_prefix = state?.names_prefix || '';
      this.values_fn = state?.values_fn || '';

      // Update popover text inputs
      /** @type {HTMLInputElement} */ (this._fillInput).value = this.values_fill;
      /** @type {HTMLInputElement} */ (this._sepInput).value = this.names_sep;
      /** @type {HTMLInputElement} */ (this._prefixInput).value = this.names_prefix;
      if (this._valuesFnSelect) {
        const fnOptions = ['', 'mean', 'median', 'sum', 'min', 'max', 'first', 'last', 'n_distinct'];
        this._valuesFnSelect.setOptions(fnOptions, this.values_fn);
      }

      // Update multi-selects
      if (this._namesFromSelect) {
        this._namesFromSelect.setOptions(this.columnOptions, this.names_from);
      }
      if (this._valuesFromSelect) {
        this._valuesFromSelect.setOptions(this.columnOptions, this.values_from);
      }
      if (this._idColsSelect) {
        this._idColsSelect.setOptions(this.columnOptions, this.id_cols);
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
      if (this._namesFromSelect) {
        this._namesFromSelect.setOptions(this.columnOptions, this.names_from);
        this.names_from = this._namesFromSelect.getValue();
      }
      if (this._valuesFromSelect) {
        this._valuesFromSelect.setOptions(this.columnOptions, this.values_from);
        this.values_from = this._valuesFromSelect.getValue();
      }
      if (this._idColsSelect) {
        this._idColsSelect.setOptions(this.columnOptions, this.id_cols);
        this.id_cols = this._idColsSelect.getValue();
      }
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'pivot-wider',
    Block: PivotWiderBlock,
    messages: {
      'pivot-wider-columns': (block, msg) => block.updateColumns(msg.columns),
      'pivot-wider-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
