// @ts-check
/**
 * SelectBlock — JS-driven column select block input binding.
 *
 * Multi-select column picker with reorderable tags, plus exclude and
 * distinct checkboxes (design-system rule: data options are checkboxes).
 * Submits immediately on any change.
 *
 * Depends on: blockr-core.js, blockr-select.js, settings-band.js
 */
(() => {
  'use strict';

  /**
   * Select-block state, mirroring make_select_expr() in R/expr-builders.R.
   * @typedef {Object} SelectBlockState
   * @property {string[]} [columns]
   * @property {boolean} [exclude]
   * @property {boolean} [distinct]
   */

  class SelectBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {string[]} */
      this.columns = [];
      this.exclude = false;
      this.distinct = false;
      /** @type {string[]} */
      this.columnNames = [];
      /** @type {Array<{value: string, label: string}>} */
      this.columnOptions = [];
      /** @type {Record<string, BlockrColumnSummary>} */
      this.columnMeta = {};
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {BlockrSelectMultiHandle | null} */
      this._multiSelect = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._excludeBox = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._distinctBox = null;

      this._buildDOM();
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'sb-card';
      this.el.appendChild(this.card);

      // Column picker
      const pickerWrap = document.createElement('div');
      pickerWrap.className = 'sb-picker-wrap';
      this.card.appendChild(pickerWrap);

      this._multiSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(pickerWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select columns\u2026',
        reorderable: true,
        onChange: (selected) => {
          this.columns = selected;
          this._submit();
        }
      });
      this._multiSelect.el.classList.add('blockr-select--bordered');

      // Option bar: exclude + distinct checkboxes (boolean data options)
      const optionBar = document.createElement('div');
      optionBar.className = 'blockr-checkbox-row sb-options';

      this._excludeBox = Blockr.checkbox('Exclude selected', this.exclude, (checked) => {
        this.exclude = checked;
        this._submit();
      });
      optionBar.appendChild(this._excludeBox.el);

      this._distinctBox = Blockr.checkbox('Distinct rows', this.distinct, (checked) => {
        this.distinct = checked;
        this._submit();
      });
      this._distinctBox.input.title =
        'Deduplicate rows based on the selected columns';
      optionBar.appendChild(this._distinctBox.el);

      this.card.appendChild(optionBar);
    }

    /** @returns {SelectBlockState} */
    _compose() {
      return {
        columns: this.columns.slice(),
        exclude: this.exclude,
        distinct: this.distinct
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

    /**
     * @param {SelectBlockState | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      this.columns = (state?.columns || []).slice();
      this.exclude = !!state?.exclude;
      this.distinct = !!state?.distinct;

      /** @type {BlockrCheckboxHandle} */ (this._excludeBox).set(this.exclude);
      /** @type {BlockrCheckboxHandle} */ (this._distinctBox).set(this.distinct);

      if (this._multiSelect) {
        this._multiSelect.setOptions(this.columnOptions, this.columns);
      }
    }

    /** @param {BlockrColumnSummary[] | null | undefined} meta */
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
        this._multiSelect.setOptions(this.columnOptions, this.columns);
        this.columns = this._multiSelect.getValue();
      }
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'select',
    Block: SelectBlock,
    messages: {
      'select-columns': (block, msg) => block.updateColumns(msg.columns),
      'select-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
