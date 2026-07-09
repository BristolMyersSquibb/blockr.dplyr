// @ts-check
/**
 * UniteBlock — JS-driven tidyr::unite block input binding.
 *
 * Main UI: new column name text input, columns multi-select (bordered),
 * separator text input, remove/na_rm checkboxes.
 * Selects and checkboxes submit immediately; text inputs commit on
 * Enter/blur (§5.5 chip). The columns picker carries the amber
 * required-empty cue while nothing is selected.
 *
 * Depends on: blockr-core.js, blockr-select.js, settings-band.js
 */
(() => {
  'use strict';

  /**
   * Block state as exchanged with R — mirrors the parameters of
   * make_unite_expr() in R/expr-builders.R (what _compose() sends
   * and setState() receives).
   * @typedef {Object} UniteState
   * @property {string} col Name of the new united column (default "united")
   * @property {string[]} cols Columns to unite, in order
   * @property {string} sep Separator between values (default "_")
   * @property {boolean} remove Remove the source columns after uniting
   * @property {boolean} na_rm Remove NA values before pasting (R: na.rm)
   */

  class UniteBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      this.col = 'united';
      /** @type {string[]} */
      this.cols = [];
      this.sep = '_';
      this.remove = true;
      this.na_rm = false;
      /** @type {string[]} */
      this.columnNames = [];
      /** @type {BlockrSelectOption[]} */
      this.columnOptions = [];
      /** @type {Record<string, BlockrPickerColumn>} */
      this.columnMeta = {};
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {BlockrSelectMultiHandle | null} */
      this._multiSelect = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._removeBox = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._naRmBox = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._colCommit = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._sepCommit = null;

      this._buildDOM();
      this._updateRequired();
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'ub-card';
      this.el.appendChild(this.card);

      // Top row: new column name + separator
      const topRow = document.createElement('div');
      topRow.className = 'ub-input-row';

      // col (new column name) — commits on Enter/blur (§5.5 chip)
      const colWrap = document.createElement('div');
      colWrap.className = 'ub-field';
      const colLabel = document.createElement('label');
      colLabel.className = 'blockr-label';
      colLabel.textContent = 'New column name';
      colWrap.appendChild(colLabel);
      const colField = document.createElement('div');
      colField.className = 'blockr-commit-field';
      this._colInput = document.createElement('input');
      this._colInput.type = 'text';
      this._colInput.className = 'blockr-text-input ub-text-input';
      this._colInput.value = this.col;
      this._colInput.placeholder = 'united';
      colField.appendChild(this._colInput);
      this._colCommit = Blockr.textCommit(this._colInput, {
        onCommit: (value) => {
          this.col = value;
          this._submit();
        }
      });
      colWrap.appendChild(colField);
      topRow.appendChild(colWrap);

      // sep — commits on Enter/blur
      const sepWrap = document.createElement('div');
      sepWrap.className = 'ub-field ub-field-narrow';
      const sepLabel = document.createElement('label');
      sepLabel.className = 'blockr-label';
      sepLabel.textContent = 'Separator';
      sepWrap.appendChild(sepLabel);
      const sepField = document.createElement('div');
      sepField.className = 'blockr-commit-field';
      this._sepInput = document.createElement('input');
      this._sepInput.type = 'text';
      this._sepInput.className = 'blockr-text-input ub-text-input';
      this._sepInput.value = this.sep;
      this._sepInput.placeholder = '_';
      sepField.appendChild(this._sepInput);
      this._sepCommit = Blockr.textCommit(this._sepInput, {
        onCommit: (value) => {
          this.sep = value;
          this._submit();
        }
      });
      sepWrap.appendChild(sepField);
      topRow.appendChild(sepWrap);

      this.card.appendChild(topRow);

      // Column picker (bordered) — required: unite with no columns is an
      // identity transform, so it carries the amber cue while empty.
      const pickerWrap = document.createElement('div');
      pickerWrap.className = 'ub-picker-wrap blockr-select--bordered';
      const pickerLabel = document.createElement('label');
      pickerLabel.className = 'blockr-label';
      pickerLabel.textContent = 'Columns to unite';
      pickerWrap.appendChild(pickerLabel);
      this.card.appendChild(pickerWrap);
      this._pickerWrap = pickerWrap;

      this._multiSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(pickerWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select columns…',
        reorderable: true,
        onChange: (selected) => {
          this.cols = selected;
          this._updateRequired();
          this._submit();
        }
      });

      // Option bar: remove + na_rm checkboxes (boolean data options)
      const optionBar = document.createElement('div');
      optionBar.className = 'blockr-checkbox-row ub-options';

      this._removeBox = Blockr.checkbox('Remove source columns', this.remove, (checked) => {
        this.remove = checked;
        this._submit();
      });
      this._removeBox.input.title =
        'Remove the source columns after uniting';
      optionBar.appendChild(this._removeBox.el);

      this._naRmBox = Blockr.checkbox('Drop NA values', this.na_rm, (checked) => {
        this.na_rm = checked;
        this._submit();
      });
      this._naRmBox.input.title =
        'Remove NA values before pasting columns together';
      optionBar.appendChild(this._naRmBox.el);

      this.card.appendChild(optionBar);
    }

    _updateRequired() {
      Blockr.setRequiredEmpty(
        /** @type {HTMLDivElement} */ (this._pickerWrap), this.cols.length === 0);
    }

    /** @returns {UniteState} */
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

    /** @returns {UniteState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {Partial<UniteState> | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      this.col = state?.col || 'united';
      this.cols = (state?.cols || []).slice();
      this.sep = state?.sep ?? '_';
      this.remove = state?.remove !== false;
      this.na_rm = !!state?.na_rm;

      // Update text inputs (sync resets the chips)
      /** @type {BlockrTextCommitHandle} */ (this._colCommit).sync(this.col);
      /** @type {BlockrTextCommitHandle} */ (this._sepCommit).sync(this.sep);

      // Update checkboxes
      /** @type {BlockrCheckboxHandle} */ (this._removeBox).set(this.remove);
      /** @type {BlockrCheckboxHandle} */ (this._naRmBox).set(this.na_rm);

      // Update multi-select
      if (this._multiSelect) {
        this._multiSelect.setOptions(this.columnOptions, this.cols);
      }
      this._updateRequired();
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
      this._updateRequired();
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'unite',
    Block: UniteBlock,
    messages: {
      'unite-columns': (block, msg) => block.updateColumns(msg.columns),
      'unite-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
