// @ts-check
/**
 * PivotLongerBlock — JS-driven pivot_longer block input binding.
 *
 * Main UI: column multi-select (bordered), names_to text input, values_to text input.
 * Settings band (in-flow, gear-toggled): values_drop_na checkbox,
 *   names_prefix text input.
 * Selects submit immediately; text inputs commit on Enter/blur (§5.5 chip).
 *
 * Depends on: blockr-core.js, blockr-select.js, settings-band.js
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
      /** @type {BlockrSelectMultiHandle | null} */
      this._multiSelect = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._dropNaBox = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._namesToCommit = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._valuesToCommit = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._prefixCommit = null;
      this._bandOpen = false;

      this._buildDOM();
      this._updateRequired();
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
      this.gearBtn.addEventListener('click', () => this._toggleBand());
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // Settings band — in flow between the gear header and the content
      // (a panel, not a menu: the gear is the only toggle).
      this._buildBand();

      // Column picker (bordered) — required: the block is an identity
      // transform until columns are chosen, so it carries the amber cue.
      const pickerWrap = document.createElement('div');
      pickerWrap.className = 'plb-picker-wrap blockr-select--bordered';
      const pickerLabel = document.createElement('label');
      pickerLabel.className = 'blockr-label';
      pickerLabel.textContent = 'Columns';
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

      // Text inputs row: names_to + values_to
      const inputRow = document.createElement('div');
      inputRow.className = 'plb-input-row';

      // names_to — commits on Enter/blur with the "Enter ↵" chip (§5.5)
      const namesToWrap = document.createElement('div');
      namesToWrap.className = 'plb-field';
      const namesToLabel = document.createElement('label');
      namesToLabel.className = 'blockr-label';
      namesToLabel.textContent = 'Names to';
      namesToWrap.appendChild(namesToLabel);
      const namesToField = document.createElement('div');
      namesToField.className = 'blockr-commit-field';
      this._namesToInput = document.createElement('input');
      this._namesToInput.type = 'text';
      this._namesToInput.className = 'blockr-text-input plb-text-input';
      this._namesToInput.value = this.names_to;
      this._namesToInput.placeholder = 'name';
      namesToField.appendChild(this._namesToInput);
      this._namesToCommit = Blockr.textCommit(this._namesToInput, {
        onCommit: (value) => {
          this.names_to = value;
          this._submit();
        }
      });
      namesToWrap.appendChild(namesToField);
      inputRow.appendChild(namesToWrap);

      // values_to
      const valuesToWrap = document.createElement('div');
      valuesToWrap.className = 'plb-field';
      const valuesToLabel = document.createElement('label');
      valuesToLabel.className = 'blockr-label';
      valuesToLabel.textContent = 'Values to';
      valuesToWrap.appendChild(valuesToLabel);
      const valuesToField = document.createElement('div');
      valuesToField.className = 'blockr-commit-field';
      this._valuesToInput = document.createElement('input');
      this._valuesToInput.type = 'text';
      this._valuesToInput.className = 'blockr-text-input plb-text-input';
      this._valuesToInput.value = this.values_to;
      this._valuesToInput.placeholder = 'value';
      valuesToField.appendChild(this._valuesToInput);
      this._valuesToCommit = Blockr.textCommit(this._valuesToInput, {
        onCommit: (value) => {
          this.values_to = value;
          this._submit();
        }
      });
      valuesToWrap.appendChild(valuesToField);
      inputRow.appendChild(valuesToWrap);

      this.card.appendChild(inputRow);
    }

    // --- Settings band ---

    _buildBand() {
      this.bandEl = document.createElement('div');
      this.bandEl.className = 'blockr-settings blockr-settings--beak';

      const title = document.createElement('div');
      title.className = 'blockr-settings__title';
      title.textContent = 'Advanced settings';
      this.bandEl.appendChild(title);

      const grid = document.createElement('div');
      grid.className = 'blockr-settings__grid';
      this.bandEl.appendChild(grid);

      // values_drop_na — boolean data option -> checkbox
      const dropNaField = document.createElement('div');
      dropNaField.className = 'blockr-settings__field';
      this._dropNaBox = Blockr.checkbox('Drop NA values', this.values_drop_na, (checked) => {
        this.values_drop_na = checked;
        this._submit();
      });
      this._dropNaBox.input.title =
        'Drop rows whose value is NA from the result';
      dropNaField.appendChild(this._dropNaBox.el);
      grid.appendChild(dropNaField);

      // names_prefix — commits on Enter/blur (§5.5 chip)
      const prefixField = document.createElement('div');
      prefixField.className = 'blockr-settings__field';
      const prefixLabel = document.createElement('label');
      prefixLabel.className = 'blockr-label';
      prefixLabel.textContent = 'Prefix';
      prefixField.appendChild(prefixLabel);
      const prefixWrap = document.createElement('div');
      prefixWrap.className = 'blockr-commit-field';
      this._prefixInput = document.createElement('input');
      this._prefixInput.type = 'text';
      this._prefixInput.className = 'blockr-text-input';
      this._prefixInput.value = this.names_prefix;
      this._prefixInput.placeholder = '(optional)';
      prefixWrap.appendChild(this._prefixInput);
      this._prefixCommit = Blockr.textCommit(this._prefixInput, {
        onCommit: (value) => {
          this.names_prefix = value;
          this._submit();
        }
      });
      prefixField.appendChild(prefixWrap);
      grid.appendChild(prefixField);

      /** @type {HTMLDivElement} */ (this.card).appendChild(this.bandEl);
    }

    _toggleBand() {
      this._bandOpen = !this._bandOpen;
      /** @type {HTMLDivElement} */ (this.bandEl).classList.toggle('blockr-settings--open', this._bandOpen);
      /** @type {HTMLButtonElement} */ (this.gearBtn).classList.toggle('blockr-gear-active', this._bandOpen);
    }

    _updateRequired() {
      Blockr.setRequiredEmpty(
        /** @type {HTMLDivElement} */ (this._pickerWrap),
        this.cols.length === 0
      );
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

      // Update text inputs (sync resets the chip to its hidden state)
      /** @type {BlockrTextCommitHandle} */ (this._namesToCommit).sync(this.names_to);
      /** @type {BlockrTextCommitHandle} */ (this._valuesToCommit).sync(this.values_to);

      // Update band controls
      /** @type {BlockrTextCommitHandle} */ (this._prefixCommit).sync(this.names_prefix);
      /** @type {BlockrCheckboxHandle} */ (this._dropNaBox).set(this.values_drop_na);

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
    name: 'pivot-longer',
    Block: PivotLongerBlock,
    messages: {
      'pivot-longer-columns': (block, msg) => block.updateColumns(msg.columns),
      'pivot-longer-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
