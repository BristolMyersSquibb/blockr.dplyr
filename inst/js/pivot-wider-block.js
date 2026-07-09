// @ts-check
/**
 * PivotWiderBlock — JS-driven pivot_wider block input binding.
 *
 * Main UI: names_from multi-select (bordered), values_from multi-select (bordered),
 * id_cols multi-select (bordered).
 * Settings band (in-flow, gear-toggled): values_fill, names_sep, names_prefix
 *   text inputs + values_fn select.
 * Selects submit immediately; text inputs commit on Enter/blur (§5.5 chip).
 *
 * Depends on: blockr-core.js, blockr-select.js, settings-band.js
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
      /** @type {BlockrSelectMultiHandle | null} */
      this._namesFromSelect = null;
      /** @type {BlockrSelectMultiHandle | null} */
      this._valuesFromSelect = null;
      /** @type {BlockrSelectMultiHandle | null} */
      this._idColsSelect = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._fillCommit = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._sepCommit = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._prefixCommit = null;
      this._bandOpen = false;

      this._buildDOM();
      this._updateRequired();
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
      this.gearBtn.addEventListener('click', () => this._toggleBand());
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // Settings band — in flow between the gear header and the content
      // (a panel, not a menu: the gear is the only toggle).
      this._buildBand();

      // names_from picker (bordered) — required: pivot_wider without it is
      // an identity transform, so it carries the amber cue while empty.
      const namesFromWrap = document.createElement('div');
      namesFromWrap.className = 'pwb-picker-wrap blockr-select--bordered';
      const namesFromLabel = document.createElement('label');
      namesFromLabel.className = 'blockr-label';
      namesFromLabel.textContent = 'Names from';
      namesFromWrap.appendChild(namesFromLabel);
      this._namesFromSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(namesFromWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select columns…',
        reorderable: false,
        onChange: (selected) => {
          this.names_from = selected;
          this._updateRequired();
          this._submit();
        }
      });
      this.card.appendChild(namesFromWrap);
      this._namesFromWrap = namesFromWrap;

      // values_from picker (bordered) — required, same reasoning
      const valuesFromWrap = document.createElement('div');
      valuesFromWrap.className = 'pwb-picker-wrap blockr-select--bordered';
      const valuesFromLabel = document.createElement('label');
      valuesFromLabel.className = 'blockr-label';
      valuesFromLabel.textContent = 'Values from';
      valuesFromWrap.appendChild(valuesFromLabel);
      this._valuesFromSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(valuesFromWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select columns…',
        reorderable: false,
        onChange: (selected) => {
          this.values_from = selected;
          this._updateRequired();
          this._submit();
        }
      });
      this.card.appendChild(valuesFromWrap);
      this._valuesFromWrap = valuesFromWrap;

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
        placeholder: 'Select columns…',
        reorderable: false,
        onChange: (selected) => {
          this.id_cols = selected;
          this._submit();
        }
      });
      this.card.appendChild(idColsWrap);
    }

    // --- Settings band ---

    /**
     * Build one band text field with a commit chip.
     * @param {HTMLElement} grid @param {string} label @param {string} value
     * @param {string} placeholder @param {(value: string) => void} onCommit
     * @returns {{ input: HTMLInputElement, commit: BlockrTextCommitHandle }}
     */
    _bandTextField(grid, label, value, placeholder, onCommit) {
      const field = document.createElement('div');
      field.className = 'blockr-settings__field';
      const labelEl = document.createElement('label');
      labelEl.className = 'blockr-label';
      labelEl.textContent = label;
      field.appendChild(labelEl);
      const wrap = document.createElement('div');
      wrap.className = 'blockr-commit-field';
      const input = document.createElement('input');
      input.type = 'text';
      input.className = 'blockr-text-input';
      input.value = value;
      input.placeholder = placeholder;
      wrap.appendChild(input);
      const commit = Blockr.textCommit(input, { onCommit });
      field.appendChild(wrap);
      grid.appendChild(field);
      return { input, commit };
    }

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

      // values_fill / names_sep / names_prefix — commit on Enter/blur (§5.5)
      const fill = this._bandTextField(grid, 'Fill', this.values_fill, '(optional)',
        (value) => { this.values_fill = value; this._submit(); });
      this._fillInput = fill.input;
      this._fillCommit = fill.commit;

      const sep = this._bandTextField(grid, 'Separator', this.names_sep, '_',
        (value) => { this.names_sep = value; this._submit(); });
      this._sepInput = sep.input;
      this._sepCommit = sep.commit;

      const prefix = this._bandTextField(grid, 'Prefix', this.names_prefix, '(optional)',
        (value) => { this.names_prefix = value; this._submit(); });
      this._prefixInput = prefix.input;
      this._prefixCommit = prefix.commit;

      // values_fn (aggregation function for duplicates)
      const fnField = document.createElement('div');
      fnField.className = 'blockr-settings__field';
      const fnLabel = document.createElement('label');
      fnLabel.className = 'blockr-label';
      fnLabel.textContent = 'Aggregation';
      fnField.appendChild(fnLabel);
      const fnWrap = document.createElement('div');
      fnField.appendChild(fnWrap);
      const fnOptions = ['', 'mean', 'median', 'sum', 'min', 'max', 'first', 'last', 'n_distinct'];
      this._valuesFnSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(fnWrap, {
        options: fnOptions,
        selected: this.values_fn || '',
        placeholder: '(none)',
        onChange: (value) => {
          this.values_fn = value;
          this._submit();
        }
      });
      this._valuesFnSelect.el.classList.add('blockr-select--bordered');
      grid.appendChild(fnField);

      /** @type {HTMLDivElement} */ (this.card).appendChild(this.bandEl);
    }

    _toggleBand() {
      this._bandOpen = !this._bandOpen;
      /** @type {HTMLDivElement} */ (this.bandEl).classList.toggle('blockr-settings--open', this._bandOpen);
      /** @type {HTMLButtonElement} */ (this.gearBtn).classList.toggle('blockr-gear-active', this._bandOpen);
    }

    _updateRequired() {
      Blockr.setRequiredEmpty(
        /** @type {HTMLDivElement} */ (this._namesFromWrap),
        this.names_from.length === 0
      );
      Blockr.setRequiredEmpty(
        /** @type {HTMLDivElement} */ (this._valuesFromWrap),
        this.values_from.length === 0
      );
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

      // Update band text inputs (sync resets the chips)
      /** @type {BlockrTextCommitHandle} */ (this._fillCommit).sync(this.values_fill);
      /** @type {BlockrTextCommitHandle} */ (this._sepCommit).sync(this.names_sep);
      /** @type {BlockrTextCommitHandle} */ (this._prefixCommit).sync(this.names_prefix);
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
      this._updateRequired();
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
