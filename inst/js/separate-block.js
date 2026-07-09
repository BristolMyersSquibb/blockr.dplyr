// @ts-check
/**
 * SeparateBlock — JS-driven tidyr::separate block input binding.
 *
 * Main UI: source column single-select (bordered), into text input
 * (comma-separated names), separator text input, remove/convert checkboxes.
 * Selects and checkboxes submit immediately; text inputs commit on
 * Enter/blur (§5.5 chip). Required fields (column, into) carry the amber
 * required-empty cue.
 *
 * Depends on: blockr-core.js, blockr-select.js, settings-band.js
 */
(() => {
  'use strict';

  /**
   * Block state as exchanged with R — mirrors the parameters of
   * make_separate_expr() in R/expr-builders.R (what _compose() sends
   * and setState() receives). The R builder also accepts extra/fill
   * ("warn" defaults), which this UI does not expose.
   * @typedef {Object} SeparateState
   * @property {string} col Source column to split
   * @property {string[]} into New column names
   * @property {string} sep Separator regex (default "[^[:alnum:]]+")
   * @property {boolean} remove Remove the source column after splitting
   * @property {boolean} convert Auto-convert split values (numbers/logicals)
   */

  class SeparateBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      this.col = '';
      /** @type {string[]} */
      this.into = [];
      this.sep = '[^[:alnum:]]+';
      this.remove = true;
      this.convert = false;
      /** @type {string[]} */
      this.columnNames = [];
      /** @type {BlockrSelectOption[]} */
      this.columnOptions = [];
      /** @type {Record<string, BlockrPickerColumn>} */
      this.columnMeta = {};
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {BlockrSelectSingleHandle | null} */
      this._colSelect = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._removeBox = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._convertBox = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._intoCommit = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._sepCommit = null;

      this._buildDOM();
      this._updateRequired();
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'spb-card';
      this.el.appendChild(this.card);

      // Source column picker (bordered) — required
      const colWrap = document.createElement('div');
      colWrap.className = 'spb-col-wrap blockr-select--bordered';
      const colLabel = document.createElement('label');
      colLabel.className = 'blockr-label';
      colLabel.textContent = 'Column';
      colWrap.appendChild(colLabel);
      this._colSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(colWrap, {
        options: this.columnOptions,
        selected: /** @type {any} */ (null),
        placeholder: 'Select column…',
        onChange: (value) => {
          this.col = value;
          this._updateRequired();
          this._submit();
        }
      });
      this.card.appendChild(colWrap);
      this._colWrap = colWrap;

      // Text inputs row: into + sep
      const inputRow = document.createElement('div');
      inputRow.className = 'spb-input-row';

      // into (comma-separated names) — required; commits on Enter/blur
      const intoWrap = document.createElement('div');
      intoWrap.className = 'spb-field';
      const intoLabel = document.createElement('label');
      intoLabel.className = 'blockr-label';
      intoLabel.textContent = 'Into (comma-separated)';
      intoWrap.appendChild(intoLabel);
      const intoField = document.createElement('div');
      intoField.className = 'blockr-commit-field';
      this._intoInput = document.createElement('input');
      this._intoInput.type = 'text';
      this._intoInput.className = 'blockr-text-input spb-text-input';
      this._intoInput.value = '';
      this._intoInput.placeholder = 'col1, col2, col3';
      intoField.appendChild(this._intoInput);
      this._intoCommit = Blockr.textCommit(this._intoInput, {
        onCommit: (value) => {
          this.into = this._parseInto(value);
          this._updateRequired();
          this._submit();
        }
      });
      intoWrap.appendChild(intoField);
      inputRow.appendChild(intoWrap);
      this._intoWrap = intoWrap;

      // sep — commits on Enter/blur
      const sepWrap = document.createElement('div');
      sepWrap.className = 'spb-field spb-field-narrow';
      const sepLabel = document.createElement('label');
      sepLabel.className = 'blockr-label';
      sepLabel.textContent = 'Separator';
      sepWrap.appendChild(sepLabel);
      const sepField = document.createElement('div');
      sepField.className = 'blockr-commit-field';
      this._sepInput = document.createElement('input');
      this._sepInput.type = 'text';
      this._sepInput.className = 'blockr-text-input spb-text-input';
      this._sepInput.value = this.sep;
      this._sepInput.placeholder = '[^[:alnum:]]+';
      sepField.appendChild(this._sepInput);
      this._sepCommit = Blockr.textCommit(this._sepInput, {
        onCommit: (value) => {
          this.sep = value;
          this._submit();
        }
      });
      sepWrap.appendChild(sepField);
      inputRow.appendChild(sepWrap);

      this.card.appendChild(inputRow);

      // Option bar: remove + convert checkboxes (boolean data options)
      const optionBar = document.createElement('div');
      optionBar.className = 'blockr-checkbox-row spb-options';

      this._removeBox = Blockr.checkbox('Remove source column', this.remove, (checked) => {
        this.remove = checked;
        this._submit();
      });
      this._removeBox.input.title =
        'Remove the source column after splitting';
      optionBar.appendChild(this._removeBox.el);

      this._convertBox = Blockr.checkbox('Auto-convert types', this.convert, (checked) => {
        this.convert = checked;
        this._submit();
      });
      this._convertBox.input.title =
        'Auto-convert split values to numbers or logicals';
      optionBar.appendChild(this._convertBox.el);

      this.card.appendChild(optionBar);
    }

    _updateRequired() {
      Blockr.setRequiredEmpty(
        /** @type {HTMLDivElement} */ (this._colWrap), !this.col);
      Blockr.setRequiredEmpty(
        /** @type {HTMLDivElement} */ (this._intoWrap), this.into.length === 0);
    }

    /**
     * @param {string} text
     * @returns {string[]}
     */
    _parseInto(text) {
      return text.split(',')
        .map(s => s.trim())
        .filter(s => s.length > 0);
    }

    /** @returns {SeparateState} */
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

    /** @returns {SeparateState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {Partial<SeparateState> | null | undefined} state
     * @param {boolean} [silent]
     */
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

      // Update text inputs (sync resets the chips)
      /** @type {BlockrTextCommitHandle} */ (this._intoCommit).sync(this.into.join(', '));
      /** @type {BlockrTextCommitHandle} */ (this._sepCommit).sync(this.sep);

      // Update checkboxes
      /** @type {BlockrCheckboxHandle} */ (this._removeBox).set(this.remove);
      /** @type {BlockrCheckboxHandle} */ (this._convertBox).set(this.convert);

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
      if (this._colSelect) {
        const current = this._colSelect.getValue();
        this._colSelect.setOptions(this.columnOptions, current);
        this.col = this._colSelect.getValue();
      }
      this._updateRequired();
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'separate',
    Block: SeparateBlock,
    messages: {
      'separate-columns': (block, msg) => block.updateColumns(msg.columns),
      'separate-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
