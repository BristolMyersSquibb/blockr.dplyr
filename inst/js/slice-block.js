// @ts-check
/**
 * SliceBlock — JS-driven dplyr::slice_* block input binding.
 *
 * Main UI: [type selector (bordered)] [n input] [gear button]
 * Settings band (in-flow, gear-toggled): order_by (min/max), weight_by
 *   (sample), with_ties (min/max), replace (sample) — conditionally visible
 *   based on type; on/off options are Blockr.checkbox.
 * Below card: "Group by:" label + bordered multi-select (summarize pattern).
 *
 * Depends on: blockr-core.js, blockr-select.js, settings-band.js
 */
(() => {
  'use strict';

  const SLICE_TYPES = ['head', 'tail', 'min', 'max', 'sample', 'custom'];

  /**
   * Block state as exchanged with R — mirrors the parameters of
   * make_slice_expr() in R/expr-builders.R (what _compose() sends and
   * setState() receives).
   * @typedef {Object} SliceState
   * @property {string} type One of 'head' | 'tail' | 'min' | 'max' | 'sample' | 'custom'
   * @property {number | null} n Number of rows (null when prop is used)
   * @property {number | null} prop Proportion 0..1 (null when n is used)
   * @property {string} order_by Column for min/max ('' = none)
   * @property {boolean} with_ties Keep tied values (min/max)
   * @property {string} weight_by Column for weighted sampling ('' = none)
   * @property {boolean} replace Sample with replacement
   * @property {string} rows Row positions expression for type 'custom' (e.g. "1:5")
   * @property {string[] | string} by Grouping columns (jsonlite may send a lone string)
   */

  class SliceBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      this.type = 'head';
      /** @type {number | null} */
      this.n = 5;
      /** @type {number | null} */
      this.prop = null;
      this.order_by = '';
      this.with_ties = true;
      this.weight_by = '';
      this.replace = false;
      this.rows = '1:5';
      /** @type {string[]} */
      this.by = [];
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
      this._typeSelect = null;
      /** @type {BlockrSelectSingleHandle | null} */
      this._orderBySelect = null;
      /** @type {BlockrSelectSingleHandle | null} */
      this._weightBySelect = null;
      /** @type {BlockrSelectMultiHandle | null} */
      this._bySelect = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._withTiesBox = null;
      /** @type {BlockrCheckboxHandle | null} */
      this._replaceBox = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._nCommit = null;
      /** @type {BlockrTextCommitHandle | null} */
      this._rowsCommit = null;
      this._bandOpen = false;

      this._buildDOM();
      this._updateBandVisibility();
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'slb-card';
      this.el.appendChild(this.card);

      // Gear header (top-right)
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = Blockr.icons.gear;
      this.gearBtn.title = 'Options';
      this.gearBtn.addEventListener('click', () => this._toggleBand());
      gearHeader.appendChild(this.gearBtn);
      this._gearHeader = gearHeader;
      this.card.appendChild(gearHeader);

      // Settings band — in flow between the gear header and the main row:
      // opening pushes the content down; the gear is the only toggle
      // (a panel, not a menu — no outside-click dismissal).
      this._buildBand();

      // Top row: type + n in a standard blockr-row
      const topRow = document.createElement('div');
      topRow.className = 'blockr-row';
      this._topRow = topRow;

      // Type selector
      const typeWrap = document.createElement('div');
      typeWrap.className = 'slb-type-wrap';
      this._typeSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(typeWrap, {
        options: SLICE_TYPES,
        selected: this.type,
        placeholder: 'Type…',
        onChange: (value) => {
          this.type = value;
          this._updateBandVisibility();
          this._submit();
        }
      });
      // No --bordered class needed: the row provides the border
      topRow.appendChild(typeWrap);

      // n input — commits on Enter/blur with the "Enter ↵" chip (§5.5)
      const nWrap = document.createElement('div');
      nWrap.className = 'slb-n-wrap';
      this._nInput = document.createElement('input');
      this._nInput.type = 'number';
      this._nInput.className = 'blockr-num-input';
      this._nInput.value = /** @type {string} */ (/** @type {*} */ (this.n));
      this._nInput.min = '1';
      this._nInput.placeholder = 'n';
      nWrap.appendChild(this._nInput);
      this._nCommit = Blockr.textCommit(this._nInput, {
        onCommit: () => {
          this._updateNValue();
          this._submit();
        }
      });
      topRow.appendChild(nWrap);

      // n/% toggle pill
      this._usesProp = false;
      this._nModePill = document.createElement('button');
      this._nModePill.type = 'button';
      this._nModePill.className = 'blockr-pill slb-mode-pill';
      this._nModePill.textContent = 'n';
      this._nModePill.title = 'Toggle between selecting a fixed number of rows (n) or a percentage (%)';
      this._nModePill.addEventListener('click', () => {
        this._usesProp = !this._usesProp;
        /** @type {HTMLButtonElement} */ (this._nModePill).textContent = this._usesProp ? '%' : 'n';
        /** @type {HTMLButtonElement} */ (this._nModePill).classList.toggle('slb-mode-active', this._usesProp);
        if (this._usesProp) {
          /** @type {HTMLInputElement} */ (this._nInput).min = '0';
          /** @type {HTMLInputElement} */ (this._nInput).max = '100';
          /** @type {HTMLInputElement} */ (this._nInput).step = '1';
          /** @type {HTMLInputElement} */ (this._nInput).placeholder = '%';
          /** @type {BlockrTextCommitHandle} */ (this._nCommit).sync('10');
        } else {
          /** @type {HTMLInputElement} */ (this._nInput).min = '1';
          /** @type {HTMLInputElement} */ (this._nInput).removeAttribute('max');
          /** @type {HTMLInputElement} */ (this._nInput).step = '1';
          /** @type {HTMLInputElement} */ (this._nInput).placeholder = 'n';
          /** @type {BlockrTextCommitHandle} */ (this._nCommit).sync('5');
        }
        this._updateNValue();
        this._submit();
      });
      topRow.appendChild(this._nModePill);

      // Rows text input (custom positions — hidden by default)
      this._rowsWrap = document.createElement('div');
      this._rowsWrap.className = 'slb-rows-wrap';
      this._rowsWrap.style.display = 'none';
      this._rowsInput = document.createElement('input');
      this._rowsInput.type = 'text';
      this._rowsInput.className = 'blockr-num-input';
      this._rowsInput.value = this.rows;
      this._rowsInput.placeholder = '1:5, c(1,3,5), -c(2,4)';
      this._rowsWrap.appendChild(this._rowsInput);
      this._rowsCommit = Blockr.textCommit(this._rowsInput, {
        onCommit: (value) => {
          this.rows = value;
          this._updateRequired();
          this._submit();
        }
      });
      topRow.appendChild(this._rowsWrap);

      this.card.appendChild(topRow);

      // Group by section (below the card, summarize pattern)
      this.bySection = document.createElement('div');
      this.bySection.className = 'slb-by-section';

      const byLabel = document.createElement('span');
      byLabel.className = 'blockr-label';
      byLabel.textContent = 'Group by:';
      this.bySection.appendChild(byLabel);

      const byWrap = document.createElement('div');
      byWrap.className = 'slb-by-wrap';
      this.bySection.appendChild(byWrap);

      this._bySelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(byWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'None',
        reorderable: false,
        onChange: (selected) => {
          this.by = selected;
          this._submit();
        }
      });
      this._bySelect.el.classList.add('blockr-select--bordered');

      this.el.appendChild(this.bySection);
    }

    // --- Settings band ---

    _buildBand() {
      this.bandEl = document.createElement('div');
      this.bandEl.className = 'blockr-settings blockr-settings--beak';

      const title = document.createElement('div');
      title.className = 'blockr-settings__title';
      title.textContent = 'Options';
      this.bandEl.appendChild(title);

      const grid = document.createElement('div');
      grid.className = 'blockr-settings__grid';
      this.bandEl.appendChild(grid);

      // order_by select (min/max) — required for slice_min/slice_max, so it
      // carries the amber required-empty cue while unset.
      this._orderByField = document.createElement('div');
      this._orderByField.className = 'blockr-settings__field';
      const orderByLabel = document.createElement('label');
      orderByLabel.className = 'blockr-label';
      orderByLabel.textContent = 'Order by';
      this._orderByField.appendChild(orderByLabel);
      const orderBySelectWrap = document.createElement('div');
      this._orderByField.appendChild(orderBySelectWrap);
      this._orderBySelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(orderBySelectWrap, {
        options: this.columnOptions,
        selected: /** @type {string | undefined} */ (/** @type {*} */ (null)),
        // Without this the control falls back to the first column and displays
        // it while `order_by` is still '' — the block looks configured and is
        // not (ux-principles, "the model and the control never disagree").
        allowEmpty: true,
        placeholder: 'Select column…',
        onChange: (value) => {
          this.order_by = value;
          this._updateRequired();
          this._submit();
        }
      });
      this._orderBySelect.el.classList.add('blockr-select--bordered');
      grid.appendChild(this._orderByField);

      // weight_by select (sample)
      this._weightByField = document.createElement('div');
      this._weightByField.className = 'blockr-settings__field';
      const weightByLabel = document.createElement('label');
      weightByLabel.className = 'blockr-label';
      weightByLabel.textContent = 'Weight by';
      this._weightByField.appendChild(weightByLabel);
      const weightBySelectWrap = document.createElement('div');
      this._weightByField.appendChild(weightBySelectWrap);
      this._weightBySelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(weightBySelectWrap, {
        options: this.columnOptions,
        selected: /** @type {string | undefined} */ (/** @type {*} */ (null)),
        allowEmpty: true,
        // Optional: empty is a configuration, so the placeholder names it.
        placeholder: 'Unweighted',
        onChange: (value) => {
          this.weight_by = value;
          this._submit();
        }
      });
      this._weightBySelect.el.classList.add('blockr-select--bordered');
      grid.appendChild(this._weightByField);

      // with_ties (min/max) — boolean data option -> checkbox
      this._withTiesField = document.createElement('div');
      this._withTiesField.className = 'blockr-settings__field';
      this._withTiesBox = Blockr.checkbox('Keep ties', this.with_ties, (checked) => {
        this.with_ties = checked;
        this._submit();
      });
      this._withTiesBox.input.title =
        'Whether rows with equal values are all included or cut off at n';
      this._withTiesField.appendChild(this._withTiesBox.el);
      grid.appendChild(this._withTiesField);

      // replace (sample) — boolean data option -> checkbox
      this._replaceField = document.createElement('div');
      this._replaceField.className = 'blockr-settings__field';
      this._replaceBox = Blockr.checkbox('Sample with replacement', this.replace, (checked) => {
        this.replace = checked;
        this._submit();
      });
      this._replaceBox.input.title =
        'Whether sampled rows can be picked more than once';
      this._replaceField.appendChild(this._replaceBox.el);
      grid.appendChild(this._replaceField);

      /** @type {HTMLDivElement} */ (this.card).appendChild(this.bandEl);
    }

    _toggleBand() {
      this._bandOpen = !this._bandOpen;
      /** @type {HTMLDivElement} */ (this.bandEl).classList.toggle('blockr-settings--open', this._bandOpen);
      /** @type {HTMLButtonElement} */ (this.gearBtn).classList.toggle('blockr-gear-active', this._bandOpen);
    }

    _closeBand() {
      if (this._bandOpen) this._toggleBand();
    }

    _updateBandVisibility() {
      const isMinMax = this.type === 'min' || this.type === 'max';
      const isSample = this.type === 'sample';
      const isCustom = this.type === 'custom';
      const hasOptions = isMinMax || isSample;

      // Hide gear entirely when no advanced options apply
      if (this._gearHeader) {
        this._gearHeader.style.display = hasOptions ? '' : 'none';
      }
      // Close band if we hid the gear
      if (!hasOptions) this._closeBand();

      // Toggle n input vs rows input
      /** @type {HTMLElement} */ (/** @type {HTMLInputElement} */ (this._nInput).parentElement).style.display = isCustom ? 'none' : '';
      /** @type {HTMLButtonElement} */ (this._nModePill).style.display = isCustom ? 'none' : '';
      /** @type {HTMLDivElement} */ (this._rowsWrap).style.display = isCustom ? '' : 'none';

      /** @type {HTMLDivElement} */ (this._orderByField).style.display = isMinMax ? '' : 'none';
      /** @type {HTMLDivElement} */ (this._weightByField).style.display = isSample ? '' : 'none';
      /** @type {HTMLDivElement} */ (this._withTiesField).style.display = isMinMax ? '' : 'none';
      /** @type {HTMLDivElement} */ (this._replaceField).style.display = isSample ? '' : 'none';

      this._updateRequired();
    }

    _updateRequired() {
      const isMinMax = this.type === 'min' || this.type === 'max';
      const isCustom = this.type === 'custom';
      // slice_min/slice_max without order_by is a runtime error — amber the
      // band field while unset; likewise custom positions with no expression.
      Blockr.setRequiredEmpty(
        /** @type {HTMLDivElement} */ (this._orderByField),
        isMinMax && !this.order_by
      );
      Blockr.setRequiredEmpty(
        /** @type {HTMLDivElement} */ (this._topRow),
        isCustom && !this.rows.trim()
      );
    }

    _updateNValue() {
      const raw = parseFloat(/** @type {HTMLInputElement} */ (this._nInput).value);
      if (this._usesProp) {
        this.prop = isNaN(raw) ? null : raw / 100;
        this.n = null;
      } else {
        this.n = isNaN(raw) ? 5 : Math.round(raw);
        this.prop = null;
      }
    }

    /** @returns {SliceState} */
    _compose() {
      return {
        type: this.type,
        n: this.n,
        prop: this.prop,
        order_by: this.order_by,
        with_ties: this.with_ties,
        weight_by: this.weight_by,
        replace: this.replace,
        rows: this.rows,
        by: this.by.slice()
      };
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    /** @returns {SliceState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {SliceState | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      this.type = state?.type || 'head';
      this.n = state?.n ?? 5;
      this.prop = state?.prop ?? null;
      this.order_by = state?.order_by || '';
      this.with_ties = state?.with_ties !== false;
      this.weight_by = state?.weight_by || '';
      this.replace = !!state?.replace;
      this.rows = state?.rows || '1:5';
      const byRaw = state?.by || [];
      this.by = Array.isArray(byRaw) ? byRaw.slice() : [byRaw];

      // Update type select
      if (this._typeSelect) {
        this._typeSelect.setOptions(SLICE_TYPES, this.type);
      }

      // Update n/prop mode
      this._usesProp = this.prop != null && this.prop > 0;
      /** @type {HTMLButtonElement} */ (this._nModePill).textContent = this._usesProp ? '%' : 'n';
      /** @type {HTMLButtonElement} */ (this._nModePill).classList.toggle('slb-mode-active', this._usesProp);
      if (this._usesProp) {
        /** @type {BlockrTextCommitHandle} */ (this._nCommit).sync(
          String(Math.round(/** @type {number} */ (this.prop) * 100)));
        /** @type {HTMLInputElement} */ (this._nInput).placeholder = '%';
      } else {
        /** @type {BlockrTextCommitHandle} */ (this._nCommit).sync(String(this.n ?? 5));
        /** @type {HTMLInputElement} */ (this._nInput).placeholder = 'n';
      }

      // Update rows input
      if (this._rowsCommit) {
        this._rowsCommit.sync(this.rows);
      }

      // Update column selects
      if (this._orderBySelect) {
        this._orderBySelect.setOptions(this.columnOptions, this.order_by || null);
      }
      if (this._weightBySelect) {
        this._weightBySelect.setOptions(this.columnOptions, this.weight_by || null);
      }
      if (this._bySelect) {
        this._bySelect.setOptions(this.columnOptions, this.by);
      }

      // Update checkboxes
      /** @type {BlockrCheckboxHandle} */ (this._withTiesBox).set(this.with_ties);
      /** @type {BlockrCheckboxHandle} */ (this._replaceBox).set(this.replace);

      this._updateBandVisibility();
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
      // Refresh options but keep the model state authoritative: a single
      // selectize picks a default when passed null, which would silently
      // commit a column the user never chose (breaks save/restore).
      if (this._orderBySelect) {
        this._orderBySelect.setOptions(this.columnOptions, this.order_by || null);
        if (this.order_by && !this.columnNames.includes(this.order_by)) {
          this.order_by = "";
        }
      }
      if (this._weightBySelect) {
        this._weightBySelect.setOptions(this.columnOptions, this.weight_by || null);
        if (this.weight_by && !this.columnNames.includes(this.weight_by)) {
          this.weight_by = "";
        }
      }
      if (this._bySelect) {
        this._bySelect.setOptions(this.columnOptions, this.by);
        this.by = (this.by || []).filter(c => this.columnNames.includes(c));
      }
      this._updateRequired();
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'slice',
    Block: SliceBlock,
    messages: {
      'slice-columns': (block, msg) => block.updateColumns(msg.columns),
      'slice-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
