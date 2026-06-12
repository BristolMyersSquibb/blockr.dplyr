// @ts-check
/**
 * FilterBlock — JS-driven filter block input binding.
 *
 * Auto-detects column types: multi-select for categorical, comparison for numeric.
 * Value/range changes auto-submit (300ms debounce). Expression mode has explicit confirm.
 *
 * Depends on: blockr-core.js, blockr-select.js, blockr-input.js
 */
(() => {
  'use strict';

  // Expression categories for autocomplete
  const defaultCategories = {
    arithmetic: ['abs', 'sign', 'ceiling', 'floor', 'round', 'trunc', 'log', 'log2', 'log10', 'exp', 'sqrt'],
    aggregate: ['mean', 'sum', 'min', 'max'],
    offset: ['lead', 'lag', 'cumsum', 'cumprod', 'cummin', 'cummax'],
    logical: ['if_else', 'case_when'],
    string: ['str_c', 'paste', 'paste0', 'str_sub', 'str_to_lower', 'str_to_upper'],
    ranking: ['row_number', 'min_rank', 'dense_rank', 'percent_rank', 'ntile']
  };

  // Operator definitions
  const CAT_OPS = [
    { value: 'is', label: 'is' },
    { value: 'is not', label: 'is not' }
  ];
  const NUM_OPS = [
    { value: 'is', label: 'is' },
    { value: 'is not', label: 'is not' },
    { value: '>', label: '>' },
    { value: '>=', label: '\u2265' },
    { value: '<', label: '<' },
    { value: '<=', label: '\u2264' }
  ];

  /**
   * One entry of the cycling operator button's list (CAT_OPS / NUM_OPS).
   * @typedef {{ value: string, label: string }} FilterOp
   */

  /**
   * Restore-time options for a value/numeric row: a saved
   * BlockrFilterCondition remapped onto row fields. `values` may arrive as
   * a bare string (R -> JSON auto-unboxing); _onColumnChange normalizes.
   * @typedef {Object} FilterRowOpts
   * @property {string} [op]
   * @property {string | string[] | null} [values]
   * @property {number | null} [numValue]
   * @property {BlockrColumnType | null} [colType]
   */

  /**
   * Internal per-condition row record (UI state; composed into a
   * BlockrFilterCondition by _compose()).
   * @typedef {Object} FilterCondRow
   * @property {number} id
   * @property {'none' | 'values' | 'numeric' | 'expr'} filterType
   * @property {string | null} column
   * @property {string} [op]
   * @property {string[] | null} values
   * @property {number | null} numValue
   * @property {BlockrColumnType | null} [_savedColType]
   * @property {null} multiSelect
   * @property {BlockrInputHandle | null} exprInput
   * @property {HTMLDivElement | null} rowEl
   * @property {BlockrSelectSingleHandle} [_colSelectize]
   * @property {BlockrSelectMultiHandle | null} [_valueSelectize]
   * @property {HTMLSpanElement} [_opBtnSlot]
   * @property {HTMLDivElement} [_contentDiv]
   * @property {BlockrColumnValues} [_meta]
   * @property {HTMLButtonElement} [_opBtn]
   * @property {FilterOp[]} [_opList]
   */

  class FilterBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {FilterCondRow[]} */
      this.conditions = [];
      /** @type {'&' | '|'} */
      this.operator = '&';
      this.nextId = 1;
      /** @type {Record<string, BlockrColumnValues>} */
      this.columnMeta = {};
      /** @type {string[]} */
      this.columnNames = [];
      /** @type {Array<{value: string, label: string}>} */
      this.columnOptions = [];
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {ReturnType<typeof setTimeout> | null} */
      this._debounceTimer = null;
      this.preserveOrder = false;
      /** @type {Set<string>} */
      this._pendingValueRequests = new Set();

      this._buildDOM();
      this._addValueRow(null, null);
    }

    _autoSubmit() {
      clearTimeout(/** @type {ReturnType<typeof setTimeout>} */ (this._debounceTimer));
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'fb-card';
      this.el.appendChild(this.card);

      this.listEl = document.createElement('div');
      this.listEl.className = 'fb-conditions';
      this.card.appendChild(this.listEl);

      // Add row bar
      const addRow = document.createElement('div');
      addRow.className = 'blockr-add-row';

      const addCondLink = document.createElement('span');
      addCondLink.className = 'blockr-add-link';
      addCondLink.innerHTML = `<span class="blockr-add-icon">${Blockr.icons.plus}</span> Add condition`;
      addCondLink.addEventListener('click', () => this._addValueRow(null, null));
      addRow.appendChild(addCondLink);

      const addExprLink = document.createElement('span');
      addExprLink.className = 'blockr-add-link-expr';
      addExprLink.innerHTML = Blockr.icons.code;
      addExprLink.title = 'Add R expression';
      addExprLink.addEventListener('click', () => this._addExprRow(''));
      addRow.appendChild(addExprLink);

      this.opToggle = document.createElement('button');
      this.opToggle.type = 'button';
      this.opToggle.className = 'blockr-pill fb-op-toggle';
      this.opToggle.textContent = 'AND';
      this.opToggle.title = 'Toggle between AND (all conditions must match) and OR (any condition can match)';
      this.opToggle.style.visibility = 'hidden';
      this.opToggle.addEventListener('click', () => {
        this.operator = this.operator === '&' ? '|' : '&';
        /** @type {HTMLButtonElement} */ (this.opToggle).textContent = this.operator === '&' ? 'AND' : 'OR';
        this._autoSubmit();
      });
      addRow.appendChild(this.opToggle);

      const spacer = document.createElement('span');
      spacer.style.flex = '1';
      addRow.appendChild(spacer);

      this.preserveBtn = document.createElement('button');
      this.preserveBtn.type = 'button';
      this.preserveBtn.className = 'blockr-pill fb-preserve-btn';
      this.preserveBtn.textContent = 'data order';
      this.preserveBtn.title = 'Toggle between original data order and the order you picked values';
      this.preserveBtn.addEventListener('click', () => {
        this.preserveOrder = !this.preserveOrder;
        /** @type {HTMLButtonElement} */ (this.preserveBtn).textContent = this.preserveOrder ? 'pick order' : 'data order';
        /** @type {HTMLButtonElement} */ (this.preserveBtn).classList.toggle('fb-preserve-active', this.preserveOrder);
        // Re-render value selects so reorderable state updates
        for (const c of this.conditions) {
          if (c.filterType === 'values' || c.filterType === 'numeric') {
            this._renderDynamicContent(c);
          }
        }
        this._autoSubmit();
      });
      addRow.appendChild(this.preserveBtn);

      this.card.appendChild(addRow);
    }

    /**
     * @param {FilterCondRow} cond
     * @param {FilterOp[]} ops
     * @param {string | null} initialOp
     * @returns {HTMLButtonElement}
     */
    _createOpButton(cond, ops, initialOp) {
      // Fall back to cond.op so the column-selectize's onChange path
      // (which calls _onColumnChange WITHOUT opts) doesn't clobber an
      // initial op set by setState (e.g. mode='exclude' → 'is not').
      // updateColumns triggers setOptions → onChange before its own
      // explicit _onColumnChange(c, col, { op: c.op }) runs, so we'd
      // otherwise lose the saved op on every column-meta refresh.
      const fallbackOp = initialOp || cond.op || null;
      let idx = fallbackOp
        ? Math.max(0, ops.findIndex(o => o.value === fallbackOp))
        : 0;
      cond.op = ops[idx].value;

      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'blockr-pill fb-op-btn';
      btn.textContent = ops[idx].label;
      btn.title = 'Cycle through comparison operators (is, is not, >, \u2265, <, \u2264)';
      btn.addEventListener('click', () => {
        idx = (idx + 1) % ops.length;
        cond.op = ops[idx].value;
        btn.textContent = ops[idx].label;
        this._renderDynamicContent(cond);
        this._autoSubmit();
      });

      cond._opBtn = btn;
      cond._opList = ops;
      return btn;
    }

    /**
     * @param {string | null | undefined} column
     * @param {FilterRowOpts | null | undefined} opts
     */
    _addValueRow(column, opts) {
      const id = this.nextId++;
      // Persist opts.op onto cond.op so it survives until column metadata
      // arrives. updateColumns later replays _onColumnChange with
      // { op: c.op } — if cond.op is left at the default 'is' here, an
      // initial state of mode='exclude' (op='is not') gets clobbered the
      // moment columns load. Same for numeric ops ('>', '<=', etc.).
      /** @type {FilterCondRow} */
      const cond = {
        id,
        filterType: 'none',
        column: column || '',
        op: opts?.op || 'is',
        values: /** @type {string[]} */ (opts?.values || []),
        numValue: opts?.numValue ?? null,
        // Saved colType from a restored state — used by _compose until live
        // column metadata arrives and takes precedence.
        _savedColType: opts?.colType || null,
        multiSelect: null,
        exprInput: null,
        rowEl: null
      };

      const row = document.createElement('div');
      row.className = 'blockr-row';
      row.setAttribute('data-cond-id', /** @type {any} */ (id));
      cond.rowEl = row;

      // Column dropdown
      const colDiv = document.createElement('div');
      colDiv.className = 'fb-col-wrap';
      row.appendChild(colDiv);
      cond._colSelectize = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(colDiv, {
        options: this.columnOptions,
        selected: /** @type {string | undefined} */ (column),
        placeholder: 'Column\u2026',
        onChange: (value) => this._onColumnChange(cond, value)
      });

      // Operator button slot
      cond._opBtnSlot = document.createElement('span');
      row.appendChild(cond._opBtnSlot);

      // Dynamic content area
      const contentDiv = document.createElement('div');
      contentDiv.className = 'blockr-row-content';
      row.appendChild(contentDiv);
      cond._contentDiv = contentDiv;

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => {
        this._removeCondition(id);
        this._autoSubmit();
      });
      row.appendChild(rmBtn);

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(row);
      this.conditions.push(cond);
      this._updateUI();

      const activeCol = column || cond._colSelectize.getValue();
      if (activeCol && this.columnMeta[activeCol]) {
        this._onColumnChange(cond, activeCol, opts);
      }
    }

    /**
     * @param {FilterCondRow} cond
     * @param {string} colName
     * @param {FilterRowOpts | null} [opts]
     */
    _onColumnChange(cond, colName, opts) {
      cond.column = colName;
      const meta = this.columnMeta[colName];

      // Restore values/numValue from opts if provided (setState path),
      // otherwise reset to empty (user interaction path).
      // Coerce scalar → [scalar]: R → JSON auto-unboxing can flatten a
      // length-1 character vector to a string, which would iterate per-char.
      const rawVals = opts?.values;
      cond.values = Array.isArray(rawVals) ? rawVals : (rawVals == null ? [] : [rawVals]);
      cond.numValue = opts?.numValue ?? null;

      if (cond._valueSelectize) { cond._valueSelectize.destroy(); cond._valueSelectize = null; }
      /** @type {HTMLDivElement} */ (cond._contentDiv).innerHTML = '';
      /** @type {HTMLSpanElement} */ (cond._opBtnSlot).innerHTML = '';

      if (!meta) { cond.filterType = 'none'; return; }

      const isNumeric = meta.type === 'numeric' || meta.type === 'integer';
      cond._meta = meta;
      cond.filterType = isNumeric ? 'numeric' : 'values';

      // Map condition op to operator list value for initial selection
      const ops = isNumeric ? NUM_OPS : CAT_OPS;
      const initialOp = opts?.op || null;
      const btn = this._createOpButton(cond, ops, initialOp);
      /** @type {HTMLSpanElement} */ (cond._opBtnSlot).appendChild(btn);

      this._renderDynamicContent(cond);
    }

    /** @param {FilterCondRow} cond */
    _renderDynamicContent(cond) {
      const container = /** @type {HTMLDivElement} */ (cond._contentDiv);
      const meta = cond._meta;

      if (cond._valueSelectize) { cond._valueSelectize.destroy(); cond._valueSelectize = null; }
      container.innerHTML = '';

      if (!meta) return;

      const op = cond.op;

      if (op === 'is' || op === 'is not') {
        const isNumeric = meta.type === 'numeric' || meta.type === 'integer';
        const hasValues = isNumeric
          ? meta.uniqueValues !== undefined
          : meta.values !== undefined;

        // Values load lazily, on first dropdown-open — never at render time.
        // A preconfigured board must not pay for a high-cardinality column's
        // unique values (50K ids ~ 289KB) that nobody may ever look at; saved
        // chips render from cond.values without the option list.
        cond._valueSelectize = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(container, {
          options: this._buildValueOptions(meta),
          selected: cond.values || [],
          placeholder: 'Select values\u2026',
          reorderable: this.preserveOrder,
          loading: !hasValues,
          onOpen: () => this._requestColumnValues(/** @type {string} */ (cond.column)),
          onChange: (selected) => {
            cond.values = selected;
            this._autoSubmit();
          }
        });
      } else {
        const numInput = document.createElement('input');
        numInput.type = 'number';
        numInput.className = 'blockr-num-input';
        numInput.step = 'any';
        numInput.placeholder = 'Enter value\u2026';
        if (cond.numValue != null) numInput.value = /** @type {any} */ (cond.numValue);
        numInput.addEventListener('input', () => {
          cond.numValue = numInput.value === '' ? null : parseFloat(numInput.value);
          this._autoSubmit();
        });
        container.appendChild(numInput);
      }
    }

    /**
     * @param {BlockrColumnValues} meta
     * @returns {string[]}
     */
    _buildValueOptions(meta) {
      const isNumeric = meta.type === 'numeric' || meta.type === 'integer';
      /** @type {string[]} */
      let allValues;
      if (isNumeric) {
        allValues = (meta.uniqueValues || []).map(String);
      } else {
        allValues = (meta.values || []).slice();
        if (meta.hasEmpty) allValues.push('<empty>');
      }
      if (meta.hasNA) allValues.push('<NA>');
      return allValues;
    }

    /** @param {string} value */
    _addExprRow(value) {
      const id = this.nextId++;

      const row = document.createElement('div');
      row.className = 'blockr-row fb-row-expr';
      row.setAttribute('data-cond-id', /** @type {any} */ (id));

      const codeDiv = document.createElement('div');
      codeDiv.className = 'blockr-row-content fb-expr-code';
      row.appendChild(codeDiv);

      const confirmBtn = document.createElement('button');
      confirmBtn.className = 'blockr-expr-confirm';
      confirmBtn.type = 'button';
      confirmBtn.innerHTML = 'Enter \u21B5';
      confirmBtn.title = 'Apply expression';

      const doConfirm = () => {
        confirmBtn.classList.add('confirmed');
        confirmBtn.innerHTML = Blockr.icons.confirm;
        this._submit();
      };
      confirmBtn.addEventListener('click', doConfirm);

      const exprInput = /** @type {BlockrInputStatic} */ (Blockr.Input).create(codeDiv, {
        value,
        columns: this.columnNames,
        categories: defaultCategories,
        placeholder: 'R expression\u2026',
        onChange: () => {
          confirmBtn.classList.remove('confirmed');
          confirmBtn.innerHTML = 'Enter \u21B5';
        },
        onConfirm: () => doConfirm()
      });
      row.appendChild(confirmBtn);

      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => this._removeCondition(id));
      row.appendChild(rmBtn);

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(row);
      this.conditions.push({
        id,
        filterType: 'expr',
        column: null,
        values: null,
        numValue: null,
        multiSelect: null,
        exprInput,
        rowEl: row
      });
      this._updateUI();
    }

    /** @param {number} id */
    _removeCondition(id) {
      if (this.conditions.length <= 1) return;

      const idx = this.conditions.findIndex(c => c.id === id);
      if (idx < 0) return;

      const cond = this.conditions[idx];
      cond._valueSelectize?.destroy();
      cond._colSelectize?.destroy();
      cond.exprInput?.destroy();
      cond.rowEl?.parentNode?.removeChild(cond.rowEl);
      this.conditions.splice(idx, 1);
      this._updateUI();
    }

    _updateUI() {
      const single = this.conditions.length <= 1;
      /** @type {HTMLButtonElement} */ (this.opToggle).style.visibility = single ? 'hidden' : 'visible';
      for (const c of this.conditions) {
        const btn = /** @type {HTMLElement | null | undefined} */ (c.rowEl?.querySelector('.blockr-row-remove'));
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
    }

    /** @returns {BlockrFilterState} */
    _compose() {
      /** @type {BlockrFilterCondition[]} */
      const conditions = [];
      for (const c of this.conditions) {
        if (!c.column && c.filterType !== 'expr') continue;

        const op = c.op;
        if ((c.filterType === 'values' || c.filterType === 'numeric') && (op === 'is' || op === 'is not')) {
          if (/** @type {string[]} */ (c.values)?.length > 0) {
            conditions.push({
              type: 'values', column: /** @type {string} */ (c.column), values: /** @type {string[]} */ (c.values),
              mode: op === 'is' ? 'include' : 'exclude',
              // Column type tag: lets R convert the string values back to
              // the column's type instead of guessing by coercibility
              // ("007" on a character column must stay "007", not become 7).
              colType: this.columnMeta[/** @type {string} */ (c.column)]?.type || c._savedColType || null
            });
          }
        } else if (c.filterType === 'numeric' && c.numValue != null) {
          conditions.push({ type: 'numeric', column: /** @type {string} */ (c.column), op, value: c.numValue });
        } else if (c.filterType === 'expr' && c.exprInput) {
          const val = c.exprInput.getValue();
          if (val) conditions.push({ type: 'expr', expr: val });
        }
      }
      return { conditions, operator: this.operator, preserveOrder: this.preserveOrder };
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
     * @param {BlockrFilterState | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      // Clear existing conditions
      while (this.conditions.length > 0) {
        const cond = this.conditions[0];
        cond._valueSelectize?.destroy();
        cond._colSelectize?.destroy();
        cond.exprInput?.destroy();
        cond.rowEl?.parentNode?.removeChild(cond.rowEl);
        this.conditions.splice(0, 1);
      }

      // Set operator
      this.operator = state?.operator || '&';
      /** @type {HTMLButtonElement} */ (this.opToggle).textContent = this.operator === '&' ? 'AND' : 'OR';

      // Rebuild from state
      const conditions = state?.conditions || [];
      if (conditions.length === 0) {
        this._addValueRow(null, null);
      } else {
        for (const cond of conditions) {
          if (cond.type === 'expr') {
            this._addExprRow(cond.expr || '');
          } else if (cond.type === 'values') {
            this._addValueRow(cond.column, {
              values: cond.values || [],
              op: cond.mode === 'exclude' ? 'is not' : 'is',
              colType: cond.colType || null
            });
          } else if (cond.type === 'numeric') {
            this._addValueRow(cond.column, {
              numValue: cond.value,
              op: cond.op || '>'
            });
          }
        }
      }
      // Mark expression confirm buttons as confirmed (state is already applied)
      for (const c of this.conditions) {
        const btn = c.rowEl?.querySelector('.blockr-expr-confirm');
        if (btn) {
          btn.classList.add('confirmed');
          btn.innerHTML = Blockr.icons.confirm;
        }
      }
      this._updateUI();
    }

    /** @param {BlockrColumnSummary[] | null | undefined} meta */
    updateColumns(meta) {
      this.columnMeta = {};
      this.columnNames = [];
      this.columnOptions = [];
      this._pendingValueRequests = new Set();
      for (const col of (meta || [])) {
        this.columnMeta[col.name] = col;
        this.columnNames.push(col.name);
        this.columnOptions.push({ value: col.name, label: col.label || '' });
      }
      for (const c of this.conditions) {
        if (c._colSelectize) {
          const current = c._colSelectize.getValue();
          c._colSelectize.setOptions(this.columnOptions, current);
          const col = c._colSelectize.getValue();
          if (col && this.columnMeta[col]) {
            // Preserve existing values/op: column metadata refreshing must
            // not clobber user- or setState-provided condition values.
            this._onColumnChange(c, col, {
              values: c.values,
              numValue: c.numValue,
              op: c.op
            });
          }
        }
        if (c.exprInput) {
          c.exprInput.setColumns(this.columnNames);
        }
      }
    }

    /** @param {string} colName */
    _requestColumnValues(colName) {
      const meta = this.columnMeta[colName];
      const loaded = meta &&
        (meta.values !== undefined || meta.uniqueValues !== undefined);
      if (loaded) return;
      if (this._pendingValueRequests.has(colName)) return;
      this._pendingValueRequests.add(colName);
      Shiny.setInputValue(
        this.el.id + '_request_values',
        colName,
        { priority: 'event' }
      );
    }

    /** @param {BlockrColumnValues} colMeta */
    receiveColumnValues(colMeta) {
      this._pendingValueRequests.delete(colMeta.name);
      // Merge full metadata into existing summary
      const existing = this.columnMeta[colMeta.name];
      if (existing) {
        Object.assign(existing, colMeta);
      } else {
        this.columnMeta[colMeta.name] = colMeta;
      }
      // Refresh conditions waiting for this column's values. In place when
      // the select exists: a re-render would destroy it and snap the dropdown
      // the user just opened shut.
      for (const c of this.conditions) {
        if (c.column === colMeta.name && c._meta) {
          c._meta = this.columnMeta[colMeta.name];
          if (c._valueSelectize) {
            c._valueSelectize.updateOptions(this._buildValueOptions(c._meta));
            c._valueSelectize.setLoading(false);
          } else {
            this._renderDynamicContent(c);
          }
        }
      }
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'filter',
    Block: FilterBlock,
    messages: {
      'filter-columns': (block, msg) => block.updateColumns(msg.columns),
      'filter-column-values': (block, msg) => block.receiveColumnValues(msg.column),
      'filter-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
