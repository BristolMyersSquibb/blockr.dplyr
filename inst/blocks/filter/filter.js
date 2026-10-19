// @ts-check
/**
 * FilterBlock — filter block.
 *
 * Auto-detects column types: multi-select for categorical, comparison for numeric.
 * Discrete controls submit immediately; numeric values and expression mode
 * commit on Enter/blur (§5.5 chip). "Keep pick order" is a checkbox.
 *
 * Depends on: blockr-block.js, blockr-select.js, blockr-input.js
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
   * `colPinned` marks a column the board restored or the user picked, as
   * opposed to one the select auto-picked (option 0) on its own — only a
   * pinned column survives a column list that does not offer it (see
   * updateColumns()).
   * @typedef {Object} FilterCondRow
   * @property {number} id
   * @property {'none' | 'values' | 'numeric' | 'expr'} filterType
   * @property {string | null} column
   * @property {boolean} colPinned
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
    /**
     * @param {HTMLElement} el
     * @param {BlockrHost<BlockrFilterState>} host
     */
    constructor(el, host) {
      this.el = el;
      this.host = host;
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
      this.preserveOrder = false;
      /** @type {BlockrCheckboxHandle | null} */
      this._preserveBox = null;
      /** @type {Set<string>} */
      this._pendingValueRequests = new Set();

      this._buildDOM();
      this._addValueRow(null, null);
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
      addExprLink.setAttribute('aria-label', 'Add R expression');
      Blockr.tooltip.set(addExprLink, 'Add R expression');
      addExprLink.addEventListener('click', () => this._addExprRow(''));
      addRow.appendChild(addExprLink);

      // All or any: a fixed choice of two, so a segmented control with both
      // values in view (design system), in place of the AND/OR cycle pill.
      this._opSeg = Blockr.segmented(
        [
          { value: '&', label: 'All', title: 'Keep rows that match all conditions' },
          { value: '|', label: 'Any', title: 'Keep rows that match any condition' }
        ],
        this.operator,
        (value) => {
          this.operator = /** @type {any} */ (value);
          this._submit();
        },
        { size: 'xs', label: 'Keep rows that match' }
      );
      this.opToggle = this._opSeg.el;
      this.opToggle.classList.add('fb-op-toggle');
      this.opToggle.style.visibility = 'hidden';
      addRow.appendChild(this.opToggle);

      const spacer = document.createElement('span');
      spacer.style.flex = '1';
      addRow.appendChild(spacer);

      // Boolean data option -> checkbox (design-system rule)
      this._preserveBox = Blockr.checkbox('Keep pick order', this.preserveOrder, (checked) => {
        this.preserveOrder = checked;
        // Re-render value selects so reorderable state updates
        for (const c of this.conditions) {
          if (c.filterType === 'values' || c.filterType === 'numeric') {
            this._renderDynamicContent(c);
          }
        }
        this._submit();
      });
      addRow.appendChild(this._preserveBox.el);

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

      // A pill that opens a menu listing every operator (design system:
      // "a pill that opens a menu"), in place of the click-to-cycle pill.
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'blockr-pill blockr-pill--menu fb-op-btn';
      btn.setAttribute('aria-haspopup', 'listbox');
      const setLabel = () => {
        btn.innerHTML = '';
        btn.appendChild(document.createTextNode(ops[idx].label));
        const car = document.createElement('span');
        car.className = 'blockr-pill__caret';
        car.innerHTML = Blockr.icons.chevron;
        btn.appendChild(car);
      };
      setLabel();
      // The pill shows the operator; the tooltip and screen readers name it.
      btn.setAttribute('aria-description', 'Operator');
      Blockr.tooltip.set(btn, 'Operator');
      btn.addEventListener('click', () => {
        /** @type {BlockrSelectStatic} */ (Blockr.Select).menu(btn, {
          title: 'Operator',
          options: ops.map(o => o.label),
          selected: ops[idx].label,
          search: false,
          onChange: (label) => {
            const next = ops.findIndex(o => o.label === label);
            if (next < 0 || next === idx) return;
            idx = next;
            cond.op = ops[idx].value;
            setLabel();
            this._renderDynamicContent(cond);
            this._submit();
          }
        });
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
      // Coerce scalar → [scalar] here as well as in _onColumnChange: R → JSON
      // auto-unboxing flattens a length-1 character vector to a string, and a
      // restored row is composed from these fields before _onColumnChange has
      // ever run (see filterType below).
      const rawVals = opts?.values;
      const values = Array.isArray(rawVals)
        ? rawVals.slice()
        : (rawVals == null || rawVals === '' ? [] : [rawVals]);

      /** @type {FilterCondRow} */
      const cond = {
        id,
        // A restored row knows its own kind from its saved state; only a fresh
        // row has to wait for column metadata to find out. Leaving this at
        // 'none' until metadata arrives is what used to make _compose() DROP a
        // restored condition whose column the current frame does not carry (an
        // upstream still settling) — the next submit then wrote an empty
        // filter over the board's. _onColumnChange overwrites it, from the
        // column's real type, the moment that metadata lands.
        filterType: values.length ? 'values' : (opts?.numValue != null ? 'numeric' : 'none'),
        column: column || '',
        // A column handed to us came from the board or the user; one the
        // select picks on its own does not.
        colPinned: !!column,
        op: opts?.op || 'is',
        values,
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
        onChange: (value) => {
          cond.colPinned = true;
          this._onColumnChange(cond, value);
          this._syncColWidth();
        }
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
        this._submit();
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
          // Only fires while the component is in server-search mode
          // (setSearchInfo with truncated=true): high-cardinality columns
          // whose value list the server capped.
          onSearch: (query) => this._searchColumnValues(/** @type {string} */ (cond.column), query),
          onChange: (selected) => {
            cond.values = selected;
            this._submit();
          }
        });
        cond._valueSelectize.setSearchInfo(meta);
      } else {
        const numInput = document.createElement('input');
        numInput.type = 'number';
        numInput.className = 'blockr-num-input';
        numInput.step = 'any';
        numInput.placeholder = 'Enter value\u2026';
        if (cond.numValue != null) numInput.value = /** @type {any} */ (cond.numValue);
        container.appendChild(numInput);
        // Commits on Enter/blur with the "Enter \u21b5" chip (\u00a75.5)
        Blockr.textCommit(numInput, {
          onCommit: (value) => {
            cond.numValue = value === '' ? null : parseFloat(value);
            this._submit();
          }
        });
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
      confirmBtn.className = 'blockr-expr-confirm blockr-expr-confirm--key';
      confirmBtn.type = 'button';
      confirmBtn.textContent = '\u21B5';
      // The key is the whole action, so the button shows it and has no
      // tooltip (as blockr.ui's Blockr.textCommit); screen readers get its name.
      confirmBtn.setAttribute('aria-label', 'Apply (Enter)');

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
          confirmBtn.textContent = '\u21B5';
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
        // No column picker on an expression row, so nothing to pin.
        colPinned: false,
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

    /** Mark the segment that holds the current operator. */
    _syncOpToggle() {
      if (this._opSeg) this._opSeg.set(this.operator);
    }

    _updateUI() {
      const single = this.conditions.length <= 1;
      /** @type {HTMLElement} */ (this.opToggle).style.visibility = single ? 'hidden' : 'visible';
      for (const c of this.conditions) {
        const btn = /** @type {HTMLElement | null | undefined} */ (c.rowEl?.querySelector('.blockr-row-remove'));
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
      this._syncColWidth();
    }

    /**
     * Size the shared column track to the widest selected value across
     * condition rows. Sets --fb-col-w on the conditions list;
     * filter-block.css clamps it via min-width / max-width, so all rows
     * stay aligned and long column names get room instead of a fixed
     * 160px.
     */
    _syncColWidth() {
      let max = 0;
      for (const c of this.conditions) {
        const v = c.rowEl?.querySelector('.fb-col-wrap .blockr-select__value');
        if (v) max = Math.max(max, Blockr.contentWidth(v));
      }
      const listEl = /** @type {HTMLDivElement} */ (this.listEl);
      if (max > 0) {
        // + control padding (12) + gap (3) + arrow (16) + wrap padding (8)
        listEl.style.setProperty('--fb-col-w', `${max + 39}px`);
      } else {
        listEl.style.removeProperty('--fb-col-w');
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
      return { conditions, operator: this.operator, preserve_order: this.preserveOrder };
    }

    _submit() {
      this.host.submit(this._compose());
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
      this._syncOpToggle();

      // Restore "preserve order" toggle
      this.preserveOrder = !!state?.preserve_order;
      if (this._preserveBox) {
        this._preserveBox.set(this.preserveOrder);
      }

      // Rebuild from state
      // R canonicalises this to a list of records (as_record_list), but a
      // stale saved board can still send one flat record as an object. An
      // unguarded for-of throws and the block renders nothing, silently.
      const conditionsRaw = state?.conditions || [];
      const conditions = Array.isArray(conditionsRaw) ? conditionsRaw : [conditionsRaw];
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

    /** @param {BlockrColumn[] | null | undefined} meta */
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
          const col = Blockr.reconcileColumn(
            c._colSelectize, this.columnOptions, c.column || '', c.colPinned
          );
          c.column = col;
          if (col && this.columnMeta[col]) {
            // Preserve existing values/op: column metadata refreshing must
            // not clobber user- or setState-provided condition values.
            this._onColumnChange(c, col, {
              values: c.values,
              numValue: c.numValue,
              op: c.op
            });
          }
          // A pinned column the frame does not carry keeps its row as it
          // stands — values, operator and `_savedColType` all intact — so
          // _compose() still ships the condition the board restored while the
          // upstream settles. Re-rendering it against absent metadata would
          // throw the values away.
        }
        if (c.exprInput) {
          c.exprInput.setColumns(this.columnNames);
        }
      }
      this._syncColWidth();
    }

    /** @param {string} colName */
    _requestColumnValues(colName) {
      const meta = this.columnMeta[colName];
      const loaded = meta &&
        (meta.values !== undefined || meta.uniqueValues !== undefined);
      if (loaded) {
        // Server-truncated column: the stored page may be a stale search
        // result — fetch a fresh unfiltered page on every open (cheap, the
        // server caps it).
        if (meta.truncated) this._searchColumnValues(colName, '');
        return;
      }
      if (this._pendingValueRequests.has(colName)) return;
      this._pendingValueRequests.add(colName);
      this._askColumnValues(colName, '');
    }

    /**
     * Ask the host for the value page matching `query` (server-search mode
     * on high-cardinality columns). Answers arrive in order, so the last one
     * wins.
     * @param {string} colName
     * @param {string} query
     */
    _searchColumnValues(colName, query) {
      this._askColumnValues(colName, query);
    }

    /**
     * @param {string} colName
     * @param {string} query
     */
    _askColumnValues(colName, query) {
      this.host.request('column_values', { input: 'data', column: colName, query })
        .then(
          (meta) => this.receiveColumnValues(meta),
          // a failed request leaves the list loading: stop the spinner
          () => {
            this._pendingValueRequests.delete(colName);
            for (const c of this.conditions) {
              if (c.column === colName) c._valueSelectize?.setLoading(false);
            }
          }
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
            c._valueSelectize.setSearchInfo(c._meta);
            c._valueSelectize.setLoading(false);
          } else {
            this._renderDynamicContent(c);
          }
        }
      }
    }
  }

  // -- The state is IR -------------------------------------------------------
  // The block's state is the filter step of the block IR: `pred`, a list of
  // predicates (ANDed), and `preserve_order`. The rows the UI draws are its
  // own reading of that IR: toIR() writes them, fromIR() reads them back. R
  // (ir_to_expr()) and blockx (SQL) translate the IR; nothing else knows the
  // rows. `&` and `|` take two or more arguments, so each row is one argument.

  /** @typedef {{ call: string, args: BlockrIrExpr[], named?: Record<string, BlockrIrExpr> }} IrCall */
  /** @typedef {Map<string, BlockrIrExpr>} Known IR shown as text, by that text */
  /**
   * @typedef {Object} Rows the UI's reading of the state
   * @property {any[]} conditions
   * @property {'&' | '|'} operator
   * @property {boolean} [preserve_order]
   */

  /** @type {Record<string, string>} */
  const NUM_OP = { is: '==', 'is not': '!=' };
  /** @type {Record<string, string>} */
  const NUM_OP_BACK = { '==': 'is', '!=': 'is not' };
  const COMPARE = ['>', '>=', '<', '<=', '==', '!='];

  /**
   * @param {unknown} e @param {string} name @param {number} [n] at least
   * @returns {e is IrCall}
   */
  const isCall = (e, name, n = 1) => !!e && typeof e === 'object' && 'call' in e &&
    /** @type {IrCall} */ (e).call === name && /** @type {IrCall} */ (e).args.length >= n;
  /** @param {BlockrIrExpr} e */
  const colOf = (e) => (e && 'col' in e ? e.col : null);

  /** @param {string[]} values @param {string | null | undefined} colType @returns {BlockrIrLit[]} */
  const castValues = (values, colType) => {
    if (colType === 'numeric' || colType === 'integer') return values.map(Number);
    if (colType === 'logical') return values.map((v) => v === 'TRUE');
    if (colType) return values;
    // no type tag (an old board): numbers when every value parses
    const nums = values.map(Number);
    return nums.every((n) => Number.isFinite(n)) ? nums : values;
  };

  /** @param {string} op @param {BlockrIrExpr[]} parts @returns {BlockrIrExpr} */
  const join = (op, parts) => (parts.length === 1 ? parts[0] : { call: op, args: parts });
  /** @param {BlockrIrExpr} e @returns {BlockrIrExpr} */
  const not = (e) => ({ call: '!', args: [e] });

  /** @param {any} c a row @param {Known} known @returns {BlockrIrExpr | null} */
  const rowToIR = (c, known) => {
    if (!c || !c.type) return null;
    if (c.type === 'expr') {
      const text = String(c.expr || '').trim();
      if (!text) return null;
      return known.get(text) || { code: text, lang: 'r' };
    }
    if (!c.column) return null;
    /** @type {BlockrIrExpr} */
    const col = { col: c.column };
    if (c.type === 'numeric') {
      if (c.value === null || c.value === undefined || c.value === '') return null;
      const op = NUM_OP[c.op] || c.op;
      if (!COMPARE.includes(op)) return null;
      return { call: op, args: [col, { lit: Number(c.value) }] };
    }
    if (c.type === 'values') {
      /** @type {string[]} */
      const values = (Array.isArray(c.values) ? c.values : [c.values]).map(String);
      if (!values.length) return null;
      const include = (c.mode || 'include') === 'include';
      const regular = values.filter((v) => v !== '<NA>' && v !== '<empty>');
      /** @type {BlockrIrExpr[]} */
      const parts = [];
      if (regular.length) {
        /** @type {BlockrIrExpr} */
        const inn = { call: '%in%', args: [col, { list: castValues(regular, c.colType) }] };
        parts.push(include ? inn : not(inn));
      }
      if (values.includes('<NA>')) {
        /** @type {BlockrIrExpr} */
        const na = { call: 'is.na', args: [col] };
        parts.push(include ? na : not(na));
      }
      if (values.includes('<empty>')) {
        parts.push({ call: include ? '==' : '!=', args: [col, { lit: '' }] });
      }
      return parts.length ? join(include ? '|' : '&', parts) : null;
    }
    return null;
  };

  /**
   * The rows as the filter step.
   * @param {Rows} ui @param {Known} known
   */
  const toIR = (ui, known) => {
    const parts = /** @type {BlockrIrExpr[]} */ (
      (ui.conditions || []).map((c) => rowToIR(c, known)).filter(Boolean)
    );
    const pred = ui.operator === '|' && parts.length > 1 ? [join('|', parts)] : parts;
    return { pred, preserve_order: !!ui.preserve_order };
  };

  /** @param {string} op @param {BlockrIrExpr} e @returns {BlockrIrExpr[]} */
  const flatten = (op, e) => (isCall(e, op, 2) ? e.args.flatMap((a) => flatten(op, a)) : [e]);

  /** @param {BlockrIrLit[]} list */
  const strs = (list) => list.map((v) => (v === true ? 'TRUE' : v === false ? 'FALSE' : String(v)));
  /** @param {BlockrIrLit[]} list */
  const typeOf = (list) => (list.length && list.every((v) => typeof v === 'number') ? 'numeric'
    : list.length && list.every((v) => typeof v === 'boolean') ? 'logical' : 'character');

  /**
   * One predicate as a values row, if it is one: an %in%, is.na() and == ""
   * on one column, ORed (include), or their negations ANDed (exclude).
   * @param {BlockrIrExpr} e
   */
  const valuesRow = (e) => {
    for (const [op, include] of /** @type {[string, boolean][]} */ ([['|', true], ['&', false]])) {
      /** @type {string | null} */
      let column = null;
      /** @type {string[]} */
      let values = [];
      /** @type {string | null} */
      let colType = null;
      let ok = true;
      for (let p of flatten(op, e)) {
        let neg = false;
        if (isCall(p, '!')) { neg = true; p = p.args[0]; }
        /** @type {string | null} */
        let col = null;
        if (isCall(p, '%in%', 2) && 'list' in p.args[1] && neg === !include) {
          col = colOf(p.args[0]);
          values = values.concat(strs(p.args[1].list));
          colType = typeOf(p.args[1].list);
        } else if (isCall(p, 'is.na') && neg === !include) {
          col = colOf(p.args[0]);
          values.push('<NA>');
        } else if (!neg && isCall(p, include ? '==' : '!=', 2) &&
                   'lit' in p.args[1] && p.args[1].lit === '') {
          col = colOf(p.args[0]);
          values.push('<empty>');
        }
        if (!col || (column && col !== column)) { ok = false; break; }
        column = col;
      }
      if (ok && column) {
        return { type: 'values', column, values, mode: include ? 'include' : 'exclude', colType };
      }
    }
    return null;
  };

  /** @param {BlockrIrExpr} e @param {Known} known */
  const rowFromIR = (e, known) => {
    if (!e || typeof e !== 'object') return null;
    if ('code' in e) return { type: 'expr', expr: e.code };
    const values = valuesRow(e);
    if (values) return values;
    if ('call' in e && COMPARE.includes(e.call) && e.args.length === 2 &&
        colOf(e.args[0]) && 'lit' in e.args[1] && typeof e.args[1].lit === 'number') {
      return { type: 'numeric', column: colOf(e.args[0]), op: NUM_OP_BACK[e.call] || e.call,
        value: e.args[1].lit };
    }
    // IR the rows cannot draw (from code or an assistant): an expression row
    // that keeps the IR until its text is edited
    const text = Blockr.ir.toR(e);
    known.set(text, e);
    return { type: 'expr', expr: text };
  };

  /**
   * The filter step as rows.
   * @param {any} state @param {Known} known @returns {Rows}
   */
  const fromIR = (state, known) => {
    /** @type {BlockrIrExpr[]} */
    const pred = Array.isArray(state && state.pred) ? state.pred : [];
    /** @type {'&' | '|'} */
    let operator = '&';
    let parts = pred;
    if (pred.length === 1) {
      const single = pred[0];
      const asRow = rowFromIR(single, new Map());
      if ((asRow && asRow.type !== 'expr') || 'code' in single) {
        parts = [single];
      } else if (isCall(single, '|', 2)) {
        // each argument a row, as toIR() writes it
        operator = '|';
        parts = single.args;
      } else if (isCall(single, '&', 2)) {
        parts = single.args;
      }
    }
    return {
      conditions: parts.map((e) => rowFromIR(e, known)).filter(Boolean),
      operator,
      preserve_order: !!(state && state.preserve_order)
    };
  };

  Blockr.defineBlock({
    name: 'filter',
    // for tests
    ir: { toIR, fromIR },
    mount(el, host) {
      /** @type {Known} */
      const known = new Map();
      const block = new FilterBlock(el, {
        state: host.state,
        context: host.context,
        submit: (ui) => host.submit(toIR(ui, known)),
        request: host.request
      });
      block.setState(fromIR(host.state, known));
      block.updateColumns(host.context.inputs.data?.columns);
      return {
        block, // for tests
        setState: (state) => block.setState(fromIR(state, known)),
        setContext: (context) => block.updateColumns(context.inputs.data?.columns)
      };
    }
  });
})();
