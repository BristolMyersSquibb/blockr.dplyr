// @ts-check
/**
 * JoinBlock — JS-driven join block input binding.
 *
 * Binary block: takes x and y data inputs.
 * Key rows: [x col select] [operator pill] [y col select] [remove]
 * Expression rows: [BlockrInput] [confirm] [remove]
 * Header: join type pill (click-cycle) + gear settings button
 * Settings popover: suffix X and Y text inputs
 *
 * Depends on: blockr-core.js, blockr-select.js, blockr-input.js
 */

/**
 * One join key, as exchanged with R (R: make_join_expr() `keys` in
 * R/expr-builders.R). An `op` other than "==" makes R emit a
 * dplyr::join_by() non-equi join.
 * @typedef {Object} JoinKey
 * @property {string} xCol Column in the x (left) input.
 * @property {string} op   One of "==", ">", ">=", "<", "<=".
 * @property {string} yCol Column in the y (right) input.
 */

/**
 * Block state: what _compose() returns and setState() receives
 * (R: make_join_expr(type, keys, exprs, suffix_x, suffix_y)).
 * @typedef {Object} JoinState
 * @property {string} type     Join type, e.g. "left_join" (see JOIN_TYPES).
 * @property {JoinKey[]} keys
 * @property {string[]} exprs  Free-form R expressions for dplyr::join_by().
 * @property {string} suffix_x Suffix for duplicate x columns (default ".x").
 * @property {string} suffix_y Suffix for duplicate y columns (default ".y").
 */

/**
 * Internal key-row record: the key model plus its UI handles.
 * @typedef {Object} JoinKeyRow
 * @property {number} id
 * @property {string} xCol
 * @property {string} op
 * @property {string} yCol
 * @property {BlockrSelectSingleHandle | null} _xSelectize
 * @property {BlockrSelectSingleHandle | null} _ySelectize
 * @property {HTMLButtonElement} [_opBtn] Assigned right after row creation.
 * @property {HTMLDivElement | null} rowEl
 */

/**
 * Internal expression-row record.
 * @typedef {Object} JoinExprRow
 * @property {number} id
 * @property {BlockrInputHandle} exprInput
 * @property {HTMLDivElement} rowEl
 */

(() => {
  'use strict';

  // Expression categories for autocomplete
  const defaultCategories = {
    arithmetic: ['abs', 'sign', 'ceiling', 'floor', 'round', 'trunc', 'log', 'log2', 'log10', 'exp', 'sqrt'],
    aggregate: ['mean', 'sum', 'min', 'max'],
    offset: ['lead', 'lag', 'cumsum', 'cumprod', 'cummin', 'cummax'],
    logical: ['if_else', 'case_when', 'between'],
    string: ['str_c', 'paste', 'paste0', 'str_sub', 'str_to_lower', 'str_to_upper'],
    ranking: ['row_number', 'min_rank', 'dense_rank', 'percent_rank', 'ntile']
  };

  // Join type definitions
  const JOIN_TYPES = [
    { value: 'left_join',  label: 'left join' },
    { value: 'inner_join', label: 'inner join' },
    { value: 'right_join', label: 'right join' },
    { value: 'full_join',  label: 'full join' },
    { value: 'semi_join',  label: 'semi join' },
    { value: 'anti_join',  label: 'anti join' }
  ];

  // Key operator definitions
  const KEY_OPS = [
    { value: '==', label: '==' },
    { value: '>=', label: '\u2265' },
    { value: '>',  label: '>' },
    { value: '<=', label: '\u2264' },
    { value: '<',  label: '<' }
  ];

  class JoinBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {JoinKeyRow[]} */
      this.keys = [];
      /** @type {JoinExprRow[]} */
      this.exprRows = [];
      this.nextId = 1;
      /** @type {string[]} */
      this.xColumns = [];
      /** @type {string[]} */
      this.yColumns = [];
      /** @type {{ value: string, label: string }[]} */
      this.xColumnOptions = [];
      /** @type {{ value: string, label: string }[]} */
      this.yColumnOptions = [];
      /** @type {Record<string, BlockrPickerColumn>} */
      this.xColumnMeta = {};
      /** @type {Record<string, BlockrPickerColumn>} */
      this.yColumnMeta = {};
      this.joinTypeIdx = 0;
      this.joinType = JOIN_TYPES[0].value;
      this.suffixX = '.x';
      this.suffixY = '.y';
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {ReturnType<typeof setTimeout> | null} */
      this._debounceTimer = null;
      this._popoverOpen = false;

      this._buildDOM();
      this._addKeyRow(null, null, null);
    }

    _autoSubmit() {
      clearTimeout(/** @type {ReturnType<typeof setTimeout>} */ (this._debounceTimer));
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    _buildDOM() {
      // Card
      this.card = document.createElement('div');
      this.card.className = 'jb-card';
      this.el.appendChild(this.card);

      // Gear header (top-right)
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = Blockr.icons.gear;
      this.gearBtn.title = 'Suffix settings';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // Header row: join type pill
      const header = document.createElement('div');
      header.className = 'jb-header';

      // Join type pill (click-through cycle)
      this.joinTypePill = document.createElement('button');
      this.joinTypePill.type = 'button';
      this.joinTypePill.className = 'blockr-pill jb-join-type-pill';
      this.joinTypePill.textContent = JOIN_TYPES[0].label;
      this.joinTypePill.title = 'Cycle through join types: left (keep all x rows), inner (only matching), right (keep all y rows), full (keep everything), semi (x rows with a match), anti (x rows without a match)';
      this.joinTypePill.addEventListener('click', () => {
        this.joinTypeIdx = (this.joinTypeIdx + 1) % JOIN_TYPES.length;
        this.joinType = JOIN_TYPES[this.joinTypeIdx].value;
        /** @type {HTMLButtonElement} */ (this.joinTypePill).textContent = JOIN_TYPES[this.joinTypeIdx].label;
        this._autoSubmit();
      });
      header.appendChild(this.joinTypePill);

      this.card.appendChild(header);

      // Settings popover
      this._buildPopover();

      // Keys/expressions list
      this.listEl = document.createElement('div');
      this.listEl.className = 'jb-keys-list';
      this.card.appendChild(this.listEl);

      // Add row bar: [+ Add key] [</>]
      const addRow = document.createElement('div');
      addRow.className = 'blockr-add-row';

      const addKeyLink = document.createElement('span');
      addKeyLink.className = 'blockr-add-link';
      addKeyLink.innerHTML = `<span class="blockr-add-icon">${Blockr.icons.plus}</span> Add key`;
      addKeyLink.addEventListener('click', () => this._addKeyRow(null, null, null));
      addRow.appendChild(addKeyLink);

      const addExprLink = document.createElement('span');
      addExprLink.className = 'blockr-add-link-expr';
      addExprLink.innerHTML = Blockr.icons.code;
      addExprLink.title = 'Add R expression';
      addExprLink.addEventListener('click', () => this._addExprRow(''));
      addRow.appendChild(addExprLink);

      this.card.appendChild(addRow);

      // Close popover on outside click
      Blockr.onDocClick(this.el, (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(/** @type {Node | null} */ (e.target)) &&
            !/** @type {HTMLButtonElement} */ (this.gearBtn).contains(/** @type {Node | null} */ (e.target))) {
          this._closePopover();
        }
      });
    }

    // --- Settings popover ---

    _buildPopover() {
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-popover';
      this.popoverEl.style.display = 'none';

      // Suffix X
      const rowX = document.createElement('div');
      rowX.className = 'blockr-popover-row';
      const labelX = document.createElement('label');
      labelX.textContent = 'Suffix X:';
      labelX.className = 'blockr-popover-label';
      this.suffixXInput = document.createElement('input');
      this.suffixXInput.type = 'text';
      this.suffixXInput.className = 'blockr-popover-input';
      this.suffixXInput.value = this.suffixX;
      this.suffixXInput.addEventListener('input', () => {
        this.suffixX = /** @type {HTMLInputElement} */ (this.suffixXInput).value;
        this._autoSubmit();
      });
      rowX.appendChild(labelX);
      rowX.appendChild(this.suffixXInput);
      this.popoverEl.appendChild(rowX);

      // Suffix Y
      const rowY = document.createElement('div');
      rowY.className = 'blockr-popover-row';
      const labelY = document.createElement('label');
      labelY.textContent = 'Suffix Y:';
      labelY.className = 'blockr-popover-label';
      this.suffixYInput = document.createElement('input');
      this.suffixYInput.type = 'text';
      this.suffixYInput.className = 'blockr-popover-input';
      this.suffixYInput.value = this.suffixY;
      this.suffixYInput.addEventListener('input', () => {
        this.suffixY = /** @type {HTMLInputElement} */ (this.suffixYInput).value;
        this._autoSubmit();
      });
      rowY.appendChild(labelY);
      rowY.appendChild(this.suffixYInput);
      this.popoverEl.appendChild(rowY);

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

    // --- Key rows ---

    /**
     * @param {string | null} xCol
     * @param {string | null} op
     * @param {string | null} yCol
     */
    _addKeyRow(xCol, op, yCol) {
      const id = this.nextId++;
      /** @type {JoinKeyRow} */
      const key = {
        id,
        xCol: xCol || '',
        op: op || '==',
        yCol: yCol || '',
        _xSelectize: null,
        _ySelectize: null,
        rowEl: null
      };

      const row = document.createElement('div');
      row.className = 'blockr-row';
      row.setAttribute('data-key-id', /** @type {string} */ (/** @type {*} */ (id)));
      key.rowEl = row;

      // X column selectize
      const xDiv = document.createElement('div');
      xDiv.className = 'jb-col-wrap jb-col-x';
      row.appendChild(xDiv);
      key._xSelectize = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(xDiv, {
        options: this.xColumnOptions,
        // null is handled by createSelect's `!= null` fallback-to-first-option
        selected: /** @type {string} */ (xCol),
        placeholder: 'x column\u2026',
        onChange: (value) => {
          key.xCol = value;
          this._syncColWidth();
          this._autoSubmit();
        }
      });

      // Operator pill (click-through cycle)
      let opIdx = KEY_OPS.findIndex(o => o.value === (op || '=='));
      if (opIdx < 0) opIdx = 0;
      const opBtn = document.createElement('button');
      opBtn.type = 'button';
      opBtn.className = 'blockr-pill jb-op-btn';
      opBtn.textContent = KEY_OPS[opIdx].label;
      opBtn.title = 'Cycle through join operators (==, \u2265, >, \u2264, <) for non-equi joins';
      opBtn.addEventListener('click', () => {
        opIdx = (opIdx + 1) % KEY_OPS.length;
        key.op = KEY_OPS[opIdx].value;
        opBtn.textContent = KEY_OPS[opIdx].label;
        this._autoSubmit();
      });
      key._opBtn = opBtn;
      row.appendChild(opBtn);

      // Y column selectize
      const yDiv = document.createElement('div');
      yDiv.className = 'jb-col-wrap jb-col-y';
      row.appendChild(yDiv);
      key._ySelectize = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(yDiv, {
        options: this.yColumnOptions,
        // null is handled by createSelect's `!= null` fallback-to-first-option
        selected: /** @type {string} */ (yCol),
        placeholder: 'y column\u2026',
        onChange: (value) => {
          key.yCol = value;
          this._autoSubmit();
        }
      });

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => {
        this._removeKey(id);
        this._autoSubmit();
      });
      row.appendChild(rmBtn);

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(row);
      this.keys.push(key);
      this._updateUI();
    }

    /** @param {number} id */
    _removeKey(id) {
      const totalRows = this.keys.length + this.exprRows.length;
      if (totalRows <= 1) return;

      const idx = this.keys.findIndex(k => k.id === id);
      if (idx < 0) return;

      const key = this.keys[idx];
      key._xSelectize?.destroy();
      key._ySelectize?.destroy();
      key.rowEl?.parentNode?.removeChild(key.rowEl);
      this.keys.splice(idx, 1);
      this._updateUI();
    }

    // --- Expression rows ---

    /** @param {string} value */
    _addExprRow(value) {
      const id = this.nextId++;

      const row = document.createElement('div');
      row.className = 'blockr-row jb-row-expr';
      row.setAttribute('data-expr-id', /** @type {string} */ (/** @type {*} */ (id)));

      const codeDiv = document.createElement('div');
      codeDiv.className = 'blockr-row-content jb-expr-code';
      row.appendChild(codeDiv);

      // Confirm button
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

      const allCols = this.xColumns.concat(this.yColumns);
      const exprInput = /** @type {BlockrInputStatic} */ (Blockr.Input).create(codeDiv, {
        value,
        columns: allCols,
        categories: defaultCategories,
        placeholder: 'R expression\u2026',
        onChange: () => {
          confirmBtn.classList.remove('confirmed');
          confirmBtn.innerHTML = 'Enter \u21B5';
        },
        onConfirm: () => doConfirm()
      });
      row.appendChild(confirmBtn);

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => this._removeExpr(id));
      row.appendChild(rmBtn);

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(row);
      this.exprRows.push({ id, exprInput, rowEl: row });
      this._updateUI();
    }

    /** @param {number} id */
    _removeExpr(id) {
      const totalRows = this.keys.length + this.exprRows.length;
      if (totalRows <= 1) return;

      const idx = this.exprRows.findIndex(er => er.id === id);
      if (idx < 0) return;

      const er = this.exprRows[idx];
      er.exprInput?.destroy();
      er.rowEl?.parentNode?.removeChild(er.rowEl);
      this.exprRows.splice(idx, 1);
      this._updateUI();
    }

    // --- UI management ---

    _updateUI() {
      const totalRows = this.keys.length + this.exprRows.length;
      const single = totalRows <= 1;

      for (const k of this.keys) {
        const btn = /** @type {HTMLElement | null | undefined} */ (k.rowEl?.querySelector('.blockr-row-remove'));
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
      for (const er of this.exprRows) {
        const btn = /** @type {HTMLElement | null | undefined} */ (er.rowEl?.querySelector('.blockr-row-remove'));
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
      this._syncColWidth();
    }

    /**
     * Size the shared x-column track to the widest selected value across
     * key rows. Sets --jb-col-w on the keys list; join-block.css clamps
     * it via min-width / max-width, so key rows stay aligned and long
     * column names get room instead of a fixed 140px.
     */
    _syncColWidth() {
      let max = 0;
      for (const k of this.keys) {
        const v = k.rowEl?.querySelector('.jb-col-x .blockr-select__value');
        if (v) max = Math.max(max, Blockr.contentWidth(v));
      }
      const listEl = /** @type {HTMLDivElement} */ (this.listEl);
      if (max > 0) {
        // + control padding (12) + gap (3) + arrow (16) + wrap padding (8)
        listEl.style.setProperty('--jb-col-w', `${max + 39}px`);
      } else {
        listEl.style.removeProperty('--jb-col-w');
      }
    }

    // --- Compose output ---

    /** @returns {JoinState} */
    _compose() {
      /** @type {JoinKey[]} */
      const keys = [];
      for (const k of this.keys) {
        if (k.xCol && k.yCol) {
          keys.push({ xCol: k.xCol, op: k.op, yCol: k.yCol });
        }
      }

      /** @type {string[]} */
      const exprs = [];
      for (const er of this.exprRows) {
        if (er.exprInput) {
          const val = er.exprInput.getValue();
          if (val) exprs.push(val);
        }
      }

      return {
        type: this.joinType,
        keys,
        exprs,
        suffix_x: this.suffixX,
        suffix_y: this.suffixY
      };
    }

    /** @returns {JoinState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {Partial<JoinState> | null | undefined} state
     * @param {boolean} [silent] Unused here; kept for setState() symmetry.
     */
    setState(state, silent) {
      // Clear existing keys
      while (this.keys.length > 0) {
        const key = this.keys[0];
        key._xSelectize?.destroy();
        key._ySelectize?.destroy();
        key.rowEl?.parentNode?.removeChild(key.rowEl);
        this.keys.splice(0, 1);
      }

      // Clear existing expression rows
      while (this.exprRows.length > 0) {
        const er = this.exprRows[0];
        er.exprInput?.destroy();
        er.rowEl?.parentNode?.removeChild(er.rowEl);
        this.exprRows.splice(0, 1);
      }

      // Set join type
      const type = state?.type || 'left_join';
      this.joinType = type;
      this.joinTypeIdx = JOIN_TYPES.findIndex(t => t.value === type);
      if (this.joinTypeIdx < 0) this.joinTypeIdx = 0;
      /** @type {HTMLButtonElement} */ (this.joinTypePill).textContent = JOIN_TYPES[this.joinTypeIdx].label;

      // Set suffixes
      this.suffixX = state?.suffix_x ?? '.x';
      this.suffixY = state?.suffix_y ?? '.y';
      /** @type {HTMLInputElement} */ (this.suffixXInput).value = this.suffixX;
      /** @type {HTMLInputElement} */ (this.suffixYInput).value = this.suffixY;

      // Rebuild keys
      const keys = state?.keys || [];
      const exprs = state?.exprs || [];

      if (keys.length === 0 && exprs.length === 0) {
        this._addKeyRow(null, null, null);
      } else {
        for (const k of keys) {
          this._addKeyRow(k.xCol || null, k.op || null, k.yCol || null);
        }
        for (const e of exprs) {
          this._addExprRow(e || '');
        }
      }
      // Mark expression confirm buttons as confirmed (state is already applied)
      for (const e of this.exprRows) {
        const btn = e.rowEl?.querySelector('.blockr-expr-confirm');
        if (btn) {
          btn.classList.add('confirmed');
          btn.innerHTML = Blockr.icons.confirm;
        }
      }
      this._updateUI();
    }

    /**
     * @param {BlockrPickerColumn[] | null | undefined} xMeta
     * @param {BlockrPickerColumn[] | null | undefined} yMeta
     */
    updateColumns(xMeta, yMeta) {
      this.xColumnMeta = {};
      this.xColumns = [];
      this.xColumnOptions = [];
      for (const col of (xMeta || [])) {
        this.xColumnMeta[col.name] = col;
        this.xColumns.push(col.name);
        this.xColumnOptions.push({ value: col.name, label: col.label || '' });
      }
      this.yColumnMeta = {};
      this.yColumns = [];
      this.yColumnOptions = [];
      for (const col of (yMeta || [])) {
        this.yColumnMeta[col.name] = col;
        this.yColumns.push(col.name);
        this.yColumnOptions.push({ value: col.name, label: col.label || '' });
      }

      // Update key row selectizes
      for (const k of this.keys) {
        if (k._xSelectize) {
          const curX = k._xSelectize.getValue();
          k._xSelectize.setOptions(this.xColumnOptions, curX);
          k.xCol = k._xSelectize.getValue();
        }
        if (k._ySelectize) {
          const curY = k._ySelectize.getValue();
          k._ySelectize.setOptions(this.yColumnOptions, curY);
          k.yCol = k._ySelectize.getValue();
        }
      }

      // Update expression inputs with combined columns
      const allCols = this.xColumns.concat(this.yColumns);
      for (const er of this.exprRows) {
        if (er.exprInput) {
          er.exprInput.setColumns(allCols);
        }
      }

      this._syncColWidth();

      // Auto-submit now that columns are available
      this._autoSubmit();
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'join',
    Block: JoinBlock,
    messages: {
      'join-columns': (block, msg) => block.updateColumns(msg.xColumns, msg.yColumns),
      'join-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
