// @ts-check
/**
 * SummarizeBlock — JS-driven summarize block input binding.
 *
 * Two row types:
 *   Simple: [name] = [func dropdown (Blockr.Select)] of [col dropdown (Blockr.Select)] [remove]
 *   Expression: [name] = [Blockr.Input] [confirm] [remove]
 * Plus a "Group by" section with Blockr.Select.multi below.
 *
 * Depends on: blockr-core.js, blockr-select.js, blockr-input.js
 */

/**
 * One summary, as exchanged with R (R: make_summarize_expr() `summaries`
 * in R/expr-builders.R).
 * @typedef {Object} SummarizeSummary
 * @property {'simple' | 'expr'} type
 * @property {string} name   Output column name (auto-generated when blank).
 * @property {string} [func] simple rows: summary function, possibly
 *   namespaced (e.g. "stats::median"; R maps short names via
 *   summarize_func_map()).
 * @property {string} [col]  simple rows: input column ("" for n()).
 * @property {string} [expr] expr rows: free-form R expression.
 */

/**
 * Block state: what _compose() returns and setState() receives
 * (R: make_summarize_expr(summaries, by)).
 * @typedef {Object} SummarizeState
 * @property {SummarizeSummary[]} summaries
 * @property {string | string[]} by Grouping columns for `.by` (jsonlite
 *   may deliver a single column as a scalar string).
 */

/**
 * Internal row record: the summary model plus its UI handles. Simple rows
 * carry func/col and the two selectizes; expr rows carry exprInput.
 * @typedef {Object} SummarizeRow
 * @property {number} id
 * @property {'simple' | 'expr'} type
 * @property {string} name
 * @property {string} [func]
 * @property {string} [col]
 * @property {HTMLDivElement | null} rowEl
 * @property {BlockrInputHandle} [exprInput]
 * @property {BlockrSelectSingleHandle | null} [_funcSelectize]
 * @property {BlockrSelectSingleHandle | null} [_colSelectize]
 * @property {HTMLDivElement | null} [_colWrap]
 * @property {HTMLInputElement | null} [_nameInput]
 * @property {HTMLSpanElement | null} [_ofLabel]
 */

(() => {
  'use strict';

  // Default summary functions available in simple mode
  const DEFAULT_SUMMARY_FUNCS = [
    'mean', 'median', 'sd', 'min', 'max', 'sum', 'n', 'n_distinct', 'first', 'last'
  ];

  // Functions that don't take a column argument
  const NO_COL_FUNCS = ['n'];

  // Expression categories for autocomplete
  const defaultCategories = {
    aggregate: ['mean', 'median', 'sd', 'sum', 'min', 'max', 'n', 'n_distinct', 'first', 'last'],
    arithmetic: ['abs', 'sign', 'ceiling', 'floor', 'round', 'trunc', 'log', 'log2', 'log10', 'exp', 'sqrt'],
    logical: ['if_else', 'case_when'],
    string: ['str_c', 'paste', 'paste0', 'str_sub', 'str_to_lower', 'str_to_upper']
  };

  class SummarizeBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {SummarizeRow[]} */
      this.summaries = [];
      this.nextId = 1;
      /** @type {string[]} */
      this.columnNames = [];
      /** @type {{ value: string, label: string }[]} */
      this.columnOptions = [];
      /** @type {Record<string, BlockrPickerColumn>} */
      this.columnMeta = {};
      /** @type {string[]} */
      this.byValues = [];
      this.summaryFuncs = DEFAULT_SUMMARY_FUNCS.slice();
      // Maps display label -> namespaced call (e.g. "paren_num" -> "blockr.topline::paren_num")
      /** @type {Record<string, string>} */
      this._funcMap = {};
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {BlockrSelectMultiHandle | null} */
      this._bySelectize = null;

      this._buildDOM();
      this._addSimpleRow(null, null, null);
    }

    _buildDOM() {
      // Card
      this.card = document.createElement('div');
      this.card.className = 'smb-card';
      this.el.appendChild(this.card);

      // Summaries list
      this.listEl = document.createElement('div');
      this.listEl.className = 'smb-summaries';
      this.card.appendChild(this.listEl);

      // Add row: [+ Add summary] [</>]
      const addRow = document.createElement('div');
      addRow.className = 'blockr-add-row';

      const addSimpleLink = document.createElement('span');
      addSimpleLink.className = 'blockr-add-link';
      addSimpleLink.innerHTML = `<span class="blockr-add-icon">${Blockr.icons.plus}</span> Add summary`;
      addSimpleLink.addEventListener('click', () => this._addSimpleRow(null, null, null));
      addRow.appendChild(addSimpleLink);

      const addExprLink = document.createElement('span');
      addExprLink.className = 'blockr-add-link-expr';
      addExprLink.innerHTML = Blockr.icons.code;
      addExprLink.title = 'Add R expression';
      addExprLink.addEventListener('click', () => this._addExprRow('', ''));
      addRow.appendChild(addExprLink);

      this.card.appendChild(addRow);

      // Group by section (below the card)
      this.bySection = document.createElement('div');
      this.bySection.className = 'smb-by-section';

      const byLabel = document.createElement('span');
      byLabel.className = 'blockr-label';
      byLabel.textContent = 'Group by:';
      this.bySection.appendChild(byLabel);

      const byWrap = document.createElement('div');
      byWrap.className = 'smb-by-wrap';
      this.bySection.appendChild(byWrap);

      this._bySelectize = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(byWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'None',
        reorderable: true,
        onChange: (value) => { this.byValues = value || []; this._submit(); }
      });
      this._bySelectize.el.classList.add('blockr-select--bordered');

      this.el.appendChild(this.bySection);
    }

    // --- Simple rows: [name] = [func] of [col] [x] ---

    /**
     * @param {string | null} name
     * @param {string | null} func
     * @param {string | null} col
     */
    _addSimpleRow(name, func, col) {
      const id = this.nextId++;
      /** @type {SummarizeRow} */
      const summary = {
        id,
        type: 'simple',
        name: name || '',
        func: func || this.summaryFuncs[0],
        col: col || '',
        rowEl: null,
        _funcSelectize: null,
        _colSelectize: null,
        _colWrap: null,
        _nameInput: null,
        _ofLabel: null
      };

      const row = document.createElement('div');
      row.className = 'blockr-row smb-row-simple';
      row.setAttribute('data-summary-id', /** @type {string} */ (/** @type {*} */ (id)));
      summary.rowEl = row;

      // Name input
      const nameInput = document.createElement('input');
      nameInput.type = 'text';
      nameInput.className = 'smb-name-input';
      nameInput.placeholder = 'column_name';
      nameInput.value = name || '';
      summary._nameInput = nameInput;
      row.appendChild(nameInput);
      // Commits on Enter/blur with the "Enter ↵" chip (§5.5)
      Blockr.textCommit(nameInput, {
        onCommit: (value) => {
          summary.name = value;
          this._submit();
        }
      });

      // Equals sign
      const eqSign = document.createElement('span');
      eqSign.className = 'smb-eq-sign';
      eqSign.textContent = '=';
      row.appendChild(eqSign);

      // Function selectize — include custom funcs not in the predefined list
      const funcDiv = document.createElement('div');
      funcDiv.className = 'smb-func-wrap';
      row.appendChild(funcDiv);
      const funcOptions = (func && !this.summaryFuncs.includes(func))
        ? [...this.summaryFuncs, func]
        : this.summaryFuncs;
      summary._funcSelectize = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(funcDiv, {
        options: funcOptions,
        selected: func || this.summaryFuncs[0],
        placeholder: 'Function\u2026',
        onChange: (value) => {
          summary.func = value;
          this._updateColVisibility(summary);
          this._submit();
        }
      });

      // "of" label
      const ofLabel = document.createElement('span');
      ofLabel.className = 'smb-of-label';
      ofLabel.textContent = 'of';
      summary._ofLabel = ofLabel;
      row.appendChild(ofLabel);

      // Column selectize
      const colWrap = document.createElement('div');
      colWrap.className = 'smb-col-wrap';
      row.appendChild(colWrap);
      summary._colWrap = colWrap;
      summary._colSelectize = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(colWrap, {
        options: this.columnOptions,
        // null is handled by createSelect's `!= null` fallback-to-first-option
        selected: /** @type {string} */ (col),
        placeholder: 'Column\u2026',
        onChange: (value) => {
          summary.col = value;
          this._submit();
        }
      });

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => {
        this._removeSummary(id);
        this._submit();
      });
      row.appendChild(rmBtn);

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(row);
      this.summaries.push(summary);
      this._updateUI();
      this._updateColVisibility(summary);
    }

    // Hide column selectize when function is n()
    /** @param {SummarizeRow} summary a simple row (func always set) */
    _updateColVisibility(summary) {
      const isNoCol = NO_COL_FUNCS.includes(/** @type {string} */ (summary.func));
      if (summary._colWrap) {
        summary._colWrap.style.display = isNoCol ? 'none' : '';
      }
      if (summary._ofLabel) {
        summary._ofLabel.style.display = isNoCol ? 'none' : '';
      }
    }

    // --- Expression rows: [name] = [Blockr.Input] [confirm] [x] ---

    /**
     * @param {string} name
     * @param {string} value
     */
    _addExprRow(name, value) {
      const id = this.nextId++;

      const row = document.createElement('div');
      row.className = 'blockr-row smb-row-expr';
      row.setAttribute('data-summary-id', /** @type {string} */ (/** @type {*} */ (id)));

      // Name input
      const nameInput = document.createElement('input');
      nameInput.type = 'text';
      nameInput.className = 'smb-name-input';
      nameInput.placeholder = 'column_name';
      nameInput.value = name || '';
      row.appendChild(nameInput);

      // Equals sign
      const eqSign = document.createElement('span');
      eqSign.className = 'smb-eq-sign';
      eqSign.textContent = '=';
      row.appendChild(eqSign);

      // Expression editor
      const codeDiv = document.createElement('div');
      codeDiv.className = 'blockr-row-content smb-expr-code';
      row.appendChild(codeDiv);

      // Confirm button
      const confirmBtn = document.createElement('button');
      confirmBtn.className = 'blockr-expr-confirm';
      confirmBtn.type = 'button';
      confirmBtn.innerHTML = 'Enter <span class="blockr-kbd">\u21B5</span>';
      confirmBtn.title = 'Apply expression';

      const doConfirm = () => {
        confirmBtn.classList.add('confirmed');
        confirmBtn.innerHTML = Blockr.icons.confirm;
        this._submit();
      };
      confirmBtn.addEventListener('click', doConfirm);

      const exprInput = /** @type {BlockrInputStatic} */ (Blockr.Input).create(codeDiv, {
        value: value || '',
        columns: this.columnNames,
        categories: defaultCategories,
        placeholder: 'R expression\u2026',
        onChange: () => {
          confirmBtn.classList.remove('confirmed');
          confirmBtn.innerHTML = 'Enter <span class="blockr-kbd">\u21B5</span>';
        },
        onConfirm: () => doConfirm()
      });
      row.appendChild(confirmBtn);

      // Name input joins the row's confirm cycle (§5.5): typing re-arms
      // the chip, Enter or blur commits the row (only when dirty).
      let nameDirty = false;
      nameInput.addEventListener('input', () => {
        nameDirty = true;
        confirmBtn.classList.remove('confirmed');
        confirmBtn.innerHTML = 'Enter <span class="blockr-kbd">\u21B5</span>';
      });
      const commitName = () => {
        if (!nameDirty) return;
        nameDirty = false;
        doConfirm();
      };
      nameInput.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') { e.preventDefault(); commitName(); }
      });
      nameInput.addEventListener('blur', commitName);

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => this._removeSummary(id));
      row.appendChild(rmBtn);

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(row);
      this.summaries.push({
        id,
        type: 'expr',
        name: name || '',
        exprInput,
        _nameInput: nameInput,
        rowEl: row
      });
      this._updateUI();
    }

    // --- Shared ---

    /** @param {number} id */
    _removeSummary(id) {
      if (this.summaries.length <= 1) return;

      const idx = this.summaries.findIndex(s => s.id === id);
      if (idx < 0) return;

      const summary = this.summaries[idx];
      summary._funcSelectize?.destroy();
      summary._colSelectize?.destroy();
      summary.exprInput?.destroy();
      summary.rowEl?.parentNode?.removeChild(summary.rowEl);
      this.summaries.splice(idx, 1);
      this._updateUI();
    }

    _updateUI() {
      const single = this.summaries.length <= 1;
      for (const s of this.summaries) {
        const btn = /** @type {HTMLElement | null | undefined} */ (s.rowEl?.querySelector('.blockr-row-remove'));
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
    }

    /** @returns {SummarizeState} */
    _compose() {
      /** @type {SummarizeSummary[]} */
      const summaries = [];
      for (const s of this.summaries) {
        if (s.type === 'simple') {
          const funcLabel = s.func || '';
          if (!funcLabel) continue;
          // Resolve display label to namespaced call if mapped
          const func = this._funcMap[funcLabel] || funcLabel;
          const col = s.col || '';
          const isNoCol = NO_COL_FUNCS.includes(funcLabel);
          if (!isNoCol && !col) continue;
          // Auto-generate name if empty
          let name = (s._nameInput ? s._nameInput.value.trim() : '') || '';
          if (!name) {
            const shortFunc = func.replace(/.*::/, '');
            name = isNoCol ? shortFunc : shortFunc + '_' + col;
          }
          summaries.push({ type: 'simple', name, func, col });
        } else if (s.type === 'expr') {
          let ename = (s._nameInput ? s._nameInput.value.trim() : '') || '';
          if (!ename) ename = 'expr_' + (summaries.length + 1);
          const val = s.exprInput ? s.exprInput.getValue() : '';
          if (!val) continue;
          summaries.push({ type: 'expr', name: ename, expr: val });
        }
      }
      return { summaries, by: this.byValues || [] };
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    /** @returns {SummarizeState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {Partial<SummarizeState> | null | undefined} state
     * @param {boolean} [silent] Suppress the auto-submit after restoring.
     */
    setState(state, silent) {
      // Clear existing summaries
      while (this.summaries.length > 0) {
        const s = this.summaries[0];
        s._funcSelectize?.destroy();
        s._colSelectize?.destroy();
        s.exprInput?.destroy();
        s.rowEl?.parentNode?.removeChild(s.rowEl);
        this.summaries.splice(0, 1);
      }

      // Rebuild summaries
      const summaries = state?.summaries || [];
      if (summaries.length === 0) {
        this._addSimpleRow(null, null, null);
      } else {
        for (const s of summaries) {
          if (s.type === 'expr') {
            this._addExprRow(s.name || '', s.expr || '');
          } else {
            // Resolve namespaced func to display label for the dropdown
            const func = this._funcLabel(s.func || null);
            this._addSimpleRow(s.name || null, func, s.col || null);
          }
        }
      }
      // Mark expression confirm buttons as confirmed (state is already applied)
      for (const s of this.summaries) {
        const btn = s.rowEl?.querySelector('.blockr-expr-confirm');
        if (btn) {
          btn.classList.add('confirmed');
          btn.innerHTML = Blockr.icons.confirm;
        }
      }

      // Rebuild group by (ensure array — R may send scalar string)
      const byRaw = state?.by || [];
      const by = Array.isArray(byRaw) ? byRaw.slice() : [byRaw];
      this.byValues = by;
      if (this._bySelectize) {
        this._bySelectize.setOptions(this.columnOptions, by);
      }

      this._updateUI();

      // Auto-submit if state has content
      if (summaries.length > 0 && !silent) {
        this._submit();
      }
    }

    // Resolve a namespaced func value back to its display label
    /**
     * @param {string | null} func
     * @returns {string | null}
     */
    _funcLabel(func) {
      if (!func) return func;
      // Check if func is a namespaced value that has a label
      for (const [label, value] of Object.entries(this._funcMap)) {
        if (value === func) return label;
      }
      return func;
    }

    /** @param {{ value: string, label: string }[]} funcs */
    updateFunctions(funcs) {
      if (!Array.isArray(funcs) || funcs.length === 0) return;

      // funcs is an array of {value, label} objects from R
      // Build the display list and mapping
      /** @type {string[]} */
      const labels = [];
      this._funcMap = {};
      for (const f of funcs) {
        labels.push(f.label);
        // Map label -> namespaced value (only when they differ)
        if (f.label !== f.value) {
          this._funcMap[f.label] = f.value;
        }
      }
      this.summaryFuncs = labels;

      // Update function selectizes in existing simple rows
      for (const s of this.summaries) {
        if (s.type === 'simple' && s._funcSelectize) {
          const current = s._funcSelectize.getValue();
          const opts = (current && !this.summaryFuncs.includes(current))
            ? [...this.summaryFuncs, current]
            : this.summaryFuncs;
          s._funcSelectize.setOptions(opts, current);
        }
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

      // Refresh simple-row column pickers, but do not auto-assign a default
      // column to a row that legitimately has none (e.g. func = "n"), and
      // only clear the stored column if it has actually been removed.
      for (const s of this.summaries) {
        if (s.type === 'simple' && s._colSelectize) {
          s._colSelectize.setOptions(this.columnOptions, s.col || null);
          if (s.col && !this.columnNames.includes(s.col)) {
            s.col = "";
          }
        }
        if (s.exprInput) {
          s.exprInput.setColumns(this.columnNames);
        }
      }

      // Refresh group-by selectize and drop removed columns from model
      if (this._bySelectize) {
        this._bySelectize.setOptions(this.columnOptions, this.byValues);
        this.byValues = (this.byValues || []).filter(
          c => this.columnNames.includes(c)
        );
      }

      // Auto-submit now that columns are available
      this._submit();
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'summarize',
    Block: SummarizeBlock,
    messages: {
      'summarize-functions': (block, msg) => block.updateFunctions(msg.functions),
      'summarize-columns': (block, msg) => block.updateColumns(msg.columns),
      'summarize-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
