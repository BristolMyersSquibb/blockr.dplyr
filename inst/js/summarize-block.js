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
    constructor(el) {
      this.el = el;
      this.summaries = [];
      this.nextId = 1;
      this.columnNames = [];
      this.columnOptions = [];
      this.columnMeta = {};
      this.byValues = [];
      this.summaryFuncs = DEFAULT_SUMMARY_FUNCS.slice();
      // Maps display label -> namespaced call (e.g. "paren_num" -> "blockr.topline::paren_num")
      this._funcMap = {};
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._bySelectize = null;

      this._buildDOM();
      this._addSimpleRow(null, null, null);
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      // Card
      this.card = document.createElement('div');
      this.card.className = 'sb-card';
      this.el.appendChild(this.card);

      // Summaries list
      this.listEl = document.createElement('div');
      this.listEl.className = 'sb-summaries';
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
      this.bySection.className = 'sb-by-section';

      const byLabel = document.createElement('span');
      byLabel.className = 'blockr-label';
      byLabel.textContent = 'Group by:';
      this.bySection.appendChild(byLabel);

      const byWrap = document.createElement('div');
      byWrap.className = 'sb-by-wrap';
      this.bySection.appendChild(byWrap);

      this._bySelectize = Blockr.Select.multi(byWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select grouping columns\u2026',
        reorderable: true,
        onChange: (value) => { this.byValues = value || []; this._autoSubmit(); }
      });
      this._bySelectize.el.classList.add('blockr-select--bordered');

      this.el.appendChild(this.bySection);
    }

    // --- Simple rows: [name] = [func] of [col] [x] ---

    _addSimpleRow(name, func, col) {
      const id = this.nextId++;
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
      row.className = 'blockr-row sb-row-simple';
      row.setAttribute('data-summary-id', id);
      summary.rowEl = row;

      // Name input
      const nameInput = document.createElement('input');
      nameInput.type = 'text';
      nameInput.className = 'sb-name-input';
      nameInput.placeholder = 'column_name';
      nameInput.value = name || '';
      nameInput.addEventListener('input', () => {
        summary.name = nameInput.value;
        this._autoSubmit();
      });
      summary._nameInput = nameInput;
      row.appendChild(nameInput);

      // Equals sign
      const eqSign = document.createElement('span');
      eqSign.className = 'sb-eq-sign';
      eqSign.textContent = '=';
      row.appendChild(eqSign);

      // Function selectize — include custom funcs not in the predefined list
      const funcDiv = document.createElement('div');
      funcDiv.className = 'sb-func-wrap';
      row.appendChild(funcDiv);
      const funcOptions = (func && !this.summaryFuncs.includes(func))
        ? [...this.summaryFuncs, func]
        : this.summaryFuncs;
      summary._funcSelectize = Blockr.Select.single(funcDiv, {
        options: funcOptions,
        selected: func || this.summaryFuncs[0],
        placeholder: 'Function\u2026',
        onChange: (value) => {
          summary.func = value;
          this._updateColVisibility(summary);
          this._autoSubmit();
        }
      });

      // "of" label
      const ofLabel = document.createElement('span');
      ofLabel.className = 'sb-of-label';
      ofLabel.textContent = 'of';
      summary._ofLabel = ofLabel;
      row.appendChild(ofLabel);

      // Column selectize
      const colWrap = document.createElement('div');
      colWrap.className = 'sb-col-wrap';
      row.appendChild(colWrap);
      summary._colWrap = colWrap;
      summary._colSelectize = Blockr.Select.single(colWrap, {
        options: this.columnOptions,
        selected: col,
        placeholder: 'Column\u2026',
        onChange: (value) => {
          summary.col = value;
          this._autoSubmit();
        }
      });

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => {
        this._removeSummary(id);
        this._autoSubmit();
      });
      row.appendChild(rmBtn);

      this.listEl.appendChild(row);
      this.summaries.push(summary);
      this._updateUI();
      this._updateColVisibility(summary);
    }

    // Hide column selectize when function is n()
    _updateColVisibility(summary) {
      const isNoCol = NO_COL_FUNCS.includes(summary.func);
      if (summary._colWrap) {
        summary._colWrap.style.display = isNoCol ? 'none' : '';
      }
      if (summary._ofLabel) {
        summary._ofLabel.style.display = isNoCol ? 'none' : '';
      }
    }

    // --- Expression rows: [name] = [Blockr.Input] [confirm] [x] ---

    _addExprRow(name, value) {
      const id = this.nextId++;

      const row = document.createElement('div');
      row.className = 'blockr-row sb-row-expr';
      row.setAttribute('data-summary-id', id);

      // Name input
      const nameInput = document.createElement('input');
      nameInput.type = 'text';
      nameInput.className = 'sb-name-input';
      nameInput.placeholder = 'column_name';
      nameInput.value = name || '';
      row.appendChild(nameInput);

      // Equals sign
      const eqSign = document.createElement('span');
      eqSign.className = 'sb-eq-sign';
      eqSign.textContent = '=';
      row.appendChild(eqSign);

      // Expression editor
      const codeDiv = document.createElement('div');
      codeDiv.className = 'blockr-row-content sb-expr-code';
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

      const exprInput = Blockr.Input.create(codeDiv, {
        value: value || '',
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

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => this._removeSummary(id));
      row.appendChild(rmBtn);

      this.listEl.appendChild(row);
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
        const btn = s.rowEl?.querySelector('.blockr-row-remove');
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
    }

    _compose() {
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

    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

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
    _funcLabel(func) {
      if (!func) return func;
      // Check if func is a namespaced value that has a label
      for (const [label, value] of Object.entries(this._funcMap)) {
        if (value === func) return label;
      }
      return func;
    }

    updateFunctions(funcs) {
      if (!Array.isArray(funcs) || funcs.length === 0) return;

      // funcs is an array of {value, label} objects from R
      // Build the display list and mapping
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
      this._autoSubmit();
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.summarize-block-container'),
    getId: (el) => el.id || null,
    getValue: (el) => el._block?.getValue() ?? null,
    setValue: (el, value) => el._block?.setState(value),
    subscribe: (el, callback) => {
      if (el._block) el._block._callback = () => callback(true);
    },
    unsubscribe: (el) => {
      if (el._block) el._block._callback = null;
    },
    initialize: (el) => {
      el._block = new SummarizeBlock(el);
      if (el._pendingFunctions) {
        el._block.updateFunctions(el._pendingFunctions);
        delete el._pendingFunctions;
      }
      if (el._pendingColumns) {
        el._block.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
      if (el._pendingState) {
        el._block.setState(el._pendingState);
        delete el._pendingState;
      }
    },
    receiveMessage: (el, data) => {
      if (data.state) el._block?.setState(data.state);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.summarize');

  // Available functions handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('summarize-functions', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.updateFunctions(msg.functions);
    } else if (el) {
      el._pendingFunctions = msg.functions;
    } else {
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) { el2._block.updateFunctions(msg.functions); clearInterval(t); }
        else if (el2) { el2._pendingFunctions = msg.functions; clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('summarize-columns', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.updateColumns(msg.columns);
    } else if (el) {
      el._pendingColumns = msg.columns;
    } else {
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) { el2._block.updateColumns(msg.columns); clearInterval(t); }
        else if (el2) { el2._pendingColumns = msg.columns; clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });

  // External control state update handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('summarize-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state, true);
    } else if (el) {
      el._pendingState = msg.state;
    } else {
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) { el2._block.setState(msg.state, true); clearInterval(t); }
        else if (el2) { el2._pendingState = msg.state; clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });
})();
