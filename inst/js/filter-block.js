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

  class FilterBlock {
    constructor(el) {
      this.el = el;
      this.conditions = [];
      this.operator = '&';
      this.nextId = 1;
      this.columnMeta = {};
      this.columnNames = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this.preserveOrder = false;

      this._buildDOM();
      this._addValueRow(null, null);
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
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
        this.opToggle.textContent = this.operator === '&' ? 'AND' : 'OR';
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
        this.preserveBtn.textContent = this.preserveOrder ? 'pick order' : 'data order';
        this.preserveBtn.classList.toggle('fb-preserve-active', this.preserveOrder);
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

    _createOpButton(cond, ops, initialOp) {
      let idx = initialOp
        ? Math.max(0, ops.findIndex(o => o.value === initialOp))
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

    _addValueRow(column, opts) {
      const id = this.nextId++;
      const cond = {
        id,
        filterType: 'none',
        column: column || '',
        op: 'is',
        values: opts?.values || [],
        numValue: opts?.numValue ?? null,
        multiSelect: null,
        exprInput: null,
        rowEl: null
      };

      const row = document.createElement('div');
      row.className = 'blockr-row';
      row.setAttribute('data-cond-id', id);
      cond.rowEl = row;

      // Column dropdown
      const colDiv = document.createElement('div');
      colDiv.className = 'fb-col-wrap';
      row.appendChild(colDiv);
      cond._colSelectize = Blockr.Select.single(colDiv, {
        options: this.columnNames,
        selected: column,
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

      this.listEl.appendChild(row);
      this.conditions.push(cond);
      this._updateUI();

      const activeCol = column || cond._colSelectize.getValue();
      if (activeCol && this.columnMeta[activeCol]) {
        this._onColumnChange(cond, activeCol, opts);
      }
    }

    _onColumnChange(cond, colName, opts) {
      cond.column = colName;
      const meta = this.columnMeta[colName];

      // Restore values/numValue from opts if provided (setState path),
      // otherwise reset to empty (user interaction path)
      cond.values = opts?.values || [];
      cond.numValue = opts?.numValue ?? null;

      if (cond._valueSelectize) { cond._valueSelectize.destroy(); cond._valueSelectize = null; }
      cond._contentDiv.innerHTML = '';
      cond._opBtnSlot.innerHTML = '';

      if (!meta) { cond.filterType = 'none'; return; }

      const isNumeric = meta.type === 'numeric' || meta.type === 'integer';
      cond._meta = meta;
      cond.filterType = isNumeric ? 'numeric' : 'values';

      // Map condition op to operator list value for initial selection
      const ops = isNumeric ? NUM_OPS : CAT_OPS;
      const initialOp = opts?.op || null;
      const btn = this._createOpButton(cond, ops, initialOp);
      cond._opBtnSlot.appendChild(btn);

      this._renderDynamicContent(cond);
    }

    _renderDynamicContent(cond) {
      const container = cond._contentDiv;
      const meta = cond._meta;

      if (cond._valueSelectize) { cond._valueSelectize.destroy(); cond._valueSelectize = null; }
      container.innerHTML = '';

      if (!meta) return;

      const op = cond.op;

      if (op === 'is' || op === 'is not') {
        let allValues;
        if (meta.type === 'numeric' || meta.type === 'integer') {
          allValues = (meta.uniqueValues || []).map(String);
        } else {
          allValues = (meta.values || []).slice();
          if (meta.hasEmpty) allValues.push('<empty>');
        }
        if (meta.hasNA) allValues.push('<NA>');

        cond._valueSelectize = Blockr.Select.multi(container, {
          options: allValues,
          selected: cond.values || [],
          placeholder: 'Select values\u2026',
          reorderable: this.preserveOrder,
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
        if (cond.numValue != null) numInput.value = cond.numValue;
        numInput.addEventListener('input', () => {
          cond.numValue = numInput.value === '' ? null : parseFloat(numInput.value);
          this._autoSubmit();
        });
        container.appendChild(numInput);
      }
    }

    _addExprRow(value) {
      const id = this.nextId++;

      const row = document.createElement('div');
      row.className = 'blockr-row fb-row-expr';
      row.setAttribute('data-cond-id', id);

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

      const exprInput = Blockr.Input.create(codeDiv, {
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

      this.listEl.appendChild(row);
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
      this.opToggle.style.visibility = single ? 'hidden' : 'visible';
      for (const c of this.conditions) {
        const btn = c.rowEl?.querySelector('.blockr-row-remove');
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
    }

    _compose() {
      const conditions = [];
      for (const c of this.conditions) {
        if (!c.column && c.filterType !== 'expr') continue;

        const op = c.op;
        if ((c.filterType === 'values' || c.filterType === 'numeric') && (op === 'is' || op === 'is not')) {
          if (c.values?.length > 0) {
            conditions.push({
              type: 'values', column: c.column, values: c.values,
              mode: op === 'is' ? 'include' : 'exclude'
            });
          }
        } else if (c.filterType === 'numeric' && c.numValue != null) {
          conditions.push({ type: 'numeric', column: c.column, op, value: c.numValue });
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
      this.opToggle.textContent = this.operator === '&' ? 'AND' : 'OR';

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
              op: cond.mode === 'exclude' ? 'is not' : 'is'
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

    updateColumns(meta) {
      this.columnMeta = {};
      this.columnNames = [];
      for (const col of (meta || [])) {
        this.columnMeta[col.name] = col;
        this.columnNames.push(col.name);
      }
      for (const c of this.conditions) {
        if (c._colSelectize) {
          const current = c._colSelectize.getValue();
          c._colSelectize.setOptions(this.columnNames, current);
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
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.filter-block-container'),
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
      el._block = new FilterBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.filter');

  // Per-instance message handlers are registered dynamically.
  // The R server sends messages named "filter-columns-<ns_id>" and
  // "block-update-<ns_id>". We use a single handler prefix pattern
  // via a MutationObserver to register handlers when filter elements
  // appear in the DOM.

  // Column metadata handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('filter-columns', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.updateColumns(msg.columns);
    } else if (el) {
      el._pendingColumns = msg.columns;
    } else {
      // Element not yet in DOM — poll briefly
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
  Shiny.addCustomMessageHandler('filter-block-update', (msg) => {
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
