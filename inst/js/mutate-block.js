/**
 * MutateBlock — JS-driven mutate block input binding.
 *
 * Each row: [name_input] = [Blockr.Input expression] [confirm] [remove]
 * Expression-only mode with confirm-on-Enter pattern.
 *
 * Depends on: blockr-core.js, blockr-input.js
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

  class MutateBlock {
    constructor(el) {
      this.el = el;
      this.rows = [];
      this.nextId = 1;
      this.columnNames = [];
      this.byValues = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._bySelect = null;

      this._buildDOM();
      this._addRow('', '');
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'mb-card';
      this.el.appendChild(this.card);

      this.listEl = document.createElement('div');
      this.listEl.className = 'mb-rows';
      this.card.appendChild(this.listEl);

      // Add row footer
      const addRow = document.createElement('div');
      addRow.className = 'blockr-add-row';

      const addLink = document.createElement('span');
      addLink.className = 'blockr-add-link';
      addLink.innerHTML = `<span class="blockr-add-icon">${Blockr.icons.plus}</span> Add column`;
      addLink.addEventListener('click', () => this._addRow('', ''));
      addRow.appendChild(addLink);

      this.card.appendChild(addRow);

      // Group by section (below the card, like summarize)
      const bySection = document.createElement('div');
      bySection.className = 'mb-by-section';

      const byLabel = document.createElement('span');
      byLabel.className = 'blockr-label';
      byLabel.textContent = 'Group by:';
      bySection.appendChild(byLabel);

      const byWrap = document.createElement('div');
      byWrap.className = 'mb-by-wrap';
      bySection.appendChild(byWrap);

      this._bySelect = Blockr.Select.multi(byWrap, {
        options: this.columnNames,
        selected: [],
        placeholder: 'Select grouping columns\u2026',
        reorderable: true,
        onChange: (value) => {
          this.byValues = value || [];
          this._autoSubmit();
        }
      });
      this._bySelect.el.classList.add('blockr-select--bordered');

      this.el.appendChild(bySection);
    }

    _addRow(name, expr) {
      const id = this.nextId++;

      const row = document.createElement('div');
      row.className = 'blockr-row';
      row.setAttribute('data-row-id', id);

      // Name input
      const nameWrap = document.createElement('div');
      nameWrap.className = 'mb-name-wrap';
      const nameInput = document.createElement('input');
      nameInput.type = 'text';
      nameInput.className = 'mb-name-input';
      nameInput.placeholder = 'column_name';
      nameInput.value = name || '';
      nameWrap.appendChild(nameInput);
      row.appendChild(nameWrap);

      // Equals separator
      const eqSep = document.createElement('span');
      eqSep.className = 'mb-eq-sep';
      eqSep.textContent = '=';
      row.appendChild(eqSep);

      // Expression editor area
      const codeDiv = document.createElement('div');
      codeDiv.className = 'blockr-row-content mb-expr-code';
      row.appendChild(codeDiv);

      // Confirm button
      const confirmBtn = document.createElement('button');
      confirmBtn.className = 'blockr-expr-confirm';
      confirmBtn.type = 'button';
      confirmBtn.innerHTML = 'Enter \u21B5';
      confirmBtn.title = 'Apply expression';

      const rowData = {
        id,
        nameInput,
        exprInput: null,
        confirmBtn,
        rowEl: row
      };

      const doConfirm = () => {
        confirmBtn.classList.add('confirmed');
        confirmBtn.innerHTML = Blockr.icons.confirm;
        this._submit();
      };
      confirmBtn.addEventListener('click', doConfirm);

      // Blockr.Input — on change reset confirm state; Enter confirms
      const exprInput = Blockr.Input.create(codeDiv, {
        value: expr,
        columns: this.columnNames,
        categories: defaultCategories,
        placeholder: 'R expression\u2026',
        onChange: () => {
          confirmBtn.classList.remove('confirmed');
          confirmBtn.innerHTML = 'Enter \u21B5';
        },
        onConfirm: () => doConfirm()
      });
      rowData.exprInput = exprInput;
      row.appendChild(confirmBtn);

      // Name input: debounced auto-submit on change (300ms)
      let nameTimer = null;
      nameInput.addEventListener('input', () => {
        clearTimeout(nameTimer);
        nameTimer = setTimeout(() => this._submit(), 300);
      });

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => {
        this._removeRow(id);
        this._submit();
      });
      row.appendChild(rmBtn);

      this.listEl.appendChild(row);
      this.rows.push(rowData);
      this._updateUI();
    }

    _removeRow(id) {
      if (this.rows.length <= 1) return;

      const idx = this.rows.findIndex(r => r.id === id);
      if (idx < 0) return;

      const rowData = this.rows[idx];
      rowData.exprInput?.destroy();
      rowData.rowEl?.parentNode?.removeChild(rowData.rowEl);
      this.rows.splice(idx, 1);
      this._updateUI();
    }

    _updateUI() {
      const single = this.rows.length <= 1;
      for (const r of this.rows) {
        const btn = r.rowEl?.querySelector('.blockr-row-remove');
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
    }

    _compose() {
      const mutations = [];
      for (const r of this.rows) {
        const name = (r.nameInput.value || '').trim();
        const expr = r.exprInput ? r.exprInput.getValue() : '';
        mutations.push({ name, expr });
      }
      return { mutations, by: this.byValues || [] };
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
      // Clear existing rows
      while (this.rows.length > 0) {
        const r = this.rows[0];
        r.exprInput?.destroy();
        r.rowEl?.parentNode?.removeChild(r.rowEl);
        this.rows.splice(0, 1);
      }

      // Rebuild from state
      const mutations = state?.mutations || [];
      if (mutations.length === 0) {
        this._addRow('', '');
      } else {
        for (const m of mutations) {
          this._addRow(m.name || '', m.expr || '');
        }
        // Mark all confirm buttons as confirmed
        for (const r of this.rows) {
          r.confirmBtn?.classList.add('confirmed');
          if (r.confirmBtn) r.confirmBtn.innerHTML = Blockr.icons.confirm;
        }
        if (!silent) this._submit();
      }
      // Update group by (ensure array — R may send scalar string)
      const by = state?.by || [];
      this.byValues = Array.isArray(by) ? by.slice() : [by];
      if (this._bySelect) {
        this._bySelect.setOptions(this.columnNames, this.byValues);
      }

      this._updateUI();
    }

    updateColumns(cols) {
      this.columnNames = cols || [];
      for (const r of this.rows) {
        r.exprInput?.setColumns(this.columnNames);
      }
      if (this._bySelect) {
        this._bySelect.setOptions(this.columnNames, this.byValues);
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.mutate-block-container'),
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
      el._block = new MutateBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.mutate');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('mutate-columns', (msg) => {
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

  // Set initial mutations handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('mutate-set-mutations', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState({ mutations: msg.mutations });
    } else if (el) {
      el._pendingState = { mutations: msg.mutations };
    } else {
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) { el2._block.setState({ mutations: msg.mutations }); clearInterval(t); }
        else if (el2) { el2._pendingState = { mutations: msg.mutations }; clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });

  // External control state update handler (global — dispatches by msg.id)
  // silent=true: R already has the state, don't echo back via _submit()
  Shiny.addCustomMessageHandler('mutate-block-update', (msg) => {
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
        if (el2?._block) { el2._block.setState(msg.state); clearInterval(t); }
        else if (el2) { el2._pendingState = msg.state; clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });
})();
