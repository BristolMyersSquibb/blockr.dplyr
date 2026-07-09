// @ts-check
/**
 * MutateBlock — JS-driven mutate block input binding.
 *
 * Each row: [name_input] = [Blockr.Input expression] [confirm] [remove]
 * Expression-only mode with confirm-on-Enter pattern.
 *
 * Depends on: blockr-core.js, blockr-input.js
 */

/**
 * One mutation, as exchanged with R (R: make_mutate_expr() `mutations`
 * in R/expr-builders.R). Rows with a blank name or expr are filtered out
 * on the R side.
 * @typedef {Object} MutateMutation
 * @property {string} name New (or overwritten) column name.
 * @property {string} expr Free-form R expression string.
 */

/**
 * Block state: what _compose() returns and setState() receives
 * (R: make_mutate_expr(mutations, by)).
 * @typedef {Object} MutateState
 * @property {MutateMutation[]} mutations
 * @property {string | string[]} by Grouping columns for `.by` (jsonlite
 *   may deliver a single column as a scalar string).
 */

/**
 * Internal row record: the mutation's UI handles.
 * @typedef {Object} MutateRow
 * @property {number} id
 * @property {HTMLInputElement} nameInput
 * @property {BlockrInputHandle | null} exprInput
 * @property {HTMLButtonElement} confirmBtn
 * @property {HTMLDivElement} rowEl
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
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {MutateRow[]} */
      this.rows = [];
      this.nextId = 1;
      /** @type {string[]} */
      this.columnNames = [];
      /** @type {{ value: string, label: string }[]} */
      this.columnOptions = [];
      /** @type {Record<string, BlockrPickerColumn>} */
      this.columnMeta = {};
      /** @type {string[]} */
      this.byValues = [];
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {BlockrSelectMultiHandle | null} */
      this._bySelect = null;

      this._buildDOM();
      this._addRow('', '');
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

      this._bySelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).multi(byWrap, {
        options: this.columnOptions,
        selected: [],
        placeholder: 'Select grouping columns\u2026',
        reorderable: true,
        onChange: (value) => {
          this.byValues = value || [];
          this._submit();
        }
      });
      this._bySelect.el.classList.add('blockr-select--bordered');

      this.el.appendChild(bySection);
    }

    /**
     * @param {string} name
     * @param {string} expr
     */
    _addRow(name, expr) {
      const id = this.nextId++;

      const row = document.createElement('div');
      row.className = 'blockr-row';
      row.setAttribute('data-row-id', /** @type {string} */ (/** @type {*} */ (id)));

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
      confirmBtn.innerHTML = 'Enter <span class="blockr-kbd">\u21B5</span>';
      confirmBtn.title = 'Apply expression';

      /** @type {MutateRow} */
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
      const exprInput = /** @type {BlockrInputStatic} */ (Blockr.Input).create(codeDiv, {
        value: expr,
        columns: this.columnNames,
        categories: defaultCategories,
        placeholder: 'R expression\u2026',
        onChange: () => {
          confirmBtn.classList.remove('confirmed');
          confirmBtn.innerHTML = 'Enter <span class="blockr-kbd">\u21B5</span>';
        },
        onConfirm: () => doConfirm()
      });
      rowData.exprInput = exprInput;
      row.appendChild(confirmBtn);

      // Name input joins the row's confirm cycle (§5.5): typing re-arms
      // the chip, Enter or blur commits the row (only when dirty — a plain
      // focus pass-through must not fire a submit).
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
      rmBtn.addEventListener('click', () => {
        this._removeRow(id);
        this._submit();
      });
      row.appendChild(rmBtn);

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(row);
      this.rows.push(rowData);
      this._updateUI();
    }

    /** @param {number} id */
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
        const btn = /** @type {HTMLElement | null | undefined} */ (r.rowEl?.querySelector('.blockr-row-remove'));
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
    }

    /** @returns {MutateState} */
    _compose() {
      /** @type {MutateMutation[]} */
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

    /** @returns {MutateState | null} */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {Partial<MutateState> | null | undefined} state
     * @param {boolean} [silent] Suppress the auto-submit after restoring.
     */
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
        this._bySelect.setOptions(this.columnOptions, this.byValues);
      }

      this._updateUI();
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
      for (const r of this.rows) {
        r.exprInput?.setColumns(this.columnNames);
      }
      if (this._bySelect) {
        this._bySelect.setOptions(this.columnOptions, this.byValues);
      }
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'mutate',
    Block: MutateBlock,
    messages: {
      'mutate-columns': (block, msg) => block.updateColumns(msg.columns),
      'mutate-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
