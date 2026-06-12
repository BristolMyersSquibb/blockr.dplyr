// @ts-check
/**
 * RenameBlock — JS-driven column rename block input binding.
 *
 * Dynamic rows: each row has a column picker (single select), an arrow
 * separator, a text input for the new name, and a remove button.
 * Footer has an "Add rename" link. Auto-submits on change (300ms debounce
 * for text input).
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  /**
   * Rename-block state, mirroring make_rename_expr() in R/expr-builders.R:
   * `renames` maps new_name -> old_name.
   * @typedef {Object} RenameBlockState
   * @property {Record<string, string>} [renames]
   */

  /**
   * Internal per-row UI record.
   * @typedef {Object} RenameRow
   * @property {number} id
   * @property {string} oldCol
   * @property {string} newName
   * @property {BlockrSelectSingleHandle | null} _colSelect
   * @property {HTMLDivElement | null} rowEl
   * @property {HTMLInputElement} [_nameInput]
   */

  class RenameBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {RenameRow[]} */
      this.rows = [];
      this.nextId = 1;
      /** @type {string[]} */
      this.columnNames = [];
      /** @type {Array<{value: string, label: string}>} */
      this.columnOptions = [];
      /** @type {Record<string, BlockrColumnSummary>} */
      this.columnMeta = {};
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {ReturnType<typeof setTimeout> | null} */
      this._debounceTimer = null;

      this._buildDOM();
      this._addRow(null, '');
    }

    _autoSubmit() {
      clearTimeout(/** @type {ReturnType<typeof setTimeout>} */ (this._debounceTimer));
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'rb-card';
      this.el.appendChild(this.card);

      this.listEl = document.createElement('div');
      this.listEl.className = 'rb-rows';
      this.card.appendChild(this.listEl);

      // Add row bar
      const addRow = document.createElement('div');
      addRow.className = 'blockr-add-row';

      const addLink = document.createElement('span');
      addLink.className = 'blockr-add-link';
      addLink.innerHTML = `<span class="blockr-add-icon">${Blockr.icons.plus}</span> Add rename`;
      addLink.addEventListener('click', () => this._addRow(null, ''));
      addRow.appendChild(addLink);

      this.card.appendChild(addRow);
    }

    /**
     * @param {string | null} oldCol
     * @param {string} newName
     */
    _addRow(oldCol, newName) {
      const id = this.nextId++;
      /** @type {RenameRow} */
      const row = {
        id,
        oldCol: oldCol || '',
        newName: newName || '',
        _colSelect: null,
        rowEl: null
      };

      const rowEl = document.createElement('div');
      rowEl.className = 'blockr-row';
      rowEl.setAttribute('data-row-id', /** @type {any} */ (id));
      row.rowEl = rowEl;

      // Old column dropdown
      const colDiv = document.createElement('div');
      colDiv.className = 'rb-col-wrap';
      rowEl.appendChild(colDiv);
      row._colSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select).single(colDiv, {
        options: this.columnOptions,
        selected: /** @type {string | undefined} */ (oldCol),
        placeholder: 'Column\u2026',
        onChange: (value) => {
          row.oldCol = value;
          // Auto-populate new name if empty
          if (!row.newName && value) {
            row.newName = value;
            /** @type {HTMLInputElement} */ (row._nameInput).value = value;
          }
          this._autoSubmit();
        }
      });

      // Arrow separator
      const arrow = document.createElement('span');
      arrow.className = 'rb-arrow';
      arrow.textContent = '\u2192';
      rowEl.appendChild(arrow);

      // New name text input
      const nameInput = document.createElement('input');
      nameInput.type = 'text';
      nameInput.className = 'rb-name-input';
      nameInput.placeholder = 'New name\u2026';
      nameInput.value = newName || '';
      nameInput.addEventListener('input', () => {
        row.newName = nameInput.value;
        this._autoSubmit();
      });
      row._nameInput = nameInput;
      rowEl.appendChild(nameInput);

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => {
        this._removeRow(id);
        this._autoSubmit();
      });
      rowEl.appendChild(rmBtn);

      // Sync row.oldCol with the select's actual value — the select may
      // auto-pick the first option when oldCol is null/empty.
      row.oldCol = row._colSelect.getValue() || '';

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(rowEl);
      this.rows.push(row);
      this._updateUI();
    }

    /** @param {number} id */
    _removeRow(id) {
      if (this.rows.length <= 1) return;

      const idx = this.rows.findIndex(r => r.id === id);
      if (idx < 0) return;

      const row = this.rows[idx];
      row._colSelect?.destroy();
      row.rowEl?.parentNode?.removeChild(row.rowEl);
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

    /** @returns {RenameBlockState} */
    _compose() {
      /** @type {Record<string, string>} */
      const renames = {};
      for (const r of this.rows) {
        if (!r.oldCol || !r.newName) continue;
        // renames: { new_name: old_name }
        renames[r.newName] = r.oldCol;
      }
      return { renames };
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
     * @param {RenameBlockState | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      // Clear existing rows
      while (this.rows.length > 0) {
        const row = this.rows[0];
        row._colSelect?.destroy();
        row.rowEl?.parentNode?.removeChild(row.rowEl);
        this.rows.splice(0, 1);
      }

      // Rebuild from state
      const renames = state?.renames || {};
      const entries = Object.entries(renames);
      if (entries.length === 0) {
        this._addRow(null, '');
      } else {
        for (const [newName, oldCol] of entries) {
          this._addRow(oldCol, newName);
        }
      }
      this._updateUI();
    }

    /** @param {BlockrColumnSummary[] | null | undefined} meta */
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
        if (r._colSelect) {
          const current = r._colSelect.getValue();
          r._colSelect.setOptions(this.columnOptions, current);
          r.oldCol = r._colSelect.getValue();
        }
      }
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'rename',
    Block: RenameBlock,
    messages: {
      'rename-columns': (block, msg) => block.updateColumns(msg.columns),
      'rename-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
