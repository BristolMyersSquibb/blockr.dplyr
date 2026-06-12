/**
 * ArrangeBlock — JS-driven arrange/sort block input binding.
 *
 * Dynamic rows: each row has a column picker (single select) and a
 * direction toggle (asc/desc). Footer has an "Add sort" link.
 * Auto-submits on any change.
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  class ArrangeBlock {
    constructor(el) {
      this.el = el;
      this.rows = [];
      this.nextId = 1;
      this.columnNames = [];
      this.columnOptions = [];
      this.columnMeta = {};
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;

      this._buildDOM();
      this._addRow(null, 'asc');
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'ab-card';
      this.el.appendChild(this.card);

      this.listEl = document.createElement('div');
      this.listEl.className = 'ab-rows';
      this.card.appendChild(this.listEl);

      // Add row bar
      const addRow = document.createElement('div');
      addRow.className = 'blockr-add-row';

      const addLink = document.createElement('span');
      addLink.className = 'blockr-add-link';
      addLink.innerHTML = `<span class="blockr-add-icon">${Blockr.icons.plus}</span> Add sort`;
      addLink.addEventListener('click', () => this._addRow(null, 'asc'));
      addRow.appendChild(addLink);

      this.card.appendChild(addRow);
    }

    _addRow(column, direction) {
      const id = this.nextId++;
      const row = {
        id,
        column: column || '',
        direction: direction || 'asc',
        _colSelect: null,
        rowEl: null
      };

      const rowEl = document.createElement('div');
      rowEl.className = 'blockr-row';
      rowEl.setAttribute('data-row-id', id);
      row.rowEl = rowEl;

      // Column dropdown
      const colDiv = document.createElement('div');
      colDiv.className = 'ab-col-wrap';
      rowEl.appendChild(colDiv);
      row._colSelect = Blockr.Select.single(colDiv, {
        options: this.columnOptions,
        selected: column,
        placeholder: 'Column\u2026',
        onChange: (value) => {
          row.column = value;
          this._autoSubmit();
        }
      });

      // Direction toggle pill
      const dirBtn = document.createElement('button');
      dirBtn.type = 'button';
      dirBtn.className = 'blockr-pill ab-dir-btn';
      dirBtn.textContent = direction === 'desc' ? 'desc' : 'asc';
      if (direction === 'desc') dirBtn.classList.add('ab-dir-desc');
      dirBtn.title = 'Toggle between ascending (A\u2192Z, 1\u21929) and descending (Z\u2192A, 9\u21921) sort order';
      dirBtn.addEventListener('click', () => {
        row.direction = row.direction === 'asc' ? 'desc' : 'asc';
        dirBtn.textContent = row.direction;
        dirBtn.classList.toggle('ab-dir-desc', row.direction === 'desc');
        this._autoSubmit();
      });
      row._dirBtn = dirBtn;
      rowEl.appendChild(dirBtn);

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

      this.listEl.appendChild(rowEl);
      this.rows.push(row);
      this._updateUI();
    }

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
        const btn = r.rowEl?.querySelector('.blockr-row-remove');
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
    }

    _compose() {
      const columns = [];
      for (const r of this.rows) {
        if (!r.column) continue;
        columns.push({ column: r.column, direction: r.direction });
      }
      return { columns };
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
        const row = this.rows[0];
        row._colSelect?.destroy();
        row.rowEl?.parentNode?.removeChild(row.rowEl);
        this.rows.splice(0, 1);
      }

      // Rebuild from state
      const columns = state?.columns || [];
      if (columns.length === 0) {
        this._addRow(null, 'asc');
      } else {
        for (const col of columns) {
          this._addRow(col.column, col.direction || 'asc');
        }
      }
      this._updateUI();
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
      for (const r of this.rows) {
        if (r._colSelect) {
          const current = r._colSelect.getValue();
          r._colSelect.setOptions(this.columnOptions, current);
          r.column = r._colSelect.getValue();
        }
      }
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'arrange',
    Block: ArrangeBlock,
    messages: {
      'arrange-columns': (block, msg) => block.updateColumns(msg.columns),
      'arrange-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
