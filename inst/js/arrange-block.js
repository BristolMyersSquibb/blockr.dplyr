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
        options: this.columnNames,
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

    setState(state) {
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

    updateColumns(names) {
      this.columnNames = names || [];
      for (const r of this.rows) {
        if (r._colSelect) {
          const current = r._colSelect.getValue();
          r._colSelect.setOptions(this.columnNames, current);
          r.column = r._colSelect.getValue();
        }
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.arrange-block-container'),
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
      el._block = new ArrangeBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.arrange');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('arrange-columns', (msg) => {
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
  Shiny.addCustomMessageHandler('arrange-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
