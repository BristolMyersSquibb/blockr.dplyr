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

  class RenameBlock {
    constructor(el) {
      this.el = el;
      this.rows = [];
      this.nextId = 1;
      this.columnNames = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;

      this._buildDOM();
      this._addRow(null, '');
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
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

    _addRow(oldCol, newName) {
      const id = this.nextId++;
      const row = {
        id,
        oldCol: oldCol || '',
        newName: newName || '',
        _colSelect: null,
        rowEl: null
      };

      const rowEl = document.createElement('div');
      rowEl.className = 'blockr-row';
      rowEl.setAttribute('data-row-id', id);
      row.rowEl = rowEl;

      // Old column dropdown
      const colDiv = document.createElement('div');
      colDiv.className = 'rb-col-wrap';
      rowEl.appendChild(colDiv);
      row._colSelect = Blockr.Select.single(colDiv, {
        options: this.columnNames,
        selected: oldCol,
        placeholder: 'Column\u2026',
        onChange: (value) => {
          row.oldCol = value;
          // Auto-populate new name if empty
          if (!row.newName && value) {
            row.newName = value;
            row._nameInput.value = value;
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

    updateColumns(names) {
      this.columnNames = names || [];
      for (const r of this.rows) {
        if (r._colSelect) {
          const current = r._colSelect.getValue();
          r._colSelect.setOptions(this.columnNames, current);
          r.oldCol = r._colSelect.getValue();
        }
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.rename-block-container'),
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
      el._block = new RenameBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.rename');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('rename-columns', (msg) => {
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
  Shiny.addCustomMessageHandler('rename-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state, true);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
