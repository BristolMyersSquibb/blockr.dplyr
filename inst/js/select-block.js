/**
 * SelectBlock — JS-driven column select block input binding.
 *
 * Multi-select column picker with reorderable tags, exclude toggle,
 * and distinct option in gear popover. Auto-submits on any change (300ms debounce).
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  class SelectBlock {
    constructor(el) {
      this.el = el;
      this.columns = [];
      this.exclude = false;
      this.distinct = false;
      this.columnNames = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._multiSelect = null;
      this._popoverOpen = false;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'sb-card';
      this.el.appendChild(this.card);

      // Gear header (top-right)
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = Blockr.icons.gear;
      this.gearBtn.title = 'Options';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // Column picker
      const pickerWrap = document.createElement('div');
      pickerWrap.className = 'sb-picker-wrap';
      this.card.appendChild(pickerWrap);

      this._multiSelect = Blockr.Select.multi(pickerWrap, {
        options: this.columnNames,
        selected: [],
        placeholder: 'Select columns\u2026',
        reorderable: true,
        onChange: (selected) => {
          this.columns = selected;
          this._autoSubmit();
        }
      });
      this._multiSelect.el.classList.add('blockr-select--bordered');

      // Toggle bar: include/exclude pill
      const toggleBar = document.createElement('div');
      toggleBar.className = 'blockr-add-row';

      this.excludeBtn = document.createElement('button');
      this.excludeBtn.type = 'button';
      this.excludeBtn.className = 'blockr-pill sb-toggle';
      this.excludeBtn.textContent = 'include';
      this.excludeBtn.title = 'Toggle include / exclude';
      this.excludeBtn.addEventListener('click', () => {
        this.exclude = !this.exclude;
        this.excludeBtn.textContent = this.exclude ? 'exclude' : 'include';
        this.excludeBtn.classList.toggle('sb-toggle-active', this.exclude);
        this._autoSubmit();
      });
      toggleBar.appendChild(this.excludeBtn);

      this.card.appendChild(toggleBar);

      // Settings popover
      this._buildPopover();

      // Close popover on outside click
      document.addEventListener('click', (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(e.target) &&
            !this.gearBtn.contains(e.target)) {
          this._closePopover();
        }
      });
    }

    _buildPopover() {
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-popover';
      this.popoverEl.style.display = 'none';

      const row = document.createElement('div');
      row.className = 'blockr-popover-row';

      const label = document.createElement('label');
      label.className = 'blockr-popover-label';
      label.textContent = 'Distinct:';
      row.appendChild(label);

      this._distinctToggle = document.createElement('button');
      this._distinctToggle.type = 'button';
      this._distinctToggle.className = 'blockr-pill blockr-popover-toggle';
      this._distinctToggle.textContent = 'off';
      this._distinctToggle.addEventListener('click', () => {
        this.distinct = !this.distinct;
        this._distinctToggle.textContent = this.distinct ? 'on' : 'off';
        this._distinctToggle.classList.toggle('blockr-popover-toggle-active', this.distinct);
        this._autoSubmit();
      });
      row.appendChild(this._distinctToggle);

      this.popoverEl.appendChild(row);
      this.card.appendChild(this.popoverEl);
    }

    _togglePopover() {
      this._popoverOpen ? this._closePopover() : this._openPopover();
    }

    _openPopover() {
      this.popoverEl.style.display = 'block';
      this._popoverOpen = true;
      this.gearBtn.classList.add('blockr-gear-active');
    }

    _closePopover() {
      this.popoverEl.style.display = 'none';
      this._popoverOpen = false;
      this.gearBtn.classList.remove('blockr-gear-active');
    }

    _compose() {
      return {
        columns: this.columns.slice(),
        exclude: this.exclude,
        distinct: this.distinct
      };
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
      this.columns = (state?.columns || []).slice();
      this.exclude = !!state?.exclude;
      this.distinct = !!state?.distinct;

      this.excludeBtn.textContent = this.exclude ? 'exclude' : 'include';
      this.excludeBtn.classList.toggle('sb-toggle-active', this.exclude);
      this._distinctToggle.textContent = this.distinct ? 'on' : 'off';
      this._distinctToggle.classList.toggle('blockr-popover-toggle-active', this.distinct);

      if (this._multiSelect) {
        this._multiSelect.setOptions(this.columnNames, this.columns);
      }
    }

    updateColumns(names) {
      this.columnNames = names || [];
      if (this._multiSelect) {
        this._multiSelect.setOptions(this.columnNames, this.columns);
        this.columns = this._multiSelect.getValue();
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.select-block-container'),
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
      el._block = new SelectBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.select');

  Shiny.addCustomMessageHandler('select-columns', (msg) => {
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

  Shiny.addCustomMessageHandler('select-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
