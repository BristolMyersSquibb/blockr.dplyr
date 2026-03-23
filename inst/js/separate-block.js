/**
 * SeparateBlock — JS-driven tidyr::separate block input binding.
 *
 * Main UI: source column single-select (bordered), into text input
 * (comma-separated names), separator text input.
 * Gear popover: remove toggle, convert toggle, extra text input, fill text input.
 * Auto-submits on any change (300ms debounce).
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  class SeparateBlock {
    constructor(el) {
      this.el = el;
      this.col = '';
      this.into = [];
      this.sep = '[^[:alnum:]]+';
      this.remove = true;
      this.convert = false;
      this.extra = 'warn';
      this.fill = 'warn';
      this.columnNames = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._colSelect = null;
      this._popoverOpen = false;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'spb-card';
      this.el.appendChild(this.card);

      // Gear header (top-right)
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = Blockr.icons.gear;
      this.gearBtn.title = 'Advanced settings';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // Source column picker (bordered)
      const colWrap = document.createElement('div');
      colWrap.className = 'spb-col-wrap blockr-select--bordered';
      const colLabel = document.createElement('label');
      colLabel.className = 'blockr-label';
      colLabel.textContent = 'Column';
      colWrap.appendChild(colLabel);
      this._colSelect = Blockr.Select.single(colWrap, {
        options: this.columnNames,
        selected: null,
        placeholder: 'Source column\u2026',
        onChange: (value) => {
          this.col = value;
          this._autoSubmit();
        }
      });
      this.card.appendChild(colWrap);

      // Text inputs row: into + sep + gear
      const inputRow = document.createElement('div');
      inputRow.className = 'spb-input-row';

      // into (comma-separated names)
      const intoWrap = document.createElement('div');
      intoWrap.className = 'spb-field';
      const intoLabel = document.createElement('label');
      intoLabel.className = 'blockr-label';
      intoLabel.textContent = 'Into (comma-separated)';
      intoWrap.appendChild(intoLabel);
      this._intoInput = document.createElement('input');
      this._intoInput.type = 'text';
      this._intoInput.className = 'blockr-text-input spb-text-input';
      this._intoInput.value = '';
      this._intoInput.placeholder = 'col1, col2, col3';
      this._intoInput.addEventListener('input', () => {
        this.into = this._parseInto(this._intoInput.value);
        this._autoSubmit();
      });
      intoWrap.appendChild(this._intoInput);
      inputRow.appendChild(intoWrap);

      // sep
      const sepWrap = document.createElement('div');
      sepWrap.className = 'spb-field spb-field-narrow';
      const sepLabel = document.createElement('label');
      sepLabel.className = 'blockr-label';
      sepLabel.textContent = 'Separator';
      sepWrap.appendChild(sepLabel);
      this._sepInput = document.createElement('input');
      this._sepInput.type = 'text';
      this._sepInput.className = 'blockr-text-input spb-text-input';
      this._sepInput.value = this.sep;
      this._sepInput.placeholder = '[^[:alnum:]]+';
      this._sepInput.addEventListener('input', () => {
        this.sep = this._sepInput.value;
        this._autoSubmit();
      });
      sepWrap.appendChild(this._sepInput);
      inputRow.appendChild(sepWrap);

      this.card.appendChild(inputRow);

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

    // --- Settings popover ---

    _buildPopover() {
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-popover';
      this.popoverEl.style.display = 'none';

      // remove toggle
      const removeRow = document.createElement('div');
      removeRow.className = 'blockr-popover-row';
      const removeLabel = document.createElement('label');
      removeLabel.className = 'blockr-popover-label';
      removeLabel.textContent = 'Remove:';
      removeRow.appendChild(removeLabel);
      this._removeToggle = document.createElement('button');
      this._removeToggle.type = 'button';
      this._removeToggle.className = 'blockr-pill spb-popover-toggle spb-popover-toggle-active';
      this._removeToggle.textContent = 'on';
      this._removeToggle.addEventListener('click', () => {
        this.remove = !this.remove;
        this._removeToggle.textContent = this.remove ? 'on' : 'off';
        this._removeToggle.classList.toggle('spb-popover-toggle-active', this.remove);
        this._autoSubmit();
      });
      removeRow.appendChild(this._removeToggle);
      this.popoverEl.appendChild(removeRow);

      // convert toggle
      const convertRow = document.createElement('div');
      convertRow.className = 'blockr-popover-row';
      const convertLabel = document.createElement('label');
      convertLabel.className = 'blockr-popover-label';
      convertLabel.textContent = 'Convert:';
      convertRow.appendChild(convertLabel);
      this._convertToggle = document.createElement('button');
      this._convertToggle.type = 'button';
      this._convertToggle.className = 'blockr-pill spb-popover-toggle';
      this._convertToggle.textContent = 'off';
      this._convertToggle.addEventListener('click', () => {
        this.convert = !this.convert;
        this._convertToggle.textContent = this.convert ? 'on' : 'off';
        this._convertToggle.classList.toggle('spb-popover-toggle-active', this.convert);
        this._autoSubmit();
      });
      convertRow.appendChild(this._convertToggle);
      this.popoverEl.appendChild(convertRow);

      // extra text input
      const extraRow = document.createElement('div');
      extraRow.className = 'blockr-popover-row';
      const extraLabel = document.createElement('label');
      extraLabel.className = 'blockr-popover-label';
      extraLabel.textContent = 'Extra:';
      extraRow.appendChild(extraLabel);
      this._extraInput = document.createElement('input');
      this._extraInput.type = 'text';
      this._extraInput.className = 'blockr-popover-input';
      this._extraInput.value = this.extra;
      this._extraInput.placeholder = 'warn';
      this._extraInput.addEventListener('input', () => {
        this.extra = this._extraInput.value || 'warn';
        this._autoSubmit();
      });
      extraRow.appendChild(this._extraInput);
      this.popoverEl.appendChild(extraRow);

      // fill text input
      const fillRow = document.createElement('div');
      fillRow.className = 'blockr-popover-row';
      const fillLabel = document.createElement('label');
      fillLabel.className = 'blockr-popover-label';
      fillLabel.textContent = 'Fill:';
      fillRow.appendChild(fillLabel);
      this._fillInput = document.createElement('input');
      this._fillInput.type = 'text';
      this._fillInput.className = 'blockr-popover-input';
      this._fillInput.value = this.fill;
      this._fillInput.placeholder = 'warn';
      this._fillInput.addEventListener('input', () => {
        this.fill = this._fillInput.value || 'warn';
        this._autoSubmit();
      });
      fillRow.appendChild(this._fillInput);
      this.popoverEl.appendChild(fillRow);

      this.card.appendChild(this.popoverEl);
    }

    _togglePopover() {
      if (this._popoverOpen) {
        this._closePopover();
      } else {
        this._openPopover();
      }
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

    _parseInto(text) {
      return text.split(',')
        .map(s => s.trim())
        .filter(s => s.length > 0);
    }

    _compose() {
      return {
        col: this.col,
        into: this.into.slice(),
        sep: this.sep,
        remove: this.remove,
        convert: this.convert,
        extra: this.extra,
        fill: this.fill
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
      this.col = state?.col || '';
      this.into = (state?.into || []).slice();
      this.sep = state?.sep ?? '[^[:alnum:]]+';
      this.remove = state?.remove !== false;
      this.convert = !!state?.convert;
      this.extra = state?.extra || 'warn';
      this.fill = state?.fill || 'warn';

      // Update column select
      if (this._colSelect) {
        this._colSelect.setOptions(this.columnNames, this.col || null);
      }

      // Update text inputs
      this._intoInput.value = this.into.join(', ');
      this._sepInput.value = this.sep;

      // Update popover controls
      this._removeToggle.textContent = this.remove ? 'on' : 'off';
      this._removeToggle.classList.toggle('spb-popover-toggle-active', this.remove);
      this._convertToggle.textContent = this.convert ? 'on' : 'off';
      this._convertToggle.classList.toggle('spb-popover-toggle-active', this.convert);
      this._extraInput.value = this.extra;
      this._fillInput.value = this.fill;
    }

    updateColumns(names) {
      this.columnNames = names || [];
      if (this._colSelect) {
        const current = this._colSelect.getValue();
        this._colSelect.setOptions(this.columnNames, current);
        this.col = this._colSelect.getValue();
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.separate-block-container'),
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
      el._block = new SeparateBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.separate');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('separate-columns', (msg) => {
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
  Shiny.addCustomMessageHandler('separate-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
