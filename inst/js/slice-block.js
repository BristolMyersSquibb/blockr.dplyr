/**
 * SliceBlock — JS-driven dplyr::slice_* block input binding.
 *
 * Main UI: [type selector (bordered)] [n input] [gear button]
 * Gear popover: order_by (min/max), weight_by (sample), with_ties (min/max),
 *   replace (sample) — conditionally visible based on type.
 * Below card: "Group by:" label + bordered multi-select (summarize pattern).
 *
 * Depends on: blockr-core.js, blockr-select.js
 */
(() => {
  'use strict';

  const SLICE_TYPES = ['head', 'tail', 'min', 'max', 'sample'];

  class SliceBlock {
    constructor(el) {
      this.el = el;
      this.type = 'head';
      this.n = 5;
      this.prop = null;
      this.order_by = '';
      this.with_ties = true;
      this.weight_by = '';
      this.replace = false;
      this.by = [];
      this.columnNames = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._typeSelect = null;
      this._orderBySelect = null;
      this._weightBySelect = null;
      this._bySelect = null;
      this._popoverOpen = false;

      this._buildDOM();
      this._updatePopoverVisibility();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'slb-card';
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
      this._gearHeader = gearHeader;
      this.card.appendChild(gearHeader);

      // Top row: type + n in a standard blockr-row
      const topRow = document.createElement('div');
      topRow.className = 'blockr-row';

      // Type selector
      const typeWrap = document.createElement('div');
      typeWrap.className = 'slb-type-wrap';
      this._typeSelect = Blockr.Select.single(typeWrap, {
        options: SLICE_TYPES,
        selected: this.type,
        placeholder: 'Type\u2026',
        onChange: (value) => {
          this.type = value;
          this._updatePopoverVisibility();
          this._autoSubmit();
        }
      });
      // No --bordered class needed: the row provides the border
      topRow.appendChild(typeWrap);

      // n input
      const nWrap = document.createElement('div');
      nWrap.className = 'slb-n-wrap';
      this._nInput = document.createElement('input');
      this._nInput.type = 'number';
      this._nInput.className = 'blockr-num-input';
      this._nInput.value = this.n;
      this._nInput.min = '1';
      this._nInput.placeholder = 'n';
      this._nInput.addEventListener('input', () => {
        this._updateNValue();
        this._autoSubmit();
      });
      nWrap.appendChild(this._nInput);
      topRow.appendChild(nWrap);

      // n/% toggle pill
      this._usesProp = false;
      this._nModePill = document.createElement('button');
      this._nModePill.type = 'button';
      this._nModePill.className = 'blockr-pill slb-mode-pill';
      this._nModePill.textContent = 'n';
      this._nModePill.title = 'Toggle between count (n) and proportion (%)';
      this._nModePill.addEventListener('click', () => {
        this._usesProp = !this._usesProp;
        this._nModePill.textContent = this._usesProp ? '%' : 'n';
        this._nModePill.classList.toggle('slb-mode-active', this._usesProp);
        if (this._usesProp) {
          this._nInput.min = '0';
          this._nInput.max = '100';
          this._nInput.step = '1';
          this._nInput.placeholder = '%';
          this._nInput.value = '10';
        } else {
          this._nInput.min = '1';
          this._nInput.removeAttribute('max');
          this._nInput.step = '1';
          this._nInput.placeholder = 'n';
          this._nInput.value = '5';
        }
        this._updateNValue();
        this._autoSubmit();
      });
      topRow.appendChild(this._nModePill);

      this.card.appendChild(topRow);

      // Settings popover
      this._buildPopover();

      // Group by section (below the card, summarize pattern)
      this.bySection = document.createElement('div');
      this.bySection.className = 'slb-by-section';

      const byLabel = document.createElement('span');
      byLabel.className = 'blockr-label';
      byLabel.textContent = 'Group by:';
      this.bySection.appendChild(byLabel);

      const byWrap = document.createElement('div');
      byWrap.className = 'slb-by-wrap';
      this.bySection.appendChild(byWrap);

      this._bySelect = Blockr.Select.multi(byWrap, {
        options: this.columnNames,
        selected: [],
        placeholder: 'Select grouping columns\u2026',
        reorderable: false,
        onChange: (selected) => {
          this.by = selected;
          this._autoSubmit();
        }
      });
      this._bySelect.el.classList.add('blockr-select--bordered');

      this.el.appendChild(this.bySection);

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

      // order_by select (min/max)
      this._popOrderByWrap = document.createElement('div');
      this._popOrderByWrap.className = 'blockr-popover-row';
      const orderByLabel = document.createElement('label');
      orderByLabel.className = 'blockr-popover-label';
      orderByLabel.textContent = 'Order by:';
      this._popOrderByWrap.appendChild(orderByLabel);
      const orderBySelectWrap = document.createElement('div');
      orderBySelectWrap.className = 'blockr-popover-select-wrap';
      this._popOrderByWrap.appendChild(orderBySelectWrap);
      this._orderBySelect = Blockr.Select.single(orderBySelectWrap, {
        options: this.columnNames,
        selected: null,
        placeholder: 'Column\u2026',
        onChange: (value) => {
          this.order_by = value;
          this._autoSubmit();
        }
      });
      this.popoverEl.appendChild(this._popOrderByWrap);

      // weight_by select (sample)
      this._popWeightByWrap = document.createElement('div');
      this._popWeightByWrap.className = 'blockr-popover-row';
      const weightByLabel = document.createElement('label');
      weightByLabel.className = 'blockr-popover-label';
      weightByLabel.textContent = 'Weight by:';
      this._popWeightByWrap.appendChild(weightByLabel);
      const weightBySelectWrap = document.createElement('div');
      weightBySelectWrap.className = 'blockr-popover-select-wrap';
      this._popWeightByWrap.appendChild(weightBySelectWrap);
      this._weightBySelect = Blockr.Select.single(weightBySelectWrap, {
        options: this.columnNames,
        selected: null,
        placeholder: 'Column (optional)\u2026',
        onChange: (value) => {
          this.weight_by = value;
          this._autoSubmit();
        }
      });
      this.popoverEl.appendChild(this._popWeightByWrap);

      // with_ties toggle (min/max)
      this._popWithTiesWrap = document.createElement('div');
      this._withTiesToggle = document.createElement('button');
      this._withTiesToggle.type = 'button';
      this._withTiesToggle.className = 'blockr-pill blockr-popover-toggle blockr-popover-toggle-active';
      this._withTiesToggle.textContent = 'With ties';
      this._withTiesToggle.addEventListener('click', () => {
        this.with_ties = !this.with_ties;
        this._withTiesToggle.classList.toggle('blockr-popover-toggle-active', this.with_ties);
        this._autoSubmit();
      });
      this._popWithTiesWrap.appendChild(this._withTiesToggle);
      this.popoverEl.appendChild(this._popWithTiesWrap);

      // replace toggle (sample)
      this._popReplaceWrap = document.createElement('div');
      this._replaceToggle = document.createElement('button');
      this._replaceToggle.type = 'button';
      this._replaceToggle.className = 'blockr-pill blockr-popover-toggle';
      this._replaceToggle.textContent = 'Replace';
      this._replaceToggle.addEventListener('click', () => {
        this.replace = !this.replace;
        this._replaceToggle.classList.toggle('blockr-popover-toggle-active', this.replace);
        this._autoSubmit();
      });
      this._popReplaceWrap.appendChild(this._replaceToggle);
      this.popoverEl.appendChild(this._popReplaceWrap);

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

    _updatePopoverVisibility() {
      const isMinMax = this.type === 'min' || this.type === 'max';
      const isSample = this.type === 'sample';
      const hasOptions = isMinMax || isSample;

      // Hide gear entirely when no advanced options apply (head/tail)
      if (this._gearHeader) {
        this._gearHeader.style.display = hasOptions ? '' : 'none';
      }
      // Close popover if we hid the gear
      if (!hasOptions && this._popoverOpen) this._closePopover();

      this._popOrderByWrap.style.display = isMinMax ? '' : 'none';
      this._popWeightByWrap.style.display = isSample ? '' : 'none';
      this._popWithTiesWrap.style.display = isMinMax ? '' : 'none';
      this._popReplaceWrap.style.display = isSample ? '' : 'none';
    }

    _updateNValue() {
      const raw = parseFloat(this._nInput.value);
      if (this._usesProp) {
        this.prop = isNaN(raw) ? null : raw / 100;
        this.n = null;
      } else {
        this.n = isNaN(raw) ? 5 : Math.round(raw);
        this.prop = null;
      }
    }

    _compose() {
      return {
        type: this.type,
        n: this.n,
        prop: this.prop,
        order_by: this.order_by,
        with_ties: this.with_ties,
        weight_by: this.weight_by,
        replace: this.replace,
        by: this.by.slice()
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
      this.type = state?.type || 'head';
      this.n = state?.n ?? 5;
      this.prop = state?.prop ?? null;
      this.order_by = state?.order_by || '';
      this.with_ties = state?.with_ties !== false;
      this.weight_by = state?.weight_by || '';
      this.replace = !!state?.replace;
      this.by = (state?.by || []).slice();

      // Update type select
      if (this._typeSelect) {
        this._typeSelect.setOptions(SLICE_TYPES, this.type);
      }

      // Update n/prop mode
      this._usesProp = this.prop != null && this.prop > 0;
      this._nModePill.textContent = this._usesProp ? '%' : 'n';
      this._nModePill.classList.toggle('slb-mode-active', this._usesProp);
      if (this._usesProp) {
        this._nInput.value = Math.round(this.prop * 100);
        this._nInput.placeholder = '%';
      } else {
        this._nInput.value = this.n ?? 5;
        this._nInput.placeholder = 'n';
      }

      // Update column selects
      if (this._orderBySelect) {
        this._orderBySelect.setOptions(this.columnNames, this.order_by || null);
      }
      if (this._weightBySelect) {
        this._weightBySelect.setOptions(this.columnNames, this.weight_by || null);
      }
      if (this._bySelect) {
        this._bySelect.setOptions(this.columnNames, this.by);
      }

      // Update toggles
      this._withTiesToggle.classList.toggle('blockr-popover-toggle-active', this.with_ties);
      this._replaceToggle.classList.toggle('blockr-popover-toggle-active', this.replace);

      this._updatePopoverVisibility();
    }

    updateColumns(names) {
      this.columnNames = names || [];
      if (this._orderBySelect) {
        const currentOb = this._orderBySelect.getValue();
        this._orderBySelect.setOptions(this.columnNames, currentOb);
        this.order_by = this._orderBySelect.getValue();
      }
      if (this._weightBySelect) {
        const currentWb = this._weightBySelect.getValue();
        this._weightBySelect.setOptions(this.columnNames, currentWb);
        this.weight_by = this._weightBySelect.getValue();
      }
      if (this._bySelect) {
        this._bySelect.setOptions(this.columnNames, this.by);
        this.by = this._bySelect.getValue();
      }
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.slice-block-container'),
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
      el._block = new SliceBlock(el);
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

  Shiny.inputBindings.register(binding, 'blockr.slice');

  // Column names handler (global — dispatches by msg.id)
  Shiny.addCustomMessageHandler('slice-columns', (msg) => {
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
  Shiny.addCustomMessageHandler('slice-block-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
