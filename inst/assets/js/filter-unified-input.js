// FilterUnified — JS-driven unified filter block.
// Auto-detects column types: multi-select for categorical, min/max for numeric.
// Value/range changes auto-submit. Expression mode has explicit Submit.

(function() {
  "use strict";

  // ---------------------------------------------------------------------------
  // SVG icons
  // ---------------------------------------------------------------------------
  var ICON_X = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>';
  var ICON_PLUS = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>';
  var ICON_CHEVRON = '<svg xmlns="http://www.w3.org/2000/svg" width="12" height="12" fill="currentColor" viewBox="0 0 16 16"><path fill-rule="evenodd" d="M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708"/></svg>';

  // ---------------------------------------------------------------------------
  // ACE editor helpers (for expression mode)
  // ---------------------------------------------------------------------------
  var defaultCategories = {
    arithmetic: ["abs","sign","ceiling","floor","round","trunc","log","log2","log10","exp","sqrt"],
    aggregate: ["mean","sum","min","max"],
    offset: ["lead","lag","cumsum","cumprod","cummin","cummax"],
    logical: ["if_else","case_when"],
    string: ["str_c","paste","paste0","str_sub","str_to_lower","str_to_upper"],
    ranking: ["row_number","min_rank","dense_rank","percent_rank","ntile"]
  };

  function backtickIfNeeded(n) {
    return /^[a-zA-Z.][a-zA-Z0-9._]*$/.test(n) ? n : "`" + n + "`";
  }

  function withAce(fn) {
    if (typeof ace !== "undefined") { fn(); return; }
    var a = 0, t = setInterval(function() {
      a++; if (typeof ace !== "undefined") { clearInterval(t); fn(); }
      if (a > 50) clearInterval(t);
    }, 100);
  }

  function makeCompleter(cols) {
    return { getCompletions: function(_e,_s,_p,_pr,cb) {
      var cats = Object.assign({}, defaultCategories);
      cats.column = (cols||[]).map(backtickIfNeeded);
      var list = [];
      Object.keys(cats).forEach(function(cat) {
        cats[cat].forEach(function(fn) {
          var isC = cat === "column";
          list.push({ caption: fn, value: fn + (isC ? "" : "()"), meta: cat, score: isC ? 1001 : 1000 });
        });
      });
      list.sort(function(a,b) { return a.score !== b.score ? b.score - a.score : a.caption.localeCompare(b.caption); });
      cb(null, list);
    }};
  }

  function createAceEditor(container, value, cols, onChangeCallback) {
    var el = document.createElement("div");
    el.className = "fu-ace-editor";
    container.appendChild(el);
    withAce(function() {
      var ed = ace.edit(el);
      ed.setTheme("ace/theme/tomorrow");
      ed.session.setMode("ace/mode/r");
      ed.setOptions({ maxLines:1, showLineNumbers:false, showPrintMargin:false,
        highlightActiveLine:false, tabSize:2, fontSize:14,
        enableLiveAutocompletion:true, enableBasicAutocompletion:true });
      ed.setValue(value||"", 1);
      ed.renderer.setScrollMargin(0,0,0,0);
      ed.completers = [makeCompleter(cols)];
      ed.commands.on("afterExec", function(e) {
        if (e.command.name==="insertstring"||e.command.name==="Return") {
          var p=ed.getCursorPosition(), l=ed.session.getLine(p.row);
          if (l.substring(p.column-2,p.column)==="()") ed.moveCursorTo(p.row,p.column-1);
        }
      });
      // Auto-submit on change (debounced by caller)
      if (onChangeCallback) {
        ed.session.on("change", onChangeCallback);
      }
      el._aceEditor = ed;
    });
    return el;
  }

  // ---------------------------------------------------------------------------
  // Multi-select dropdown widget
  // ---------------------------------------------------------------------------
  function MultiSelect(container, values, selected, onChange) {
    this.values = values || [];
    this.selected = new Set(selected || []);
    this.onChange = onChange || function(){};
    this.isOpen = false;
    this._build(container);
  }

  MultiSelect.prototype._build = function(container) {
    var self = this;
    this.el = document.createElement("div");
    this.el.className = "fu-multiselect";

    this.trigger = document.createElement("div");
    this.trigger.className = "fu-multiselect-trigger";
    this.trigger.addEventListener("click", function(e) {
      e.stopPropagation(); self.toggle();
    });
    this._updateTrigger();
    this.el.appendChild(this.trigger);

    this.panel = document.createElement("div");
    this.panel.className = "fu-multiselect-panel";
    this.panel.style.display = "none";

    this.searchInput = document.createElement("input");
    this.searchInput.type = "text";
    this.searchInput.className = "fu-multiselect-search";
    this.searchInput.placeholder = "Search...";
    this.searchInput.addEventListener("input", function() { self._filter(); });
    this.searchInput.addEventListener("click", function(e) { e.stopPropagation(); });
    this.panel.appendChild(this.searchInput);

    this.listEl = document.createElement("div");
    this.listEl.className = "fu-multiselect-list";
    this._renderOptions();
    this.panel.appendChild(this.listEl);

    this.el.appendChild(this.panel);
    container.appendChild(this.el);

    this._outsideClick = function(e) {
      if (!self.el.contains(e.target)) self.close();
    };
    document.addEventListener("click", this._outsideClick);
  };

  MultiSelect.prototype._renderOptions = function() {
    var self = this;
    this.listEl.innerHTML = "";
    this.optionEls = [];

    this.values.forEach(function(val) {
      var opt = document.createElement("label");
      opt.className = "fu-multiselect-option";
      if (self.selected.has(val)) opt.classList.add("selected");
      opt.setAttribute("data-value", val);

      var cb = document.createElement("input");
      cb.type = "checkbox";
      cb.checked = self.selected.has(val);
      cb.addEventListener("change", function(e) {
        e.stopPropagation();
        if (cb.checked) self.selected.add(val);
        else self.selected.delete(val);
        opt.classList.toggle("selected", cb.checked);
        self._updateTrigger();
        self.onChange(Array.from(self.selected));
      });
      opt.appendChild(cb);

      var label = document.createElement("span");
      label.textContent = val;
      opt.appendChild(label);

      self.listEl.appendChild(opt);
      self.optionEls.push({ el: opt, value: val.toLowerCase() });
    });
  };

  MultiSelect.prototype._filter = function() {
    var q = this.searchInput.value.toLowerCase();
    this.optionEls.forEach(function(o) {
      o.el.style.display = o.value.indexOf(q) >= 0 ? "" : "none";
    });
  };

  MultiSelect.prototype._updateTrigger = function() {
    var self = this;
    this.trigger.innerHTML = "";
    var n = this.selected.size;

    if (n === 0) {
      var ph = document.createElement("span");
      ph.className = "fu-multiselect-placeholder";
      ph.textContent = "Select values...";
      this.trigger.appendChild(ph);
    } else {
      var chipsDiv = document.createElement("span");
      chipsDiv.className = "fu-multiselect-chips";

      var arr = Array.from(this.selected);
      var maxShow = 3;
      var shown = arr.slice(0, maxShow);

      shown.forEach(function(val) {
        var chip = document.createElement("span");
        chip.className = "fu-chip";

        var chipText = document.createElement("span");
        chipText.className = "fu-chip-text";
        chipText.textContent = val;
        chip.appendChild(chipText);

        var chipX = document.createElement("span");
        chipX.className = "fu-chip-remove";
        chipX.innerHTML = "&times;";
        chipX.addEventListener("click", function(e) {
          e.stopPropagation();
          self.selected.delete(val);
          // Update checkbox in dropdown
          self.optionEls.forEach(function(o) {
            if (o.el.getAttribute("data-value") === val) {
              o.el.classList.remove("selected");
              var cb = o.el.querySelector("input[type=checkbox]");
              if (cb) cb.checked = false;
            }
          });
          self._updateTrigger();
          self.onChange(Array.from(self.selected));
        });
        chip.appendChild(chipX);
        chipsDiv.appendChild(chip);
      });

      if (arr.length > maxShow) {
        var more = document.createElement("span");
        more.className = "fu-chip-more";
        more.textContent = "+" + (arr.length - maxShow) + " more";
        chipsDiv.appendChild(more);
      }

      this.trigger.appendChild(chipsDiv);
    }

    var chevron = document.createElement("span");
    chevron.className = "fu-multiselect-chevron";
    chevron.innerHTML = ICON_CHEVRON;
    this.trigger.appendChild(chevron);
  };

  MultiSelect.prototype.toggle = function() { this.isOpen ? this.close() : this.open(); };
  MultiSelect.prototype.open = function() {
    this.isOpen = true; this.panel.style.display = "";
    this.searchInput.value = ""; this._filter(); this.searchInput.focus();
  };
  MultiSelect.prototype.close = function() { this.isOpen = false; this.panel.style.display = "none"; };

  MultiSelect.prototype.setValues = function(values, selected) {
    this.values = values || []; this.selected = new Set(selected || []);
    this._renderOptions(); this._updateTrigger();
  };

  MultiSelect.prototype.destroy = function() {
    document.removeEventListener("click", this._outsideClick);
    if (this.el.parentNode) this.el.parentNode.removeChild(this.el);
  };

  // ---------------------------------------------------------------------------
  // FilterUnified component
  // ---------------------------------------------------------------------------
  function FilterUnified(el) {
    this.el = el;
    this.conditions = [];
    this.operator = "&";
    this.nextId = 1;
    this.columnMeta = {};
    this.columnNames = [];
    this._callback = null;
    this._submitted = false;
    this._debounceTimer = null;

    this._initValue = null;
    this._buildDOM();
    this._addValueRow(null, null);
  }

  // Debounced auto-submit for value/range changes (300ms)
  FilterUnified.prototype._autoSubmit = function() {
    var self = this;
    clearTimeout(this._debounceTimer);
    this._debounceTimer = setTimeout(function() {
      self._submit();
    }, 300);
  };

  FilterUnified.prototype._buildDOM = function() {
    var self = this;

    // Header
    var header = document.createElement("div");
    header.className = "fu-header";

    this.opToggle = document.createElement("button");
    this.opToggle.type = "button";
    this.opToggle.className = "fu-op-toggle";
    this.opToggle.textContent = "AND";
    this.opToggle.title = "Toggle AND / OR";
    this.opToggle.style.visibility = "hidden";
    this.opToggle.addEventListener("click", function() {
      self.operator = self.operator === "&" ? "|" : "&";
      self.opToggle.textContent = self.operator === "&" ? "AND" : "OR";
      self._autoSubmit();
    });
    header.appendChild(this.opToggle);

    this.el.appendChild(header);

    // Card
    this.card = document.createElement("div");
    this.card.className = "fu-card";
    this.el.appendChild(this.card);

    // Value conditions list
    this.listEl = document.createElement("div");
    this.listEl.className = "fu-conditions";
    this.card.appendChild(this.listEl);

    // Add row with condition + expression options
    var addRow = document.createElement("div");
    addRow.className = "fu-add-row";

    var addCondLink = document.createElement("span");
    addCondLink.className = "fu-add-link";
    addCondLink.innerHTML = '<span class="fu-add-icon">' + ICON_PLUS + '</span> Add condition';
    addCondLink.addEventListener("click", function() { self._addValueRow(null, null); });
    addRow.appendChild(addCondLink);

    var addSep = document.createElement("span");
    addSep.className = "fu-add-sep";
    addSep.textContent = "or";
    addRow.appendChild(addSep);

    var addExprLink = document.createElement("span");
    addExprLink.className = "fu-add-link-expr";
    addExprLink.textContent = "R expression";
    addExprLink.addEventListener("click", function() { self._addExprRow(""); });
    addRow.appendChild(addExprLink);

    this.card.appendChild(addRow);
  };

  // --- Switch toggle for exclude ---
  FilterUnified.prototype._createExcludeToggle = function(cond) {
    var self = this;
    var toggle = document.createElement("label");
    toggle.className = "fu-switch";
    toggle.title = "Toggle include/exclude";

    var input = document.createElement("input");
    input.type = "checkbox";
    input.checked = cond.exclude;
    input.addEventListener("change", function() {
      cond.exclude = input.checked;
      slider.classList.toggle("active", input.checked);
      labelEl.textContent = input.checked ? "Excl" : "Incl";
      self._autoSubmit();
    });
    toggle.appendChild(input);

    var slider = document.createElement("span");
    slider.className = "fu-switch-slider" + (cond.exclude ? " active" : "");
    toggle.appendChild(slider);

    var labelEl = document.createElement("span");
    labelEl.className = "fu-switch-label";
    labelEl.textContent = cond.exclude ? "Excl" : "Incl";
    toggle.appendChild(labelEl);

    return toggle;
  };

  // --- Value condition rows ---

  FilterUnified.prototype._addValueRow = function(column, opts) {
    var self = this;
    var id = this.nextId++;
    var cond = {
      id: id, filterType: "none", column: column || "",
      values: (opts && opts.values) || [],
      min: (opts && opts.min) || null,
      max: (opts && opts.max) || null,
      exclude: (opts && opts.exclude) || false,
      multiSelect: null, exprEl: null, rowEl: null
    };

    var row = document.createElement("div");
    row.className = "fu-row";
    row.setAttribute("data-cond-id", id);
    cond.rowEl = row;

    // Column dropdown
    var colSelect = document.createElement("select");
    colSelect.className = "fu-col-select";
    this._populateColumnSelect(colSelect, column);
    colSelect.addEventListener("change", function() {
      self._onColumnChange(cond, colSelect.value);
    });
    row.appendChild(colSelect);
    cond._colSelect = colSelect;

    // Dynamic content area
    var contentDiv = document.createElement("div");
    contentDiv.className = "fu-row-content";
    row.appendChild(contentDiv);
    cond._contentDiv = contentDiv;

    // Exclude switch toggle
    var exclToggle = this._createExcludeToggle(cond);
    row.appendChild(exclToggle);

    // Remove button
    var rmBtn = document.createElement("button");
    rmBtn.className = "fu-row-remove";
    rmBtn.type = "button";
    rmBtn.innerHTML = ICON_X;
    rmBtn.addEventListener("click", function() {
      self._removeCondition(id);
      self._autoSubmit();
    });
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.conditions.push(cond);
    this._updateUI();

    if (column && this.columnMeta[column]) {
      this._onColumnChange(cond, column);
    }
  };

  FilterUnified.prototype._populateColumnSelect = function(select, selected) {
    select.innerHTML = "";
    var opt0 = document.createElement("option");
    opt0.value = "";
    opt0.textContent = "Select column...";
    select.appendChild(opt0);

    this.columnNames.forEach(function(name) {
      var opt = document.createElement("option");
      opt.value = name;
      opt.textContent = name;
      if (name === selected) opt.selected = true;
      select.appendChild(opt);
    });
  };

  FilterUnified.prototype._onColumnChange = function(cond, colName) {
    var self = this;
    cond.column = colName;
    var meta = this.columnMeta[colName];
    var contentDiv = cond._contentDiv;

    if (cond.multiSelect) { cond.multiSelect.destroy(); cond.multiSelect = null; }
    contentDiv.innerHTML = "";

    if (!meta) { cond.filterType = "none"; return; }

    if (meta.type === "numeric" || meta.type === "integer") {
      cond.filterType = "range";
      cond.min = meta.min;
      cond.max = meta.max;
      this._renderRangeUI(cond, contentDiv, meta);
    } else {
      cond.filterType = "values";
      cond.values = [];
      this._renderValuesUI(cond, contentDiv, meta);
    }
  };

  FilterUnified.prototype._renderValuesUI = function(cond, container, meta) {
    var self = this;
    var allValues = (meta.values || []).slice();
    if (meta.hasEmpty) allValues.push("<empty>");
    if (meta.hasNA) allValues.push("<NA>");

    cond.multiSelect = new MultiSelect(container, allValues, cond.values, function(selected) {
      cond.values = selected;
      self._autoSubmit();
    });
  };

  FilterUnified.prototype._renderRangeUI = function(cond, container, meta) {
    var self = this;
    var rangeDiv = document.createElement("div");
    rangeDiv.className = "fu-range";

    var minLabel = document.createElement("span");
    minLabel.className = "fu-range-label";
    minLabel.textContent = "Min";
    rangeDiv.appendChild(minLabel);

    var minInput = document.createElement("input");
    minInput.type = "number";
    minInput.className = "fu-range-input";
    minInput.step = "any";
    minInput.value = meta.min != null ? meta.min : "";
    minInput.addEventListener("input", function() {
      cond.min = minInput.value === "" ? null : parseFloat(minInput.value);
      self._autoSubmit();
    });
    rangeDiv.appendChild(minInput);

    var dash = document.createElement("span");
    dash.className = "fu-range-dash";
    dash.textContent = "—";
    rangeDiv.appendChild(dash);

    var maxLabel = document.createElement("span");
    maxLabel.className = "fu-range-label";
    maxLabel.textContent = "Max";
    rangeDiv.appendChild(maxLabel);

    var maxInput = document.createElement("input");
    maxInput.type = "number";
    maxInput.className = "fu-range-input";
    maxInput.step = "any";
    maxInput.value = meta.max != null ? meta.max : "";
    maxInput.addEventListener("input", function() {
      cond.max = maxInput.value === "" ? null : parseFloat(maxInput.value);
      self._autoSubmit();
    });
    rangeDiv.appendChild(maxInput);

    container.appendChild(rangeDiv);
  };

  // --- Expression rows ---

  var ICON_CONFIRM = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>';

  FilterUnified.prototype._addExprRow = function(value) {
    var self = this;
    var id = this.nextId++;

    var row = document.createElement("div");
    row.className = "fu-row fu-row-expr";
    row.setAttribute("data-cond-id", id);

    var codeDiv = document.createElement("div");
    codeDiv.className = "fu-row-content fu-expr-code";
    row.appendChild(codeDiv);

    // Confirm button: shows "Enter ↵", turns into ✓ when confirmed
    var confirmBtn = document.createElement("button");
    confirmBtn.className = "fu-expr-confirm";
    confirmBtn.type = "button";
    confirmBtn.innerHTML = "Enter &#x21B5;";
    confirmBtn.title = "Apply expression";
    var doConfirm = function() {
      confirmBtn.classList.add("confirmed");
      confirmBtn.innerHTML = ICON_CONFIRM;
      self._submit();
    };
    confirmBtn.addEventListener("click", doConfirm);

    // When expression text changes, reset to unconfirmed
    var exprEl = createAceEditor(codeDiv, value, this.columnNames, function() {
      confirmBtn.classList.remove("confirmed");
      confirmBtn.innerHTML = "Enter &#x21B5;";
    });

    // Also confirm on Enter key in the ACE editor
    withAce(function() {
      if (exprEl._aceEditor) {
        exprEl._aceEditor.commands.addCommand({
          name: "confirmExpr",
          bindKey: { win: "Enter", mac: "Enter" },
          exec: function() { doConfirm(); }
        });
      }
    });
    row.appendChild(confirmBtn);

    var rmBtn = document.createElement("button");
    rmBtn.className = "fu-row-remove";
    rmBtn.type = "button";
    rmBtn.innerHTML = ICON_X;
    rmBtn.addEventListener("click", function() { self._removeCondition(id); });
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.conditions.push({
      id: id, filterType: "expr", column: null,
      values: null, min: null, max: null, exclude: false,
      multiSelect: null, exprEl: exprEl, rowEl: row
    });
    this._updateUI();
  };

  // --- Shared ---

  FilterUnified.prototype._removeCondition = function(id) {
    // Must keep at least one condition total
    if (this.conditions.length <= 1) return;

    var cond = null, idx = -1;
    for (var i = 0; i < this.conditions.length; i++) {
      if (this.conditions[i].id === id) { cond = this.conditions[i]; idx = i; break; }
    }
    if (!cond) return;

    if (cond.multiSelect) cond.multiSelect.destroy();
    if (cond.exprEl && cond.exprEl._aceEditor) cond.exprEl._aceEditor.destroy();
    if (cond.rowEl && cond.rowEl.parentNode) cond.rowEl.parentNode.removeChild(cond.rowEl);
    this.conditions.splice(idx, 1);
    this._updateUI();
  };

  FilterUnified.prototype._updateUI = function() {
    var total = this.conditions.length;
    var single = total <= 1;
    this.opToggle.style.visibility = total > 1 ? "visible" : "hidden";

    this.conditions.forEach(function(c) {
      var btn = c.rowEl.querySelector(".fu-row-remove");
      if (btn) btn.style.visibility = single ? "hidden" : "visible";
    });
  };

  FilterUnified.prototype._compose = function() {
    var conditions = [];
    this.conditions.forEach(function(c) {
      if (c.filterType === "values" && c.column && c.values && c.values.length > 0) {
        conditions.push({ type: "values", column: c.column, values: c.values,
          mode: c.exclude ? "exclude" : "include" });
      } else if (c.filterType === "range" && c.column && (c.min != null || c.max != null)) {
        conditions.push({ type: "range", column: c.column, min: c.min, max: c.max,
          mode: c.exclude ? "exclude" : "include" });
      } else if (c.filterType === "expr" && c.exprEl && c.exprEl._aceEditor) {
        var val = c.exprEl._aceEditor.getValue().trim();
        if (val && val !== "") conditions.push({ type: "expr", expr: val });
      }
    });
    return { conditions: conditions, operator: this.operator };
  };

  FilterUnified.prototype._submit = function() {
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  FilterUnified.prototype.getValue = function() {
    if (!this._submitted) return null;
    return this._compose();
  };

  FilterUnified.prototype.setValue = function(value) {};

  FilterUnified.prototype.updateColumns = function(meta) {
    this.columnMeta = {};
    this.columnNames = [];
    var self = this;
    (meta || []).forEach(function(col) {
      self.columnMeta[col.name] = col;
      self.columnNames.push(col.name);
    });
    this.conditions.forEach(function(c) {
      if (c._colSelect) {
        var current = c._colSelect.value;
        self._populateColumnSelect(c._colSelect, current);
        if (current && self.columnMeta[current]) {
          self._onColumnChange(c, current);
        }
      }
    });
  };

  // ---------------------------------------------------------------------------
  // Shiny input binding
  // ---------------------------------------------------------------------------
  var Binding = new Shiny.InputBinding();

  $.extend(Binding, {
    find: function(scope) { return $(scope).find(".filter-unified-container"); },
    getId: function(el) { return el.id || null; },
    getValue: function(el) {
      if (!el._filterUnified) return null;
      return el._filterUnified.getValue();
    },
    setValue: function(el, value) {
      if (el._filterUnified) el._filterUnified.setValue(value);
    },
    subscribe: function(el, callback) {
      if (el._filterUnified) el._filterUnified._callback = function() { callback(true); };
    },
    unsubscribe: function(el) {
      if (el._filterUnified) el._filterUnified._callback = null;
    },
    initialize: function(el) {
      el._filterUnified = new FilterUnified(el);
      if (el._pendingColumns) {
        el._filterUnified.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);
    }
  });

  Shiny.inputBindings.register(Binding, "blockr.filterUnified");

  Shiny.addCustomMessageHandler("filter-unified-update-columns", function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._filterUnified) {
      el._filterUnified.updateColumns(msg.columns);
    } else if (el) {
      el._pendingColumns = msg.columns;
    } else {
      var a = 0, t = setInterval(function() {
        a++;
        var el2 = document.getElementById(msg.id);
        if (el2 && el2._filterUnified) { el2._filterUnified.updateColumns(msg.columns); clearInterval(t); }
        else if (el2) { el2._pendingColumns = msg.columns; clearInterval(t); }
        if (a > 50) clearInterval(t);
      }, 100);
    }
  });

})();
