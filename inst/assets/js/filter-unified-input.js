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

  function createAceEditor(container, value, cols, onChangeCallback, opts) {
    var el = document.createElement("div");
    el.className = "fu-ace-editor";
    container.appendChild(el);
    withAce(function() {
      var ed = ace.edit(el);
      ed.setTheme("ace/theme/tomorrow");
      ed.session.setMode("ace/mode/r");
      var maxL = (opts && opts.maxLines) || 1;
      ed.setOptions({ minLines:1, maxLines:maxL, showLineNumbers:false, showPrintMargin:false,
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
  // Selectize helpers (using Shiny's already-loaded selectize.js via jQuery)
  // ---------------------------------------------------------------------------

  // Wait for selectize to be available (Shiny loads it)
  function withSelectize(fn) {
    if (typeof $ !== "undefined" && $.fn && $.fn.selectize) { fn(); return; }
    var a = 0, t = setInterval(function() {
      a++;
      if (typeof $ !== "undefined" && $.fn && $.fn.selectize) { clearInterval(t); fn(); }
      if (a > 100) clearInterval(t); // give up after 10s
    }, 100);
  }

  // Create a single-select selectize for column picking
  function createColumnSelectize(container, options, selected, onChange) {
    var sel = document.createElement("select");
    sel.className = "fu-selectize-col";
    container.appendChild(sel);

    var wrapper = {
      el: sel, api: null,
      setOptions: function(opts, s) {
        if (!wrapper.api) { wrapper._pending = { opts: opts, sel: s }; return; }
        wrapper.api.clearOptions();
        opts.forEach(function(v) { wrapper.api.addOption({ value: v, text: v }); });
        wrapper.api.refreshOptions(false);
        if (s) wrapper.api.setValue(s, true);
        else if (opts.length > 0) wrapper.api.setValue(opts[0], true);
      },
      getValue: function() { return wrapper.api ? wrapper.api.getValue() : ""; },
      destroy: function() { if (wrapper.api) wrapper.api.destroy(); }
    };

    withSelectize(function() {
      var $sel = $(sel).selectize({
        options: options.map(function(v) { return { value: v, text: v }; }),
        items: selected ? [selected] : (options.length > 0 ? [options[0]] : []),
        maxItems: 1,
        placeholder: "Column...",
        onChange: function(value) { if (onChange) onChange(value); }
      });
      wrapper.api = $sel[0].selectize;
      // Apply any pending options that arrived before init
      if (wrapper._pending) {
        wrapper.setOptions(wrapper._pending.opts, wrapper._pending.sel);
        delete wrapper._pending;
      }
    });

    return wrapper;
  }

  // Create a multi-select selectize for value picking
  function createValueSelectize(container, options, selected, onChange) {
    var sel = document.createElement("select");
    sel.multiple = true;
    sel.className = "fu-selectize-val";
    container.appendChild(sel);

    var wrapper = {
      el: sel, api: null,
      getValue: function() { return wrapper.api ? (wrapper.api.getValue() || []) : []; },
      destroy: function() { if (wrapper.api) wrapper.api.destroy(); }
    };

    withSelectize(function() {
      var $sel = $(sel).selectize({
        options: options.map(function(v) { return { value: v, text: v }; }),
        items: selected || [],
        plugins: ["remove_button", "drag_drop"],
        placeholder: "Select values\u2026",
        onChange: function(value) { if (onChange) onChange(value || []); }
      });
      wrapper.api = $sel[0].selectize;
    });

    return wrapper;
  }

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

    // Card
    this.card = document.createElement("div");
    this.card.className = "fu-card";
    this.el.appendChild(this.card);

    // Conditions list
    this.listEl = document.createElement("div");
    this.listEl.className = "fu-conditions";
    this.card.appendChild(this.listEl);

    // Add row: [+ Add condition] [AND] ... [R expression]
    var addRow = document.createElement("div");
    addRow.className = "fu-add-row";

    var addCondLink = document.createElement("span");
    addCondLink.className = "fu-add-link";
    addCondLink.innerHTML = '<span class="fu-add-icon">' + ICON_PLUS + '</span> Add condition';
    addCondLink.addEventListener("click", function() { self._addValueRow(null, null); });
    addRow.appendChild(addCondLink);

    // AND/OR toggle — sits next to "+ Add condition"
    this.opToggle = document.createElement("button");
    this.opToggle.type = "button";
    this.opToggle.className = "fu-pill fu-op-toggle";
    this.opToggle.textContent = "AND";
    this.opToggle.title = "Toggle AND / OR";
    this.opToggle.style.visibility = "hidden";
    this.opToggle.addEventListener("click", function() {
      self.operator = self.operator === "&" ? "|" : "&";
      self.opToggle.textContent = self.operator === "&" ? "AND" : "OR";
      self._autoSubmit();
    });
    addRow.appendChild(this.opToggle);

    // Spacer
    var spacer = document.createElement("span");
    spacer.style.flex = "1";
    addRow.appendChild(spacer);

    var addExprLink = document.createElement("span");
    addExprLink.className = "fu-add-link-expr";
    addExprLink.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M10.478 1.647a.5.5 0 1 0-.956-.294l-4 13a.5.5 0 0 0 .956.294zM4.854 4.146a.5.5 0 0 1 0 .708L1.707 8l3.147 3.146a.5.5 0 0 1-.708.708l-3.5-3.5a.5.5 0 0 1 0-.708l3.5-3.5a.5.5 0 0 1 .708 0m6.292 0a.5.5 0 0 0 0 .708L14.293 8l-3.147 3.146a.5.5 0 0 0 .708.708l3.5-3.5a.5.5 0 0 0 0-.708l-3.5-3.5a.5.5 0 0 0-.708 0"/></svg>';
    addExprLink.title = "Add R expression";
    addExprLink.addEventListener("click", function() { self._addExprRow(""); });
    addRow.appendChild(addExprLink);

    this.card.appendChild(addRow);

    // Preserve order checkbox
    var optionsRow = document.createElement("div");
    optionsRow.className = "fu-options-row";

    var preserveLabel = document.createElement("label");
    preserveLabel.className = "fu-preserve-label";
    this.preserveCheckbox = document.createElement("input");
    this.preserveCheckbox.type = "checkbox";
    this.preserveCheckbox.addEventListener("change", function() {
      self.preserveOrder = self.preserveCheckbox.checked;
      self._autoSubmit();
    });
    preserveLabel.appendChild(this.preserveCheckbox);
    preserveLabel.appendChild(document.createTextNode(" Preserve selection order"));
    optionsRow.appendChild(preserveLabel);

    this.card.appendChild(optionsRow);
    this.preserveOrder = false;
  };

  // --- Operator cycle button ---
  var CAT_OPS = [
    { value: "is", label: "is" },
    { value: "is not", label: "is not" }
  ];
  var NUM_OPS = [
    { value: "is", label: "is" },
    { value: "is not", label: "is not" },
    { value: ">", label: ">" },
    { value: ">=", label: "\u2265" },
    { value: "<", label: "<" },
    { value: "<=", label: "\u2264" }
  ];

  FilterUnified.prototype._createOpButton = function(cond, ops) {
    var self = this;
    var idx = 0;
    cond.op = ops[0].value;

    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "fu-pill fu-op-btn";
    btn.textContent = ops[0].label;
    btn.title = "Click to cycle operator";
    btn.addEventListener("click", function() {
      idx = (idx + 1) % ops.length;
      cond.op = ops[idx].value;
      btn.textContent = ops[idx].label;
      self._renderDynamicContent(cond);
      self._autoSubmit();
    });

    cond._opBtn = btn;
    cond._opList = ops;
    return btn;
  };

  // --- Value condition rows ---

  FilterUnified.prototype._addValueRow = function(column, opts) {
    var self = this;
    var id = this.nextId++;
    var cond = {
      id: id, filterType: "none", column: column || "",
      op: "is",
      values: (opts && opts.values) || [],
      numValue: (opts && opts.numValue) || null,
      multiSelect: null, exprEl: null, rowEl: null
    };

    var row = document.createElement("div");
    row.className = "fu-row";
    row.setAttribute("data-cond-id", id);
    cond.rowEl = row;

    // Column dropdown (selectize single-select)
    var colDiv = document.createElement("div");
    colDiv.className = "fu-col-wrap";
    row.appendChild(colDiv);
    cond._colSelectize = createColumnSelectize(
      colDiv, this.columnNames, column,
      function(value) { self._onColumnChange(cond, value); }
    );

    // Operator button slot (populated on column change)
    cond._opBtnSlot = document.createElement("span");
    row.appendChild(cond._opBtnSlot);

    // Dynamic content area
    var contentDiv = document.createElement("div");
    contentDiv.className = "fu-row-content";
    row.appendChild(contentDiv);
    cond._contentDiv = contentDiv;

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

    // Trigger column change — either for the explicitly passed column
    // or for the auto-selected first column
    var activeCol = column || cond._colSelectize.getValue();
    if (activeCol && this.columnMeta[activeCol]) {
      this._onColumnChange(cond, activeCol);
    }
  };

  FilterUnified.prototype._onColumnChange = function(cond, colName) {
    cond.column = colName;
    var meta = this.columnMeta[colName];

    // Reset selections when column changes
    cond.values = [];
    cond.numValue = null;

    if (cond._valueSelectize) { cond._valueSelectize.destroy(); cond._valueSelectize = null; }
    cond._contentDiv.innerHTML = "";
    cond._opBtnSlot.innerHTML = "";

    if (!meta) { cond.filterType = "none"; return; }

    var isNumeric = meta.type === "numeric" || meta.type === "integer";
    cond._meta = meta;
    cond.filterType = isNumeric ? "numeric" : "values";

    // Create operator button with the right ops for this type
    var ops = isNumeric ? NUM_OPS : CAT_OPS;
    var btn = this._createOpButton(cond, ops);
    cond._opBtnSlot.appendChild(btn);

    this._renderDynamicContent(cond);
  };

  // Unified content renderer — dispatches based on op
  FilterUnified.prototype._renderDynamicContent = function(cond) {
    var self = this;
    var container = cond._contentDiv;
    var meta = cond._meta;

    if (cond._valueSelectize) { cond._valueSelectize.destroy(); cond._valueSelectize = null; }
    container.innerHTML = "";

    if (!meta) return;

    var op = cond.op;

    if (op === "is" || op === "is not") {
      // Multi-select for exact values (both categorical and numeric)
      var allValues;
      if (meta.type === "numeric" || meta.type === "integer") {
        allValues = (meta.uniqueValues || []).map(String);
      } else {
        allValues = (meta.values || []).slice();
        if (meta.hasEmpty) allValues.push("<empty>");
      }
      if (meta.hasNA) allValues.push("<NA>");

      cond._valueSelectize = createValueSelectize(
        container, allValues, cond.values || [],
        function(selected) {
          cond.values = selected;
          self._autoSubmit();
        }
      );
    } else {
      // Single number input for comparison operators
      var numInput = document.createElement("input");
      numInput.type = "number";
      numInput.className = "fu-num-input";
      numInput.step = "any";
      numInput.placeholder = "Enter value\u2026";
      if (cond.numValue != null) numInput.value = cond.numValue;
      numInput.addEventListener("input", function() {
        cond.numValue = numInput.value === "" ? null : parseFloat(numInput.value);
        self._autoSubmit();
      });
      container.appendChild(numInput);
    }
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
    }, { maxLines: 10 });

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

    if (cond._valueSelectize) cond._valueSelectize.destroy();
    if (cond._colSelectize) cond._colSelectize.destroy();
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
      if (!c.column && c.filterType !== "expr") return;

      var op = c.op;

      if ((c.filterType === "values" || c.filterType === "numeric") && (op === "is" || op === "is not")) {
        // Multi-select values (categorical or numeric =)
        if (c.values && c.values.length > 0) {
          conditions.push({ type: "values", column: c.column, values: c.values,
            mode: op === "is" ? "include" : "exclude" });
        }
      } else if (c.filterType === "numeric" && c.numValue != null) {
        // Comparison operator with single value
        conditions.push({ type: "numeric", column: c.column, op: op, value: c.numValue });
      } else if (c.filterType === "expr" && c.exprEl && c.exprEl._aceEditor) {
        var val = c.exprEl._aceEditor.getValue().trim();
        if (val && val !== "") conditions.push({ type: "expr", expr: val });
      }
    });
    return { conditions: conditions, operator: this.operator, preserveOrder: this.preserveOrder };
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
      if (c._colSelectize) {
        var current = c._colSelectize.getValue();
        c._colSelectize.setOptions(self.columnNames, current);
        // Auto-select first column if none was set
        var col = c._colSelectize.getValue();
        if (col && self.columnMeta[col]) {
          self._onColumnChange(c, col);
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
