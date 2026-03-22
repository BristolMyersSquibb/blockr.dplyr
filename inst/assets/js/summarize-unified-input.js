// SummarizeUnified — JS-driven unified summarize block.
// Each row is either "simple" (function + column) or "expression" (ACE editor).
// Simple changes auto-submit (300ms debounce), expressions need Enter confirm.

(function() {
  "use strict";

  // ---------------------------------------------------------------------------
  // SVG icons
  // ---------------------------------------------------------------------------
  var ICON_X = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>';
  var ICON_PLUS = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>';
  var ICON_CONFIRM = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>';

  // ---------------------------------------------------------------------------
  // Summary functions available in simple mode
  // ---------------------------------------------------------------------------
  var SUMMARY_FUNCS = [
    "mean", "median", "sd", "min", "max", "sum", "n", "n_distinct", "first", "last"
  ];

  // Functions that don't take a column argument
  var NO_COL_FUNCS = ["n"];

  // ---------------------------------------------------------------------------
  // ACE editor helpers (for expression mode)
  // ---------------------------------------------------------------------------
  var defaultCategories = {
    aggregate: ["mean", "median", "sd", "sum", "min", "max", "n", "n_distinct", "first", "last"],
    arithmetic: ["abs", "sign", "ceiling", "floor", "round", "trunc", "log", "log2", "log10", "exp", "sqrt"],
    logical: ["if_else", "case_when"],
    string: ["str_c", "paste", "paste0", "str_sub", "str_to_lower", "str_to_upper"]
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
    return { getCompletions: function(_e, _s, _p, _pr, cb) {
      var cats = Object.assign({}, defaultCategories);
      cats.column = (cols || []).map(backtickIfNeeded);
      var list = [];
      Object.keys(cats).forEach(function(cat) {
        cats[cat].forEach(function(fn) {
          var isC = cat === "column";
          list.push({ caption: fn, value: fn + (isC ? "" : "()"), meta: cat, score: isC ? 1001 : 1000 });
        });
      });
      list.sort(function(a, b) { return a.score !== b.score ? b.score - a.score : a.caption.localeCompare(b.caption); });
      cb(null, list);
    }};
  }

  function createAceEditor(container, value, cols, onChangeCallback, opts) {
    var el = document.createElement("div");
    el.className = "su-ace-editor";
    container.appendChild(el);
    withAce(function() {
      var ed = ace.edit(el);
      ed.setTheme("ace/theme/tomorrow");
      ed.session.setMode("ace/mode/r");
      var maxL = (opts && opts.maxLines) || 1;
      ed.setOptions({ minLines: 1, maxLines: maxL, showLineNumbers: false, showPrintMargin: false,
        highlightActiveLine: false, tabSize: 2, fontSize: 14,
        enableLiveAutocompletion: true, enableBasicAutocompletion: true });
      ed.setValue(value || "", 1);
      ed.renderer.setScrollMargin(0, 0, 0, 0);
      ed.completers = [makeCompleter(cols)];
      ed.commands.on("afterExec", function(e) {
        if (e.command.name === "insertstring" || e.command.name === "Return") {
          var p = ed.getCursorPosition(), l = ed.session.getLine(p.row);
          if (l.substring(p.column - 2, p.column) === "()") ed.moveCursorTo(p.row, p.column - 1);
        }
      });
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

  function withSelectize(fn) {
    if (typeof $ !== "undefined" && $.fn && $.fn.selectize) { fn(); return; }
    var a = 0, t = setInterval(function() {
      a++;
      if (typeof $ !== "undefined" && $.fn && $.fn.selectize) { clearInterval(t); fn(); }
      if (a > 100) clearInterval(t);
    }, 100);
  }

  // Single-select for function picking
  function createFuncSelectize(container, selected, onChange) {
    var sel = document.createElement("select");
    sel.className = "su-selectize-func";
    container.appendChild(sel);

    var wrapper = {
      el: sel, api: null,
      getValue: function() { return wrapper.api ? wrapper.api.getValue() : ""; },
      destroy: function() { if (wrapper.api) wrapper.api.destroy(); }
    };

    withSelectize(function() {
      var $sel = $(sel).selectize({
        options: SUMMARY_FUNCS.map(function(v) { return { value: v, text: v }; }),
        items: selected ? [selected] : [SUMMARY_FUNCS[0]],
        maxItems: 1,
        placeholder: "Function...",
        onChange: function(value) { if (onChange) onChange(value); }
      });
      wrapper.api = $sel[0].selectize;
    });

    return wrapper;
  }

  // Single-select for column picking
  function createColumnSelectize(container, options, selected, onChange) {
    var sel = document.createElement("select");
    sel.className = "su-selectize-col";
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
      if (wrapper._pending) {
        wrapper.setOptions(wrapper._pending.opts, wrapper._pending.sel);
        delete wrapper._pending;
      }
    });

    return wrapper;
  }

  // Multi-select for .by grouping
  function createGroupBySelectize(container, options, selected, onChange) {
    var sel = document.createElement("select");
    sel.multiple = true;
    sel.className = "su-selectize-by";
    container.appendChild(sel);

    var wrapper = {
      el: sel, api: null,
      setOptions: function(opts, s) {
        if (!wrapper.api) { wrapper._pending = { opts: opts, sel: s }; return; }
        wrapper.api.clearOptions();
        opts.forEach(function(v) { wrapper.api.addOption({ value: v, text: v }); });
        wrapper.api.refreshOptions(false);
        if (s && s.length > 0) wrapper.api.setValue(s, true);
      },
      getValue: function() { return wrapper.api ? (wrapper.api.getValue() || []) : []; },
      destroy: function() { if (wrapper.api) wrapper.api.destroy(); }
    };

    withSelectize(function() {
      var $sel = $(sel).selectize({
        options: options.map(function(v) { return { value: v, text: v }; }),
        items: selected || [],
        plugins: ["remove_button", "drag_drop"],
        placeholder: "Select grouping columns\u2026",
        onChange: function(value) { if (onChange) onChange(value || []); }
      });
      wrapper.api = $sel[0].selectize;
      if (wrapper._pending) {
        wrapper.setOptions(wrapper._pending.opts, wrapper._pending.sel);
        delete wrapper._pending;
      }
    });

    return wrapper;
  }

  // ---------------------------------------------------------------------------
  // SummarizeUnified component
  // ---------------------------------------------------------------------------
  function SummarizeUnified(el) {
    this.el = el;
    this.summaries = [];
    this.nextId = 1;
    this.columnNames = [];
    this.byValues = [];
    this._callback = null;
    this._submitted = false;
    this._debounceTimer = null;
    this._bySelectize = null;

    this._buildDOM();
    this._addSimpleRow(null, null, null);
  }

  // Debounced auto-submit for simple row changes (300ms)
  SummarizeUnified.prototype._autoSubmit = function() {
    var self = this;
    clearTimeout(this._debounceTimer);
    this._debounceTimer = setTimeout(function() {
      self._submit();
    }, 300);
  };

  SummarizeUnified.prototype._buildDOM = function() {
    var self = this;

    // Card
    this.card = document.createElement("div");
    this.card.className = "su-card";
    this.el.appendChild(this.card);

    // Summaries list
    this.listEl = document.createElement("div");
    this.listEl.className = "su-summaries";
    this.card.appendChild(this.listEl);

    // Add row: [+ Add summary] [</>]
    var addRow = document.createElement("div");
    addRow.className = "su-add-row";

    var addSimpleLink = document.createElement("span");
    addSimpleLink.className = "su-add-link";
    addSimpleLink.innerHTML = '<span class="su-add-icon">' + ICON_PLUS + '</span> Add summary';
    addSimpleLink.addEventListener("click", function() { self._addSimpleRow(null, null, null); });
    addRow.appendChild(addSimpleLink);

    var addExprLink = document.createElement("span");
    addExprLink.className = "su-add-link-expr";
    addExprLink.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M10.478 1.647a.5.5 0 1 0-.956-.294l-4 13a.5.5 0 0 0 .956.294zM4.854 4.146a.5.5 0 0 1 0 .708L1.707 8l3.147 3.146a.5.5 0 0 1-.708.708l-3.5-3.5a.5.5 0 0 1 0-.708l3.5-3.5a.5.5 0 0 1 .708 0m6.292 0a.5.5 0 0 0 0 .708L14.293 8l-3.147 3.146a.5.5 0 0 0 .708.708l3.5-3.5a.5.5 0 0 0 0-.708l-3.5-3.5a.5.5 0 0 0-.708 0"/></svg>';
    addExprLink.title = "Add R expression";
    addExprLink.addEventListener("click", function() { self._addExprRow("", ""); });
    addRow.appendChild(addExprLink);

    this.card.appendChild(addRow);

    // Group by section (below the card)
    this.bySection = document.createElement("div");
    this.bySection.className = "su-by-section";

    var byLabel = document.createElement("span");
    byLabel.className = "su-by-label";
    byLabel.textContent = "Group by:";
    this.bySection.appendChild(byLabel);

    var byWrap = document.createElement("div");
    byWrap.className = "su-by-wrap";
    this.bySection.appendChild(byWrap);

    this._bySelectize = createGroupBySelectize(
      byWrap, this.columnNames, [],
      function(value) { self.byValues = value || []; self._autoSubmit(); }
    );

    this.el.appendChild(this.bySection);
  };

  // --- Simple rows: [name] = [func ▾] ( [col ▾] ) × ---

  SummarizeUnified.prototype._addSimpleRow = function(name, func, col) {
    var self = this;
    var id = this.nextId++;
    var summary = {
      id: id,
      type: "simple",
      name: name || "",
      func: func || SUMMARY_FUNCS[0],
      col: col || "",
      rowEl: null,
      _funcSelectize: null,
      _colSelectize: null,
      _colWrap: null,
      _nameInput: null,
      _ofLabel: null
    };

    var row = document.createElement("div");
    row.className = "su-row su-row-simple";
    row.setAttribute("data-summary-id", id);
    summary.rowEl = row;

    // Name input
    var nameInput = document.createElement("input");
    nameInput.type = "text";
    nameInput.className = "su-name-input";
    nameInput.placeholder = "name";
    nameInput.value = name || "";
    nameInput.addEventListener("input", function() {
      summary.name = nameInput.value;
      self._autoSubmit();
    });
    summary._nameInput = nameInput;
    row.appendChild(nameInput);

    // Equals sign
    var eqSign = document.createElement("span");
    eqSign.className = "su-eq-sign";
    eqSign.textContent = "=";
    row.appendChild(eqSign);

    // Function selectize
    var funcDiv = document.createElement("div");
    funcDiv.className = "su-func-wrap";
    row.appendChild(funcDiv);
    summary._funcSelectize = createFuncSelectize(
      funcDiv, func || SUMMARY_FUNCS[0],
      function(value) {
        summary.func = value;
        self._updateColVisibility(summary);
        self._autoSubmit();
      }
    );

    // "of" label
    var ofLabel = document.createElement("span");
    ofLabel.className = "su-of-label";
    ofLabel.textContent = "of";
    summary._ofLabel = ofLabel;
    row.appendChild(ofLabel);

    // Column selectize
    var colWrap = document.createElement("div");
    colWrap.className = "su-col-wrap";
    row.appendChild(colWrap);
    summary._colWrap = colWrap;
    summary._colSelectize = createColumnSelectize(
      colWrap, this.columnNames, col,
      function(value) {
        summary.col = value;
        self._autoSubmit();
      }
    );

    // Remove button
    var rmBtn = document.createElement("button");
    rmBtn.className = "su-row-remove";
    rmBtn.type = "button";
    rmBtn.innerHTML = ICON_X;
    rmBtn.addEventListener("click", function() {
      self._removeSummary(id);
      self._autoSubmit();
    });
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.summaries.push(summary);
    this._updateUI();
    this._updateColVisibility(summary);
  };

  // Hide column selectize when function is n()
  SummarizeUnified.prototype._updateColVisibility = function(summary) {
    var isNoCol = NO_COL_FUNCS.indexOf(summary.func) !== -1;
    if (summary._colWrap) {
      summary._colWrap.style.display = isNoCol ? "none" : "";
    }
    // Hide "of" label for n() — no column needed
    if (summary._ofLabel) {
      summary._ofLabel.style.display = isNoCol ? "none" : "";
    }
  };

  // --- Expression rows: [name] = [ACE editor] [Enter ↵] × ---

  SummarizeUnified.prototype._addExprRow = function(name, value) {
    var self = this;
    var id = this.nextId++;

    var row = document.createElement("div");
    row.className = "su-row su-row-expr";
    row.setAttribute("data-summary-id", id);

    // Name input
    var nameInput = document.createElement("input");
    nameInput.type = "text";
    nameInput.className = "su-name-input";
    nameInput.placeholder = "name";
    nameInput.value = name || "";
    row.appendChild(nameInput);

    // Equals sign
    var eqSign = document.createElement("span");
    eqSign.className = "su-eq-sign";
    eqSign.textContent = "=";
    row.appendChild(eqSign);

    // ACE editor
    var codeDiv = document.createElement("div");
    codeDiv.className = "su-row-content su-expr-code";
    row.appendChild(codeDiv);

    // Confirm button
    var confirmBtn = document.createElement("button");
    confirmBtn.className = "su-expr-confirm";
    confirmBtn.type = "button";
    confirmBtn.innerHTML = "Enter &#x21B5;";
    confirmBtn.title = "Apply expression";
    var doConfirm = function() {
      confirmBtn.classList.add("confirmed");
      confirmBtn.innerHTML = ICON_CONFIRM;
      self._submit();
    };
    confirmBtn.addEventListener("click", doConfirm);

    var exprEl = createAceEditor(codeDiv, value || "", this.columnNames, function() {
      confirmBtn.classList.remove("confirmed");
      confirmBtn.innerHTML = "Enter &#x21B5;";
    }, { maxLines: 10 });

    // Confirm on Enter in ACE editor
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

    // Remove button
    var rmBtn = document.createElement("button");
    rmBtn.className = "su-row-remove";
    rmBtn.type = "button";
    rmBtn.innerHTML = ICON_X;
    rmBtn.addEventListener("click", function() { self._removeSummary(id); });
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.summaries.push({
      id: id,
      type: "expr",
      name: name || "",
      exprEl: exprEl,
      _nameInput: nameInput,
      rowEl: row
    });
    this._updateUI();
  };

  // --- Shared ---

  SummarizeUnified.prototype._removeSummary = function(id) {
    if (this.summaries.length <= 1) return;

    var summary = null, idx = -1;
    for (var i = 0; i < this.summaries.length; i++) {
      if (this.summaries[i].id === id) { summary = this.summaries[i]; idx = i; break; }
    }
    if (!summary) return;

    if (summary._funcSelectize) summary._funcSelectize.destroy();
    if (summary._colSelectize) summary._colSelectize.destroy();
    if (summary.exprEl && summary.exprEl._aceEditor) summary.exprEl._aceEditor.destroy();
    if (summary.rowEl && summary.rowEl.parentNode) summary.rowEl.parentNode.removeChild(summary.rowEl);
    this.summaries.splice(idx, 1);
    this._updateUI();
  };

  SummarizeUnified.prototype._updateUI = function() {
    var total = this.summaries.length;
    var single = total <= 1;

    this.summaries.forEach(function(s) {
      var btn = s.rowEl.querySelector(".su-row-remove");
      if (btn) btn.style.visibility = single ? "hidden" : "visible";
    });
  };

  SummarizeUnified.prototype._compose = function() {
    var summaries = [];
    this.summaries.forEach(function(s) {
      if (s.type === "simple") {
        var name = (s._nameInput ? s._nameInput.value : s.name) || "";
        if (name === "") return;
        var func = s.func || "";
        if (func === "") return;
        var col = s.col || "";
        var isNoCol = NO_COL_FUNCS.indexOf(func) !== -1;
        if (!isNoCol && col === "") return;
        summaries.push({ type: "simple", name: name, func: func, col: col });
      } else if (s.type === "expr") {
        var ename = (s._nameInput ? s._nameInput.value : s.name) || "";
        if (ename === "") return;
        var val = s.exprEl && s.exprEl._aceEditor ? s.exprEl._aceEditor.getValue().trim() : "";
        if (val === "") return;
        summaries.push({ type: "expr", name: ename, expr: val });
      }
    });
    return { summaries: summaries, by: this.byValues || [] };
  };

  SummarizeUnified.prototype._submit = function() {
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  SummarizeUnified.prototype.getValue = function() {
    if (!this._submitted) return null;
    return this._compose();
  };

  SummarizeUnified.prototype.setValue = function(value) {};

  SummarizeUnified.prototype.updateColumns = function(columns) {
    this.columnNames = columns || [];
    var self = this;

    // Update column selectizes in simple rows
    this.summaries.forEach(function(s) {
      if (s.type === "simple" && s._colSelectize) {
        var current = s._colSelectize.getValue();
        s._colSelectize.setOptions(self.columnNames, current);
      }
    });

    // Update group by selectize
    if (this._bySelectize) {
      var currentBy = this._bySelectize.getValue();
      this._bySelectize.setOptions(this.columnNames, currentBy);
    }
  };

  // ---------------------------------------------------------------------------
  // Shiny input binding
  // ---------------------------------------------------------------------------
  var Binding = new Shiny.InputBinding();

  $.extend(Binding, {
    find: function(scope) { return $(scope).find(".summarize-unified-container"); },
    getId: function(el) { return el.id || null; },
    getValue: function(el) {
      if (!el._summarizeUnified) return null;
      return el._summarizeUnified.getValue();
    },
    setValue: function(el, value) {
      if (el._summarizeUnified) el._summarizeUnified.setValue(value);
    },
    subscribe: function(el, callback) {
      if (el._summarizeUnified) el._summarizeUnified._callback = function() { callback(true); };
    },
    unsubscribe: function(el) {
      if (el._summarizeUnified) el._summarizeUnified._callback = null;
    },
    initialize: function(el) {
      el._summarizeUnified = new SummarizeUnified(el);
      if (el._pendingColumns) {
        el._summarizeUnified.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);
    }
  });

  Shiny.inputBindings.register(Binding, "blockr.summarizeUnified");

  Shiny.addCustomMessageHandler("summarize-unified-update-columns", function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._summarizeUnified) {
      el._summarizeUnified.updateColumns(msg.columns);
    } else if (el) {
      el._pendingColumns = msg.columns;
    } else {
      var a = 0, t = setInterval(function() {
        a++;
        var el2 = document.getElementById(msg.id);
        if (el2 && el2._summarizeUnified) { el2._summarizeUnified.updateColumns(msg.columns); clearInterval(t); }
        else if (el2) { el2._pendingColumns = msg.columns; clearInterval(t); }
        if (a > 50) clearInterval(t);
      }, 100);
    }
  });

})();
