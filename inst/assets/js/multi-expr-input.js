// MultiExpr — Shared JS component for multi-row expression editors.
// Used by filter_js, mutate_js, and summarize_js blocks.
//
// Supports two row modes:
//   "expr"   — single expression per row (for filter)
//   "kvexpr" — name = expression per row (for mutate, summarize)
//
// All row management happens client-side. R only receives the final
// composed value on Submit.

(function() {
  "use strict";

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  function backtickIfNeeded(name) {
    if (/^[a-zA-Z.][a-zA-Z0-9._]*$/.test(name)) return name;
    return "`" + name + "`";
  }

  // Default function categories for ACE autocomplete
  var defaultCategories = {
    arithmetic: ["abs", "sign", "ceiling", "floor", "round", "trunc",
                 "log", "log2", "log10", "exp", "sqrt"],
    aggregate:  ["mean", "sum", "min", "max"],
    offset:     ["lead", "lag", "cumsum", "cumprod", "cummin", "cummax"],
    logical:    ["if_else", "case_when"],
    string:     ["str_c", "paste", "paste0", "str_sub", "str_to_lower", "str_to_upper"],
    ranking:    ["row_number", "min_rank", "dense_rank", "percent_rank", "ntile"]
  };

  // ---------------------------------------------------------------------------
  // ACE editor
  // ---------------------------------------------------------------------------

  function withAce(fn) {
    if (typeof ace !== "undefined") { fn(); return; }
    var attempts = 0;
    var timer = setInterval(function() {
      attempts++;
      if (typeof ace !== "undefined") { clearInterval(timer); fn(); }
      if (attempts > 50) clearInterval(timer);
    }, 100);
  }

  function makeCompleter(columns) {
    return {
      getCompletions: function(_ed, _sess, _pos, _prefix, cb) {
        var cats = Object.assign({}, defaultCategories);
        cats.column = (columns || []).map(backtickIfNeeded);
        var list = [];
        Object.keys(cats).forEach(function(cat) {
          cats[cat].forEach(function(fn) {
            var isCol = cat === "column";
            list.push({
              caption: fn,
              value: fn + (isCol ? "" : "()"),
              meta: cat,
              score: isCol ? 1001 : 1000
            });
          });
        });
        list.sort(function(a, b) {
          if (a.score !== b.score) return b.score - a.score;
          if (a.meta === b.meta) return a.caption.localeCompare(b.caption);
          return a.meta.localeCompare(b.meta);
        });
        cb(null, list);
      }
    };
  }

  function createEditor(containerEl, value, columns, opts) {
    var el = document.createElement("div");
    el.className = "multi-expr-ace";
    containerEl.appendChild(el);

    withAce(function() {
      var editor = ace.edit(el);
      editor.setTheme("ace/theme/tomorrow");
      editor.session.setMode("ace/mode/r");
      editor.setOptions({
        maxLines: 1,
        showLineNumbers: false,
        showPrintMargin: false,
        highlightActiveLine: false,
        tabSize: 2,
        fontSize: 14,
        enableLiveAutocompletion: opts && opts.autocomplete !== false,
        enableBasicAutocompletion: opts && opts.autocomplete !== false
      });
      editor.setValue(value || "", 1);
      editor.renderer.setScrollMargin(0, 0, 0, 0);

      if (opts && opts.autocomplete !== false) {
        editor.completers = [makeCompleter(columns)];
      } else {
        editor.completers = [];
      }

      // Cursor-inside-parens after function insert
      editor.commands.on("afterExec", function(e) {
        if (e.command.name === "insertstring" || e.command.name === "Return") {
          var pos = editor.getCursorPosition();
          var line = editor.session.getLine(pos.row);
          if (line.substring(pos.column - 2, pos.column) === "()") {
            editor.moveCursorTo(pos.row, pos.column - 1);
          }
        }
      });

      if (opts && opts.placeholder) {
        editor.setOptions({ placeholder: opts.placeholder });
      }

      el._aceEditor = editor;
    });

    return el;
  }

  function getVal(el) {
    return (el && el._aceEditor) ? el._aceEditor.getValue().trim() : "";
  }

  function updateColumns(el, columns) {
    if (el && el._aceEditor) {
      el._aceEditor.completers = [makeCompleter(columns)];
    }
  }

  function destroyEd(el) {
    if (el && el._aceEditor) {
      el._aceEditor.destroy();
      el._aceEditor = null;
    }
  }

  // ---------------------------------------------------------------------------
  // SVG icons
  // ---------------------------------------------------------------------------

  var ICON_X = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>';
  var ICON_PLUS = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>';

  // ---------------------------------------------------------------------------
  // MultiExpr component
  //
  // Config options (read from data attributes):
  //   data-mode       "expr" (filter) or "kvexpr" (mutate/summarize)
  //   data-value      initial value (JSON for kvexpr, string for expr)
  //   data-operator   "&" or "|" (only for expr mode)
  //   data-add-label  label for the add row (default: "Add condition")
  // ---------------------------------------------------------------------------

  function MultiExpr(el) {
    this.el = el;
    this.mode = el.getAttribute("data-mode") || "expr";
    this.conditions = [];  // [{id, nameEl?, exprEl, rowEl}]
    this.operator = el.getAttribute("data-operator") || "&";
    this.nextId = 1;
    this.columns = [];
    this._callback = null;
    this._submitted = false;
    this.addLabel = el.getAttribute("data-add-label") || "Add condition";

    // Parse initial value
    var rawValue = el.getAttribute("data-value") || "";
    if (this.mode === "kvexpr") {
      try {
        this._initValue = JSON.parse(rawValue);
      } catch (e) {
        this._initValue = { new_col: "1" };
      }
    } else {
      this._initValue = rawValue || "TRUE";
    }

    this._buildDOM();
    this._initConditions();
  }

  MultiExpr.prototype._buildDOM = function() {
    var self = this;

    // Header: operator toggle (expr only) + submit
    var header = document.createElement("div");
    header.className = "multi-expr-header";

    if (this.mode === "expr") {
      this.opToggle = document.createElement("button");
      this.opToggle.type = "button";
      this.opToggle.className = "multi-expr-op-toggle";
      this.opToggle.textContent = this.operator === "&" ? "AND" : "OR";
      this.opToggle.title = "Toggle between AND / OR (applies to all conditions)";
      this.opToggle.addEventListener("click", function() {
        self.operator = self.operator === "&" ? "|" : "&";
        self.opToggle.textContent = self.operator === "&" ? "AND" : "OR";
      });
      header.appendChild(this.opToggle);
    } else {
      // Spacer for layout
      header.appendChild(document.createElement("span"));
    }

    var submitBtn = document.createElement("button");
    submitBtn.className = "btn btn-primary btn-sm multi-expr-submit";
    submitBtn.type = "button";
    submitBtn.textContent = "Submit";
    submitBtn.addEventListener("click", function() { self._submit(); });
    header.appendChild(submitBtn);

    this.el.appendChild(header);

    // Card
    this.card = document.createElement("div");
    this.card.className = "multi-expr-card";
    this.el.appendChild(this.card);

    // Conditions list
    this.listEl = document.createElement("div");
    this.listEl.className = "multi-expr-list";
    this.card.appendChild(this.listEl);

    // Add row
    var addRow = document.createElement("div");
    addRow.className = "multi-expr-add-row";
    addRow.addEventListener("click", function() {
      if (self.mode === "kvexpr") {
        self._addKvRow("new_col", "");
      } else {
        self._addExprRow("");
      }
    });

    var icon = document.createElement("span");
    icon.className = "multi-expr-add-icon";
    icon.innerHTML = ICON_PLUS;
    addRow.appendChild(icon);

    var label = document.createElement("span");
    label.textContent = this.addLabel;
    addRow.appendChild(label);

    this.card.appendChild(addRow);
  };

  MultiExpr.prototype._initConditions = function() {
    if (this.mode === "kvexpr") {
      var vals = this._initValue;
      var keys = Object.keys(vals);
      if (keys.length === 0) {
        this._addKvRow("new_col", "1");
      } else {
        for (var i = 0; i < keys.length; i++) {
          this._addKvRow(keys[i], vals[keys[i]]);
        }
      }
    } else {
      this._addExprRow(this._initValue);
    }
  };

  // --- Expr mode (filter) ---

  MultiExpr.prototype._addExprRow = function(value) {
    var self = this;
    var id = this.nextId++;

    var row = document.createElement("div");
    row.className = "multi-expr-row";
    row.setAttribute("data-cond-id", id);

    var codeDiv = document.createElement("div");
    codeDiv.className = "multi-expr-row-code";
    row.appendChild(codeDiv);

    var exprEl = createEditor(codeDiv, value, this.columns);

    var rmBtn = this._makeRemoveBtn(id);
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.conditions.push({ id: id, exprEl: exprEl, rowEl: row });
    this._updateUI();
  };

  // --- KV mode (mutate/summarize) ---

  MultiExpr.prototype._addKvRow = function(name, value) {
    var self = this;
    var id = this.nextId++;

    var row = document.createElement("div");
    row.className = "multi-expr-row multi-expr-row-kv";
    row.setAttribute("data-cond-id", id);

    // Name editor (no autocomplete, narrow)
    var nameDiv = document.createElement("div");
    nameDiv.className = "multi-expr-row-name";
    row.appendChild(nameDiv);
    var nameEl = createEditor(nameDiv, name, [], { autocomplete: false });

    // Equals separator
    var eqSpan = document.createElement("span");
    eqSpan.className = "multi-expr-row-eq";
    eqSpan.textContent = "=";
    row.appendChild(eqSpan);

    // Value editor (with autocomplete, wide)
    var codeDiv = document.createElement("div");
    codeDiv.className = "multi-expr-row-code";
    row.appendChild(codeDiv);
    var exprEl = createEditor(codeDiv, value, this.columns);

    var rmBtn = this._makeRemoveBtn(id);
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.conditions.push({ id: id, nameEl: nameEl, exprEl: exprEl, rowEl: row });
    this._updateUI();
  };

  // --- Shared ---

  MultiExpr.prototype._makeRemoveBtn = function(id) {
    var self = this;
    var btn = document.createElement("button");
    btn.className = "multi-expr-row-remove";
    btn.type = "button";
    btn.innerHTML = ICON_X;
    btn.title = "Remove";
    btn.addEventListener("click", function() { self._remove(id); });
    return btn;
  };

  MultiExpr.prototype._remove = function(id) {
    if (this.conditions.length <= 1) return;
    var idx = -1;
    for (var i = 0; i < this.conditions.length; i++) {
      if (this.conditions[i].id === id) { idx = i; break; }
    }
    if (idx === -1) return;

    var c = this.conditions[idx];
    if (c.nameEl) destroyEd(c.nameEl);
    destroyEd(c.exprEl);
    if (c.rowEl.parentNode) c.rowEl.parentNode.removeChild(c.rowEl);
    this.conditions.splice(idx, 1);
    this._updateUI();
  };

  MultiExpr.prototype._updateUI = function() {
    var single = this.conditions.length <= 1;
    this.conditions.forEach(function(c) {
      var btn = c.rowEl.querySelector(".multi-expr-row-remove");
      if (btn) btn.style.visibility = single ? "hidden" : "visible";
    });
    // Show/hide operator toggle
    if (this.opToggle) {
      this.opToggle.style.visibility = this.conditions.length > 1 ? "visible" : "hidden";
    }
  };

  MultiExpr.prototype._compose = function() {
    if (this.mode === "kvexpr") {
      return this._composeKv();
    }
    return this._composeExpr();
  };

  MultiExpr.prototype._composeExpr = function() {
    var parts = [];
    for (var i = 0; i < this.conditions.length; i++) {
      var v = getVal(this.conditions[i].exprEl) || "TRUE";
      parts.push(v);
    }
    if (parts.length === 0) return "TRUE";
    return parts.join(" " + this.operator + " ");
  };

  MultiExpr.prototype._composeKv = function() {
    var result = {};
    for (var i = 0; i < this.conditions.length; i++) {
      var c = this.conditions[i];
      var name = getVal(c.nameEl) || "new_col";
      var val = getVal(c.exprEl) || "1";
      // Handle duplicate names by appending suffix
      var finalName = name;
      var n = 1;
      while (result.hasOwnProperty(finalName)) {
        finalName = name + "_" + n;
        n++;
      }
      result[finalName] = val;
    }
    return result;
  };

  MultiExpr.prototype._submit = function() {
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  MultiExpr.prototype.getValue = function() {
    if (!this._submitted) return this._initValue;
    return this._compose();
  };

  MultiExpr.prototype.setValue = function(value) {
    // Clear existing
    while (this.conditions.length > 0) {
      var c = this.conditions.pop();
      if (c.nameEl) destroyEd(c.nameEl);
      destroyEd(c.exprEl);
    }
    this.listEl.innerHTML = "";

    if (this.mode === "kvexpr") {
      var obj = (typeof value === "object" && value !== null) ? value : { new_col: "1" };
      var keys = Object.keys(obj);
      for (var i = 0; i < keys.length; i++) {
        this._addKvRow(keys[i], obj[keys[i]]);
      }
    } else {
      this._addExprRow(value || "TRUE");
    }
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  MultiExpr.prototype.updateColumns = function(cols) {
    this.columns = cols || [];
    this.conditions.forEach(function(c) {
      updateColumns(c.exprEl, cols);
      // Don't update name editors — they don't have column autocomplete
    });
  };

  // ---------------------------------------------------------------------------
  // Shiny input binding
  // ---------------------------------------------------------------------------

  var MultiExprBinding = new Shiny.InputBinding();

  $.extend(MultiExprBinding, {
    find: function(scope) {
      return $(scope).find(".multi-expr-container");
    },
    getId: function(el) {
      return el.id || null;
    },
    getValue: function(el) {
      if (!el._multiExpr) {
        var raw = el.getAttribute("data-value");
        if (el.getAttribute("data-mode") === "kvexpr") {
          try { return JSON.parse(raw); } catch (e) { return { new_col: "1" }; }
        }
        return raw || "TRUE";
      }
      return el._multiExpr.getValue();
    },
    setValue: function(el, value) {
      if (el._multiExpr) el._multiExpr.setValue(value);
    },
    subscribe: function(el, callback) {
      if (el._multiExpr) {
        el._multiExpr._callback = function() { callback(true); };
      }
    },
    unsubscribe: function(el) {
      if (el._multiExpr) el._multiExpr._callback = null;
    },
    initialize: function(el) {
      el._multiExpr = new MultiExpr(el);
      // Apply columns that arrived before initialization
      if (el._pendingColumns) {
        el._multiExpr.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
      }
    }
  });

  Shiny.inputBindings.register(MultiExprBinding, "blockr.multiExpr");

  // ---------------------------------------------------------------------------
  // Custom message handler for column updates from R
  // ---------------------------------------------------------------------------

  Shiny.addCustomMessageHandler("multi-expr-update-columns", function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._multiExpr) {
      el._multiExpr.updateColumns(msg.columns);
    } else if (el) {
      // Element exists but not yet initialized — queue columns
      el._pendingColumns = msg.columns;
    } else {
      // Element doesn't exist yet (dock board lazy render) — retry
      var attempts = 0;
      var timer = setInterval(function() {
        attempts++;
        var el2 = document.getElementById(msg.id);
        if (el2 && el2._multiExpr) {
          el2._multiExpr.updateColumns(msg.columns);
          clearInterval(timer);
        } else if (el2) {
          el2._pendingColumns = msg.columns;
          clearInterval(timer);
        }
        if (attempts > 50) clearInterval(timer); // give up after 5s
      }, 100);
    }
  });

})();
