// FilterJs — Custom Shiny input binding for JS-driven filter expressions.
// All condition-row management (add/remove/toggle) happens client-side.
// R is only contacted on Submit (to parse + eval the expression).

(function() {
  "use strict";

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  function backtickIfNeeded(name) {
    // Wrap non-syntactic column names in backticks (same logic as R side)
    if (/^[a-zA-Z.][a-zA-Z0-9._]*$/.test(name)) return name;
    return "`" + name + "`";
  }

  function makeId(prefix, n) {
    return prefix + "-cond-" + n;
  }

  // Default function categories for autocomplete (mirrors get_default_categories())
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
  // ACE editor helpers
  // ---------------------------------------------------------------------------

  // Wait for ACE to be available, then call fn
  function withAce(fn) {
    if (typeof ace !== "undefined") { fn(); return; }
    var attempts = 0;
    var timer = setInterval(function() {
      attempts++;
      if (typeof ace !== "undefined") { clearInterval(timer); fn(); }
      if (attempts > 50) clearInterval(timer); // give up after 5s
    }, 100);
  }

  function createAceEditor(containerEl, value, columns) {
    var editorEl = document.createElement("div");
    editorEl.className = "filter-js-ace-editor";
    containerEl.appendChild(editorEl);

    var editor = null;
    withAce(function() {
      editor = ace.edit(editorEl);
      editor.setTheme("ace/theme/tomorrow");
      editor.session.setMode("ace/mode/r");
      editor.setOptions({
        maxLines: 1,
        showLineNumbers: false,
        showPrintMargin: false,
        highlightActiveLine: false,
        tabSize: 2,
        fontSize: 14,
        enableLiveAutocompletion: true,
        enableBasicAutocompletion: true
      });
      editor.setValue(value || "TRUE", 1); // 1 = move cursor to end
      editor.renderer.setScrollMargin(0, 0, 0, 0);

      // Custom completer scoped to this editor's container
      var completer = {
        getCompletions: function(_editor, _session, _pos, _prefix, callback) {
          var cats = Object.assign({}, defaultCategories);
          cats.column = (columns || []).map(backtickIfNeeded);
          var wordList = [];
          Object.keys(cats).forEach(function(category) {
            cats[category].forEach(function(fn) {
              var isCol = category === "column";
              wordList.push({
                caption: fn,
                value: fn + (isCol ? "" : "()"),
                meta: category,
                score: isCol ? 1001 : 1000
              });
            });
          });
          wordList.sort(function(a, b) {
            if (a.score !== b.score) return b.score - a.score;
            if (a.meta === b.meta) return a.caption.localeCompare(b.caption);
            return a.meta.localeCompare(b.meta);
          });
          callback(null, wordList);
        }
      };
      editor.completers = [completer];

      // Move cursor inside parens after inserting a function
      editor.commands.on("afterExec", function(e) {
        if (e.command.name === "insertstring" || e.command.name === "Return") {
          var pos = editor.getCursorPosition();
          var line = editor.session.getLine(pos.row);
          if (line.substring(pos.column - 2, pos.column) === "()") {
            editor.moveCursorTo(pos.row, pos.column - 1);
          }
        }
      });

      // Store editor reference
      editorEl._aceEditor = editor;
    });

    return editorEl;
  }

  function getEditorValue(editorEl) {
    if (editorEl && editorEl._aceEditor) {
      return editorEl._aceEditor.getValue().trim();
    }
    return "";
  }

  function setEditorColumns(editorEl, columns) {
    // The completer closure captures the columns array by reference through
    // the _filterJsColumns property. We update it here so the next
    // completion request sees the new columns.
    if (editorEl && editorEl._aceEditor) {
      editorEl._aceEditor.completers = [{
        getCompletions: function(_editor, _session, _pos, _prefix, callback) {
          var cats = Object.assign({}, defaultCategories);
          cats.column = (columns || []).map(backtickIfNeeded);
          var wordList = [];
          Object.keys(cats).forEach(function(category) {
            cats[category].forEach(function(fn) {
              var isCol = category === "column";
              wordList.push({
                caption: fn,
                value: fn + (isCol ? "" : "()"),
                meta: category,
                score: isCol ? 1001 : 1000
              });
            });
          });
          wordList.sort(function(a, b) {
            if (a.score !== b.score) return b.score - a.score;
            if (a.meta === b.meta) return a.caption.localeCompare(b.caption);
            return a.meta.localeCompare(b.meta);
          });
          callback(null, wordList);
        }
      }];
    }
  }

  function destroyEditor(editorEl) {
    if (editorEl && editorEl._aceEditor) {
      editorEl._aceEditor.destroy();
      editorEl._aceEditor = null;
    }
  }

  // ---------------------------------------------------------------------------
  // FilterJs "component" — manages state + DOM for one input binding instance
  // ---------------------------------------------------------------------------

  function FilterJs(el) {
    this.el = el;
    this.conditions = [];  // [{id: Number, editorEl: Element}]
    this.operators = [];   // "&" or "|"
    this.nextId = 1;
    this.columns = [];
    this._callback = null; // Shiny subscribe callback
    this._submitted = false;

    // Read initial value from data attribute
    var initial = el.getAttribute("data-value") || "TRUE";
    this._initValue = initial;

    // Build the DOM skeleton
    this._buildDOM();
    // Add the initial condition
    this._addCondition(initial);
  }

  FilterJs.prototype._buildDOM = function() {
    var self = this;

    // Conditions container
    this.conditionsEl = document.createElement("div");
    this.conditionsEl.className = "filter-js-conditions";
    this.el.appendChild(this.conditionsEl);

    // Actions bar (Add Condition + Submit)
    var actionsEl = document.createElement("div");
    actionsEl.className = "blockr-multi-actions";

    var addBtn = document.createElement("button");
    addBtn.className = "btn btn-outline-secondary btn-sm";
    addBtn.type = "button";
    addBtn.innerHTML = '<i class="fa fa-plus"></i> Add Condition';
    addBtn.addEventListener("click", function() { self._addCondition("TRUE"); });

    var submitBtn = document.createElement("button");
    submitBtn.className = "btn btn-primary btn-sm";
    submitBtn.type = "button";
    submitBtn.textContent = "Submit";
    submitBtn.addEventListener("click", function() { self._submit(); });

    actionsEl.appendChild(addBtn);
    actionsEl.appendChild(submitBtn);
    this.el.appendChild(actionsEl);
  };

  FilterJs.prototype._addCondition = function(value) {
    var self = this;
    var id = this.nextId++;
    var idx = this.conditions.length;

    // If not the first condition, add an operator row before this condition
    if (idx > 0) {
      var opRow = this._createOperatorRow(idx - 1);
      this.conditionsEl.appendChild(opRow);
      this.operators.push("&");
    }

    // Condition row
    var rowEl = document.createElement("div");
    rowEl.className = "input-group mb-2 multi-filter-condition border border-dark-subtle rounded";
    rowEl.setAttribute("data-cond-id", id);

    var codeDiv = document.createElement("div");
    codeDiv.className = "condition-code";
    rowEl.appendChild(codeDiv);

    var editorEl = createAceEditor(codeDiv, value, this.columns);

    // Remove button (only if > 1 condition)
    var removeBtn = document.createElement("button");
    removeBtn.className = "blockr-btn-icon";
    removeBtn.type = "button";
    removeBtn.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-x-lg" viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>';
    removeBtn.addEventListener("click", function() { self._removeCondition(id); });
    rowEl.appendChild(removeBtn);

    this.conditionsEl.appendChild(rowEl);
    this.conditions.push({ id: id, editorEl: editorEl, rowEl: rowEl });
    this._updateRemoveButtons();
  };

  FilterJs.prototype._createOperatorRow = function(opIndex) {
    var self = this;
    var opRow = document.createElement("div");
    opRow.className = "d-flex justify-content-start my-2 filter-js-operator-row";
    opRow.setAttribute("data-op-index", opIndex);

    var select = document.createElement("select");
    select.className = "form-select form-select-sm";
    select.style.width = "80px";

    var optAnd = document.createElement("option");
    optAnd.value = "&";
    optAnd.textContent = "AND";
    optAnd.selected = true;

    var optOr = document.createElement("option");
    optOr.value = "|";
    optOr.textContent = "OR";

    select.appendChild(optAnd);
    select.appendChild(optOr);

    select.addEventListener("change", function() {
      // Find the actual operator index based on position in DOM
      var opRows = self.conditionsEl.querySelectorAll(".filter-js-operator-row");
      for (var i = 0; i < opRows.length; i++) {
        if (opRows[i] === opRow) {
          self.operators[i] = select.value;
          break;
        }
      }
    });

    opRow.appendChild(select);
    return opRow;
  };

  FilterJs.prototype._removeCondition = function(id) {
    if (this.conditions.length <= 1) return;

    var idx = -1;
    for (var i = 0; i < this.conditions.length; i++) {
      if (this.conditions[i].id === id) { idx = i; break; }
    }
    if (idx === -1) return;

    var cond = this.conditions[idx];
    destroyEditor(cond.editorEl);

    // Remove the condition row from DOM
    if (cond.rowEl.parentNode) cond.rowEl.parentNode.removeChild(cond.rowEl);

    // Remove the associated operator row
    // If removing first condition, remove the operator AFTER it
    // If removing any other, remove the operator BEFORE it
    var opRows = this.conditionsEl.querySelectorAll(".filter-js-operator-row");
    var opIdx = idx > 0 ? idx - 1 : 0;
    if (opRows.length > 0 && opIdx < opRows.length) {
      opRows[opIdx].parentNode.removeChild(opRows[opIdx]);
    }

    // Update arrays
    this.conditions.splice(idx, 1);
    if (this.operators.length > 0) {
      this.operators.splice(idx > 0 ? idx - 1 : 0, 1);
    }

    this._updateRemoveButtons();
  };

  FilterJs.prototype._updateRemoveButtons = function() {
    var single = this.conditions.length <= 1;
    this.conditions.forEach(function(c) {
      var btn = c.rowEl.querySelector(".blockr-btn-icon");
      if (btn) btn.style.display = single ? "none" : "";
    });
  };

  FilterJs.prototype._compose = function() {
    if (this.conditions.length === 0) return "TRUE";
    if (this.conditions.length === 1) {
      var val = getEditorValue(this.conditions[0].editorEl);
      return val || "TRUE";
    }

    var parts = [];
    for (var i = 0; i < this.conditions.length; i++) {
      var v = getEditorValue(this.conditions[i].editorEl) || "TRUE";
      if (i > 0) {
        var op = (i - 1 < this.operators.length) ? this.operators[i - 1] : "&";
        parts.push(op);
      }
      parts.push(v);
    }
    return parts.join(" ");
  };

  FilterJs.prototype._submit = function() {
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  FilterJs.prototype.getValue = function() {
    if (!this._submitted) return this._initValue;
    return this._compose();
  };

  FilterJs.prototype.setValue = function(value) {
    // External ctrl: reset to a single condition with the given expression
    // Destroy existing conditions
    while (this.conditions.length > 0) {
      var c = this.conditions.pop();
      destroyEditor(c.editorEl);
    }
    this.operators = [];
    this.conditionsEl.innerHTML = "";
    this._addCondition(value || "TRUE");
    // Auto-submit on external set
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  FilterJs.prototype.updateColumns = function(columns) {
    this.columns = columns || [];
    // Update all existing editors
    this.conditions.forEach(function(c) {
      setEditorColumns(c.editorEl, columns);
    });
  };

  // ---------------------------------------------------------------------------
  // Shiny input binding
  // ---------------------------------------------------------------------------

  var FilterJsBinding = new Shiny.InputBinding();

  $.extend(FilterJsBinding, {

    find: function(scope) {
      return $(scope).find(".filter-js-container");
    },

    getId: function(el) {
      return el.id || null;
    },

    getValue: function(el) {
      if (!el._filterJs) return el.getAttribute("data-value") || "TRUE";
      return el._filterJs.getValue();
    },

    setValue: function(el, value) {
      if (el._filterJs) el._filterJs.setValue(value);
    },

    subscribe: function(el, callback) {
      if (el._filterJs) {
        el._filterJs._callback = function() { callback(true); };
      }
    },

    unsubscribe: function(el) {
      if (el._filterJs) el._filterJs._callback = null;
    },

    initialize: function(el) {
      el._filterJs = new FilterJs(el);
    },

    receiveMessage: function(el, data) {
      // Handle messages from R (e.g., external ctrl setValue)
      if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
      }
    }
  });

  Shiny.inputBindings.register(FilterJsBinding, "blockr.filterJs");

  // ---------------------------------------------------------------------------
  // Custom message handler for column updates from R
  // ---------------------------------------------------------------------------

  Shiny.addCustomMessageHandler("filter-js-update-columns", function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._filterJs) {
      el._filterJs.updateColumns(msg.columns);
    }
  });

})();
