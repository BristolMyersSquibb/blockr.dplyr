// FilterJs — Custom Shiny input binding for JS-driven filter expressions.
// All condition-row management (add/remove/toggle) happens client-side.
// R is only contacted on Submit (to parse + eval the expression).
//
// UI design: unified card with global AND/OR toggle, condition rows with
// inline remove, and a "+" row at the bottom of the card.

(function() {
  "use strict";

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  function backtickIfNeeded(name) {
    if (/^[a-zA-Z.][a-zA-Z0-9._]*$/.test(name)) return name;
    return "`" + name + "`";
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
  }

  function createAceEditor(containerEl, value, columns) {
    var editorEl = document.createElement("div");
    editorEl.className = "filter-js-ace-editor";
    containerEl.appendChild(editorEl);

    withAce(function() {
      var editor = ace.edit(editorEl);
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
      editor.setValue(value || "TRUE", 1);
      editor.renderer.setScrollMargin(0, 0, 0, 0);
      editor.completers = [makeCompleter(columns)];

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
    if (editorEl && editorEl._aceEditor) {
      editorEl._aceEditor.completers = [makeCompleter(columns)];
    }
  }

  function destroyEditor(editorEl) {
    if (editorEl && editorEl._aceEditor) {
      editorEl._aceEditor.destroy();
      editorEl._aceEditor = null;
    }
  }

  // ---------------------------------------------------------------------------
  // SVG icons
  // ---------------------------------------------------------------------------

  var ICON_X = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>';
  var ICON_PLUS = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>';

  // ---------------------------------------------------------------------------
  // FilterJs component
  // ---------------------------------------------------------------------------

  function FilterJs(el) {
    this.el = el;
    this.conditions = [];  // [{id, editorEl, rowEl}]
    this.operator = "&";   // uniform: "&" or "|"
    this.nextId = 1;
    this.columns = [];
    this._callback = null;
    this._submitted = false;

    var initial = el.getAttribute("data-value") || "TRUE";
    this._initValue = initial;

    this._buildDOM();
    this._addCondition(initial);
  }

  FilterJs.prototype._buildDOM = function() {
    var self = this;

    // Header bar: operator toggle + submit
    var headerEl = document.createElement("div");
    headerEl.className = "filter-js-header";

    // Operator toggle button
    this.opToggleEl = document.createElement("button");
    this.opToggleEl.type = "button";
    this.opToggleEl.className = "filter-js-op-toggle";
    this.opToggleEl.textContent = "AND";
    this.opToggleEl.title = "Toggle between AND / OR (applies to all conditions)";
    this.opToggleEl.addEventListener("click", function() {
      self.operator = self.operator === "&" ? "|" : "&";
      self.opToggleEl.textContent = self.operator === "&" ? "AND" : "OR";
      self._updateSeparators();
    });
    headerEl.appendChild(this.opToggleEl);

    // Submit button
    var submitBtn = document.createElement("button");
    submitBtn.className = "btn btn-primary btn-sm filter-js-submit";
    submitBtn.type = "button";
    submitBtn.textContent = "Submit";
    submitBtn.addEventListener("click", function() { self._submit(); });
    headerEl.appendChild(submitBtn);

    this.el.appendChild(headerEl);

    // Card container for conditions
    this.cardEl = document.createElement("div");
    this.cardEl.className = "filter-js-card";
    this.el.appendChild(this.cardEl);

    // Conditions list (inside card)
    this.conditionsEl = document.createElement("div");
    this.conditionsEl.className = "filter-js-conditions";
    this.cardEl.appendChild(this.conditionsEl);

    // Add row (inside card, at bottom)
    var addRow = document.createElement("div");
    addRow.className = "filter-js-add-row";
    addRow.addEventListener("click", function() { self._addCondition(""); });

    var addIcon = document.createElement("span");
    addIcon.className = "filter-js-add-icon";
    addIcon.innerHTML = ICON_PLUS;
    addRow.appendChild(addIcon);

    var addLabel = document.createElement("span");
    addLabel.className = "filter-js-add-label";
    addLabel.textContent = "Add condition";
    addRow.appendChild(addLabel);

    this.cardEl.appendChild(addRow);
  };

  FilterJs.prototype._addCondition = function(value) {
    var self = this;
    var id = this.nextId++;

    // Condition row
    var rowEl = document.createElement("div");
    rowEl.className = "filter-js-row";
    rowEl.setAttribute("data-cond-id", id);

    // Code editor area
    var codeDiv = document.createElement("div");
    codeDiv.className = "filter-js-row-code";
    rowEl.appendChild(codeDiv);

    var editorEl = createAceEditor(codeDiv, value, this.columns);

    // Remove button
    var removeBtn = document.createElement("button");
    removeBtn.className = "filter-js-row-remove";
    removeBtn.type = "button";
    removeBtn.innerHTML = ICON_X;
    removeBtn.title = "Remove condition";
    removeBtn.addEventListener("click", function() { self._removeCondition(id); });
    rowEl.appendChild(removeBtn);

    this.conditionsEl.appendChild(rowEl);
    this.conditions.push({ id: id, editorEl: editorEl, rowEl: rowEl });
    this._updateRemoveButtons();
    this._updateSeparators();
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
    if (cond.rowEl.parentNode) cond.rowEl.parentNode.removeChild(cond.rowEl);

    this.conditions.splice(idx, 1);
    this._updateRemoveButtons();
    this._updateSeparators();
  };

  FilterJs.prototype._updateRemoveButtons = function() {
    var single = this.conditions.length <= 1;
    this.conditions.forEach(function(c) {
      var btn = c.rowEl.querySelector(".filter-js-row-remove");
      if (btn) btn.style.visibility = single ? "hidden" : "visible";
    });
  };

  FilterJs.prototype._updateSeparators = function() {
    // Show/hide the operator toggle based on condition count
    this.opToggleEl.style.visibility = this.conditions.length > 1 ? "visible" : "hidden";
    // Update the CSS class for separator labels
    var label = this.operator === "&" ? "AND" : "OR";
    this.conditionsEl.setAttribute("data-separator", label);
  };

  FilterJs.prototype._compose = function() {
    var op = this.operator;
    var parts = [];
    for (var i = 0; i < this.conditions.length; i++) {
      var v = getEditorValue(this.conditions[i].editorEl) || "TRUE";
      parts.push(v);
    }
    if (parts.length === 0) return "TRUE";
    return parts.join(" " + op + " ");
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
    while (this.conditions.length > 0) {
      var c = this.conditions.pop();
      destroyEditor(c.editorEl);
    }
    this.conditionsEl.innerHTML = "";
    this._addCondition(value || "TRUE");
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  FilterJs.prototype.updateColumns = function(columns) {
    this.columns = columns || [];
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
