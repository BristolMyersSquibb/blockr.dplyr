// MutateUnified — JS-driven unified mutate block.
// Each row: [name_input] = [ACE editor expression] [Enter] [x]
// Expression-only mode with confirm-on-Enter pattern.

(function() {
  "use strict";

  // ---------------------------------------------------------------------------
  // SVG icons
  // ---------------------------------------------------------------------------
  var ICON_X = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>';
  var ICON_PLUS = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>';
  var ICON_CONFIRM = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>';

  // ---------------------------------------------------------------------------
  // ACE editor helpers
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
    el.className = "mu-ace-editor";
    container.appendChild(el);
    withAce(function() {
      var ed = ace.edit(el);
      ed.setTheme("ace/theme/tomorrow");
      ed.session.setMode("ace/mode/r");
      var maxL = (opts && opts.maxLines) || 10;
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
      if (onChangeCallback) {
        ed.session.on("change", onChangeCallback);
      }
      el._aceEditor = ed;
    });
    return el;
  }

  // ---------------------------------------------------------------------------
  // MutateUnified component
  // ---------------------------------------------------------------------------
  function MutateUnified(el) {
    this.el = el;
    this.rows = [];
    this.nextId = 1;
    this.columnNames = [];
    this._callback = null;
    this._submitted = false;

    this._buildDOM();
  }

  MutateUnified.prototype._buildDOM = function() {
    var self = this;

    // Card
    this.card = document.createElement("div");
    this.card.className = "mu-card";
    this.el.appendChild(this.card);

    // Rows list
    this.listEl = document.createElement("div");
    this.listEl.className = "mu-rows";
    this.card.appendChild(this.listEl);

    // Add row footer
    var addRow = document.createElement("div");
    addRow.className = "mu-add-row";

    var addLink = document.createElement("span");
    addLink.className = "mu-add-link";
    addLink.innerHTML = '<span class="mu-add-icon">' + ICON_PLUS + '</span> Add column';
    addLink.addEventListener("click", function() { self._addRow("", ""); });
    addRow.appendChild(addLink);

    this.card.appendChild(addRow);
  };

  MutateUnified.prototype._addRow = function(name, expr) {
    var self = this;
    var id = this.nextId++;

    var row = document.createElement("div");
    row.className = "mu-row";
    row.setAttribute("data-row-id", id);

    // Name input
    var nameWrap = document.createElement("div");
    nameWrap.className = "mu-name-wrap";
    var nameInput = document.createElement("input");
    nameInput.type = "text";
    nameInput.className = "mu-name-input";
    nameInput.placeholder = "column_name";
    nameInput.value = name || "";
    nameWrap.appendChild(nameInput);
    row.appendChild(nameWrap);

    // Equals separator
    var eqSep = document.createElement("span");
    eqSep.className = "mu-eq-sep";
    eqSep.textContent = "=";
    row.appendChild(eqSep);

    // ACE editor area
    var codeDiv = document.createElement("div");
    codeDiv.className = "mu-expr-code";
    row.appendChild(codeDiv);

    // Confirm button: shows "Enter ↵", turns into checkmark when confirmed
    var confirmBtn = document.createElement("button");
    confirmBtn.className = "mu-expr-confirm";
    confirmBtn.type = "button";
    confirmBtn.innerHTML = "Enter &#x21B5;";
    confirmBtn.title = "Apply expression";

    var rowData = {
      id: id,
      nameInput: nameInput,
      exprEl: null,
      confirmBtn: confirmBtn,
      rowEl: row
    };

    var doConfirm = function() {
      confirmBtn.classList.add("confirmed");
      confirmBtn.innerHTML = ICON_CONFIRM;
      self._submit();
    };
    confirmBtn.addEventListener("click", doConfirm);

    // ACE editor — on change, reset confirm state
    var exprEl = createAceEditor(codeDiv, expr, this.columnNames, function() {
      confirmBtn.classList.remove("confirmed");
      confirmBtn.innerHTML = "Enter &#x21B5;";
    }, { maxLines: 10 });
    rowData.exprEl = exprEl;

    // Enter key in ACE confirms
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

    // Name input: debounced auto-submit on change (300ms)
    var nameTimer = null;
    nameInput.addEventListener("input", function() {
      clearTimeout(nameTimer);
      nameTimer = setTimeout(function() {
        self._submit();
      }, 300);
    });

    // Remove button
    var rmBtn = document.createElement("button");
    rmBtn.className = "mu-row-remove";
    rmBtn.type = "button";
    rmBtn.innerHTML = ICON_X;
    rmBtn.addEventListener("click", function() {
      self._removeRow(id);
      self._submit();
    });
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.rows.push(rowData);
    this._updateUI();
  };

  MutateUnified.prototype._removeRow = function(id) {
    // Must keep at least one row
    if (this.rows.length <= 1) return;

    var rowData = null, idx = -1;
    for (var i = 0; i < this.rows.length; i++) {
      if (this.rows[i].id === id) { rowData = this.rows[i]; idx = i; break; }
    }
    if (!rowData) return;

    if (rowData.exprEl && rowData.exprEl._aceEditor) rowData.exprEl._aceEditor.destroy();
    if (rowData.rowEl && rowData.rowEl.parentNode) rowData.rowEl.parentNode.removeChild(rowData.rowEl);
    this.rows.splice(idx, 1);
    this._updateUI();
  };

  MutateUnified.prototype._updateUI = function() {
    var single = this.rows.length <= 1;
    this.rows.forEach(function(r) {
      var btn = r.rowEl.querySelector(".mu-row-remove");
      if (btn) btn.style.visibility = single ? "hidden" : "visible";
    });
  };

  MutateUnified.prototype._compose = function() {
    var columns = [];
    this.rows.forEach(function(r) {
      var name = (r.nameInput.value || "").trim();
      var expr = "";
      if (r.exprEl && r.exprEl._aceEditor) {
        expr = r.exprEl._aceEditor.getValue().trim();
      }
      // Include the row even if name/expr are empty — R side will validate
      columns.push({ name: name, expr: expr });
    });
    return { columns: columns };
  };

  MutateUnified.prototype._submit = function() {
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  MutateUnified.prototype.getValue = function() {
    if (!this._submitted) return null;
    return this._compose();
  };

  MutateUnified.prototype.setValue = function(value) {};

  MutateUnified.prototype.updateColumns = function(cols) {
    this.columnNames = cols || [];
    var completer = makeCompleter(this.columnNames);
    this.rows.forEach(function(r) {
      if (r.exprEl && r.exprEl._aceEditor) {
        r.exprEl._aceEditor.completers = [completer];
      }
    });
  };

  MutateUnified.prototype.setRows = function(columns) {
    var self = this;
    // Clear existing rows
    this.rows.forEach(function(r) {
      if (r.exprEl && r.exprEl._aceEditor) r.exprEl._aceEditor.destroy();
      if (r.rowEl && r.rowEl.parentNode) r.rowEl.parentNode.removeChild(r.rowEl);
    });
    this.rows = [];

    // Add rows from data and auto-confirm
    if (columns && columns.length > 0) {
      columns.forEach(function(col) {
        self._addRow(col.name || "", col.expr || "");
      });
      // Mark all confirm buttons as confirmed
      this.rows.forEach(function(r) {
        var btn = r.rowEl.querySelector(".mu-expr-confirm");
        if (btn) btn.classList.add("confirmed");
      });
      // Auto-submit so the block evaluates immediately
      this._submit();
    } else {
      self._addRow("", "");
    }
  };

  // ---------------------------------------------------------------------------
  // Shiny input binding
  // ---------------------------------------------------------------------------
  var Binding = new Shiny.InputBinding();

  $.extend(Binding, {
    find: function(scope) { return $(scope).find(".mutate-unified-container"); },
    getId: function(el) { return el.id || null; },
    getValue: function(el) {
      if (!el._mutateUnified) return null;
      return el._mutateUnified.getValue();
    },
    setValue: function(el, value) {
      if (el._mutateUnified) el._mutateUnified.setValue(value);
    },
    subscribe: function(el, callback) {
      if (el._mutateUnified) el._mutateUnified._callback = function() { callback(true); };
    },
    unsubscribe: function(el) {
      if (el._mutateUnified) el._mutateUnified._callback = null;
    },
    initialize: function(el) {
      el._mutateUnified = new MutateUnified(el);

      // Apply pending columns if they arrived before init
      if (el._pendingColumns) {
        el._mutateUnified.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }

      // Apply pending initial rows if they arrived before init
      if (el._pendingRows) {
        el._mutateUnified.setRows(el._pendingRows);
        delete el._pendingRows;
      }
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);
    }
  });

  Shiny.inputBindings.register(Binding, "blockr.mutateUnified");

  // Message handler: update column names for autocomplete
  Shiny.addCustomMessageHandler("mutate-unified-update-columns", function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._mutateUnified) {
      el._mutateUnified.updateColumns(msg.columns);
    } else if (el) {
      el._pendingColumns = msg.columns;
    } else {
      var a = 0, t = setInterval(function() {
        a++;
        var el2 = document.getElementById(msg.id);
        if (el2 && el2._mutateUnified) { el2._mutateUnified.updateColumns(msg.columns); clearInterval(t); }
        else if (el2) { el2._pendingColumns = msg.columns; clearInterval(t); }
        if (a > 50) clearInterval(t);
      }, 100);
    }
  });

  // Message handler: set initial rows (for restoring state)
  Shiny.addCustomMessageHandler("mutate-unified-set-rows", function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._mutateUnified) {
      el._mutateUnified.setRows(msg.columns);
    } else if (el) {
      el._pendingRows = msg.columns;
    } else {
      var a = 0, t = setInterval(function() {
        a++;
        var el2 = document.getElementById(msg.id);
        if (el2 && el2._mutateUnified) { el2._mutateUnified.setRows(msg.columns); clearInterval(t); }
        else if (el2) { el2._pendingRows = msg.columns; clearInterval(t); }
        if (a > 50) clearInterval(t);
      }, 100);
    }
  });

})();
