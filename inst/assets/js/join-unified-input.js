// JoinUnified — JS-driven unified join block with non-equi join support.
// Binary block: takes x and y data inputs.
// Key rows with column selectize + operator cycling.
// Expression mode with ACE editor. Settings popover for suffix config.

(function() {
  "use strict";

  // ---------------------------------------------------------------------------
  // SVG icons
  // ---------------------------------------------------------------------------
  var ICON_X = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>';
  var ICON_PLUS = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>';
  var ICON_CODE = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M10.478 1.647a.5.5 0 1 0-.956-.294l-4 13a.5.5 0 0 0 .956.294zM4.854 4.146a.5.5 0 0 1 0 .708L1.707 8l3.147 3.146a.5.5 0 0 1-.708.708l-3.5-3.5a.5.5 0 0 1 0-.708l3.5-3.5a.5.5 0 0 1 .708 0m6.292 0a.5.5 0 0 0 0 .708L14.293 8l-3.147 3.146a.5.5 0 0 0 .708.708l3.5-3.5a.5.5 0 0 0 0-.708l-3.5-3.5a.5.5 0 0 0-.708 0"/></svg>';
  var ICON_GEAR = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>';
  var ICON_CONFIRM = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>';

  // ---------------------------------------------------------------------------
  // ACE editor helpers
  // ---------------------------------------------------------------------------
  var defaultCategories = {
    arithmetic: ["abs","sign","ceiling","floor","round","trunc","log","log2","log10","exp","sqrt"],
    aggregate: ["mean","sum","min","max"],
    offset: ["lead","lag","cumsum","cumprod","cummin","cummax"],
    logical: ["if_else","case_when","between"],
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
    el.className = "ju-ace-editor";
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

  function createColumnSelectize(container, options, selected, placeholder, onChange) {
    var sel = document.createElement("select");
    sel.className = "ju-selectize-col";
    container.appendChild(sel);

    var wrapper = {
      el: sel, api: null,
      setOptions: function(opts, s) {
        if (!wrapper.api) { wrapper._pending = { opts: opts, sel: s }; return; }
        wrapper.api.clearOptions();
        opts.forEach(function(v) { wrapper.api.addOption({ value: v, text: v }); });
        wrapper.api.refreshOptions(false);
        if (s && opts.indexOf(s) >= 0) wrapper.api.setValue(s, true);
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
        placeholder: placeholder || "Column\u2026",
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

  // ---------------------------------------------------------------------------
  // Join type definitions
  // ---------------------------------------------------------------------------
  var JOIN_TYPES = [
    { value: "left_join",  label: "left join" },
    { value: "inner_join", label: "inner join" },
    { value: "right_join", label: "right join" },
    { value: "full_join",  label: "full join" },
    { value: "semi_join",  label: "semi join" },
    { value: "anti_join",  label: "anti" }
  ];

  var KEY_OPS = [
    { value: "==", label: "==" },
    { value: ">=", label: "\u2265" },
    { value: ">",  label: ">" },
    { value: "<=", label: "\u2264" },
    { value: "<",  label: "<" }
  ];

  // ---------------------------------------------------------------------------
  // JoinUnified component
  // ---------------------------------------------------------------------------
  function JoinUnified(el) {
    this.el = el;
    this.keys = [];
    this.exprRows = [];
    this.nextId = 1;
    this.xColumns = [];
    this.yColumns = [];
    this.joinTypeIdx = 0;
    this.joinType = JOIN_TYPES[0].value;
    this.suffixX = ".x";
    this.suffixY = ".y";
    this._callback = null;
    this._submitted = false;
    this._debounceTimer = null;
    this._popoverOpen = false;

    this._buildDOM();
    this._addKeyRow(null, null, null);
  }

  // Debounced auto-submit (300ms)
  JoinUnified.prototype._autoSubmit = function() {
    var self = this;
    clearTimeout(this._debounceTimer);
    this._debounceTimer = setTimeout(function() {
      self._submit();
    }, 300);
  };

  JoinUnified.prototype._submit = function() {
    this._submitted = true;
    if (this._callback) this._callback(true);
  };

  JoinUnified.prototype._buildDOM = function() {
    var self = this;

    // Card
    this.card = document.createElement("div");
    this.card.className = "ju-card";
    this.el.appendChild(this.card);

    // Header row: Join type pill + spacer + gear icon
    var header = document.createElement("div");
    header.className = "ju-header";

    // Join type pill (click-through cycle)
    this.joinTypePill = document.createElement("button");
    this.joinTypePill.type = "button";
    this.joinTypePill.className = "ju-pill ju-join-type-pill";
    this.joinTypePill.textContent = JOIN_TYPES[0].label;
    this.joinTypePill.title = "Click to cycle join type";
    this.joinTypePill.addEventListener("click", function() {
      self.joinTypeIdx = (self.joinTypeIdx + 1) % JOIN_TYPES.length;
      self.joinType = JOIN_TYPES[self.joinTypeIdx].value;
      self.joinTypePill.textContent = JOIN_TYPES[self.joinTypeIdx].label;
      self._autoSubmit();
    });
    header.appendChild(this.joinTypePill);

    var spacer = document.createElement("span");
    spacer.style.flex = "1";
    header.appendChild(spacer);

    // Settings gear button
    this.gearBtn = document.createElement("button");
    this.gearBtn.type = "button";
    this.gearBtn.className = "ju-gear-btn";
    this.gearBtn.innerHTML = ICON_GEAR;
    this.gearBtn.title = "Suffix settings";
    this.gearBtn.addEventListener("click", function(e) {
      e.stopPropagation();
      self._togglePopover();
    });
    header.appendChild(this.gearBtn);

    this.card.appendChild(header);

    // Settings popover
    this._buildPopover();

    // Keys list
    this.listEl = document.createElement("div");
    this.listEl.className = "ju-keys-list";
    this.card.appendChild(this.listEl);

    // Add row: [+ Add key] [</>]
    var addRow = document.createElement("div");
    addRow.className = "ju-add-row";

    var addKeyLink = document.createElement("span");
    addKeyLink.className = "ju-add-link";
    addKeyLink.innerHTML = '<span class="ju-add-icon">' + ICON_PLUS + '</span> Add key';
    addKeyLink.addEventListener("click", function() { self._addKeyRow(null, null, null); });
    addRow.appendChild(addKeyLink);

    var addExprLink = document.createElement("span");
    addExprLink.className = "ju-add-link-expr";
    addExprLink.innerHTML = ICON_CODE;
    addExprLink.title = "Add R expression";
    addExprLink.addEventListener("click", function() { self._addExprRow("between(col, start, end)"); });
    addRow.appendChild(addExprLink);

    this.card.appendChild(addRow);

    // Close popover on outside click
    document.addEventListener("click", function(e) {
      if (self._popoverOpen && self.popoverEl &&
          !self.popoverEl.contains(e.target) &&
          !self.gearBtn.contains(e.target)) {
        self._closePopover();
      }
    });
  };

  // ---------------------------------------------------------------------------
  // Settings popover
  // ---------------------------------------------------------------------------
  JoinUnified.prototype._buildPopover = function() {
    var self = this;

    this.popoverEl = document.createElement("div");
    this.popoverEl.className = "ju-popover";
    this.popoverEl.style.display = "none";

    // Suffix X
    var rowX = document.createElement("div");
    rowX.className = "ju-popover-row";
    var labelX = document.createElement("label");
    labelX.textContent = "Suffix X:";
    labelX.className = "ju-popover-label";
    this.suffixXInput = document.createElement("input");
    this.suffixXInput.type = "text";
    this.suffixXInput.className = "ju-popover-input";
    this.suffixXInput.value = this.suffixX;
    this.suffixXInput.addEventListener("input", function() {
      self.suffixX = self.suffixXInput.value;
      self._autoSubmit();
    });
    rowX.appendChild(labelX);
    rowX.appendChild(this.suffixXInput);
    this.popoverEl.appendChild(rowX);

    // Suffix Y
    var rowY = document.createElement("div");
    rowY.className = "ju-popover-row";
    var labelY = document.createElement("label");
    labelY.textContent = "Suffix Y:";
    labelY.className = "ju-popover-label";
    this.suffixYInput = document.createElement("input");
    this.suffixYInput.type = "text";
    this.suffixYInput.className = "ju-popover-input";
    this.suffixYInput.value = this.suffixY;
    this.suffixYInput.addEventListener("input", function() {
      self.suffixY = self.suffixYInput.value;
      self._autoSubmit();
    });
    rowY.appendChild(labelY);
    rowY.appendChild(this.suffixYInput);
    this.popoverEl.appendChild(rowY);

    this.card.appendChild(this.popoverEl);
  };

  JoinUnified.prototype._togglePopover = function() {
    if (this._popoverOpen) {
      this._closePopover();
    } else {
      this._openPopover();
    }
  };

  JoinUnified.prototype._openPopover = function() {
    this.popoverEl.style.display = "block";
    this._popoverOpen = true;
    this.gearBtn.classList.add("ju-gear-active");
  };

  JoinUnified.prototype._closePopover = function() {
    this.popoverEl.style.display = "none";
    this._popoverOpen = false;
    this.gearBtn.classList.remove("ju-gear-active");
  };

  // ---------------------------------------------------------------------------
  // Key rows
  // ---------------------------------------------------------------------------
  JoinUnified.prototype._addKeyRow = function(xCol, op, yCol) {
    var self = this;
    var id = this.nextId++;
    var key = {
      id: id,
      xCol: xCol || "",
      op: op || "==",
      yCol: yCol || "",
      _xSelectize: null,
      _ySelectize: null,
      rowEl: null
    };

    var row = document.createElement("div");
    row.className = "ju-row";
    row.setAttribute("data-key-id", id);
    key.rowEl = row;

    // X column selectize
    var xDiv = document.createElement("div");
    xDiv.className = "ju-col-wrap ju-col-x";
    row.appendChild(xDiv);
    key._xSelectize = createColumnSelectize(
      xDiv, this.xColumns, xCol, "x column\u2026",
      function(value) {
        key.xCol = value;
        self._autoSubmit();
      }
    );

    // Operator pill (click-through cycle)
    var opIdx = 0;
    for (var i = 0; i < KEY_OPS.length; i++) {
      if (KEY_OPS[i].value === (op || "==")) { opIdx = i; break; }
    }
    var opBtn = document.createElement("button");
    opBtn.type = "button";
    opBtn.className = "ju-pill ju-op-btn";
    opBtn.textContent = KEY_OPS[opIdx].label;
    opBtn.title = "Click to cycle operator";
    opBtn.addEventListener("click", function() {
      opIdx = (opIdx + 1) % KEY_OPS.length;
      key.op = KEY_OPS[opIdx].value;
      opBtn.textContent = KEY_OPS[opIdx].label;
      self._autoSubmit();
    });
    key._opBtn = opBtn;
    row.appendChild(opBtn);

    // Y column selectize
    var yDiv = document.createElement("div");
    yDiv.className = "ju-col-wrap ju-col-y";
    row.appendChild(yDiv);
    key._ySelectize = createColumnSelectize(
      yDiv, this.yColumns, yCol, "y column\u2026",
      function(value) {
        key.yCol = value;
        self._autoSubmit();
      }
    );

    // Remove button
    var rmBtn = document.createElement("button");
    rmBtn.className = "ju-row-remove";
    rmBtn.type = "button";
    rmBtn.innerHTML = ICON_X;
    rmBtn.addEventListener("click", function() {
      self._removeKey(id);
      self._autoSubmit();
    });
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.keys.push(key);
    this._updateUI();
  };

  JoinUnified.prototype._removeKey = function(id) {
    var totalRows = this.keys.length + this.exprRows.length;
    if (totalRows <= 1) return;

    for (var i = 0; i < this.keys.length; i++) {
      if (this.keys[i].id === id) {
        var key = this.keys[i];
        if (key._xSelectize) key._xSelectize.destroy();
        if (key._ySelectize) key._ySelectize.destroy();
        if (key.rowEl && key.rowEl.parentNode) key.rowEl.parentNode.removeChild(key.rowEl);
        this.keys.splice(i, 1);
        break;
      }
    }
    this._updateUI();
  };

  // ---------------------------------------------------------------------------
  // Expression rows
  // ---------------------------------------------------------------------------
  JoinUnified.prototype._addExprRow = function(value) {
    var self = this;
    var id = this.nextId++;

    var row = document.createElement("div");
    row.className = "ju-row ju-row-expr";
    row.setAttribute("data-expr-id", id);

    var codeDiv = document.createElement("div");
    codeDiv.className = "ju-row-content ju-expr-code";
    row.appendChild(codeDiv);

    // Confirm button
    var confirmBtn = document.createElement("button");
    confirmBtn.className = "ju-expr-confirm";
    confirmBtn.type = "button";
    confirmBtn.innerHTML = "Enter &#x21B5;";
    confirmBtn.title = "Apply expression";
    var doConfirm = function() {
      confirmBtn.classList.add("confirmed");
      confirmBtn.innerHTML = ICON_CONFIRM;
      self._submit();
    };
    confirmBtn.addEventListener("click", doConfirm);

    var allCols = self.xColumns.concat(self.yColumns);
    var exprEl = createAceEditor(codeDiv, value, allCols, function() {
      confirmBtn.classList.remove("confirmed");
      confirmBtn.innerHTML = "Enter &#x21B5;";
    }, { maxLines: 10 });

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
    rmBtn.className = "ju-row-remove";
    rmBtn.type = "button";
    rmBtn.innerHTML = ICON_X;
    rmBtn.addEventListener("click", function() {
      self._removeExpr(id);
    });
    row.appendChild(rmBtn);

    this.listEl.appendChild(row);
    this.exprRows.push({
      id: id,
      exprEl: exprEl,
      rowEl: row
    });
    this._updateUI();
  };

  JoinUnified.prototype._removeExpr = function(id) {
    var totalRows = this.keys.length + this.exprRows.length;
    if (totalRows <= 1) return;

    for (var i = 0; i < this.exprRows.length; i++) {
      if (this.exprRows[i].id === id) {
        var er = this.exprRows[i];
        if (er.exprEl && er.exprEl._aceEditor) er.exprEl._aceEditor.destroy();
        if (er.rowEl && er.rowEl.parentNode) er.rowEl.parentNode.removeChild(er.rowEl);
        this.exprRows.splice(i, 1);
        break;
      }
    }
    this._updateUI();
  };

  // ---------------------------------------------------------------------------
  // Shared UI management
  // ---------------------------------------------------------------------------
  JoinUnified.prototype._updateUI = function() {
    var totalRows = this.keys.length + this.exprRows.length;
    var single = totalRows <= 1;

    this.keys.forEach(function(k) {
      var btn = k.rowEl.querySelector(".ju-row-remove");
      if (btn) btn.style.visibility = single ? "hidden" : "visible";
    });
    this.exprRows.forEach(function(er) {
      var btn = er.rowEl.querySelector(".ju-row-remove");
      if (btn) btn.style.visibility = single ? "hidden" : "visible";
    });
  };

  // ---------------------------------------------------------------------------
  // Compose output
  // ---------------------------------------------------------------------------
  JoinUnified.prototype._compose = function() {
    var keys = [];
    this.keys.forEach(function(k) {
      if (k.xCol && k.yCol) {
        keys.push({ xCol: k.xCol, op: k.op, yCol: k.yCol });
      }
    });

    var exprs = [];
    this.exprRows.forEach(function(er) {
      if (er.exprEl && er.exprEl._aceEditor) {
        var val = er.exprEl._aceEditor.getValue().trim();
        if (val && val !== "") exprs.push(val);
      }
    });

    return {
      joinType: this.joinType,
      keys: keys,
      exprs: exprs,
      suffixX: this.suffixX,
      suffixY: this.suffixY
    };
  };

  JoinUnified.prototype.getValue = function() {
    if (!this._submitted) return null;
    return this._compose();
  };

  JoinUnified.prototype.setValue = function(value) {};

  // ---------------------------------------------------------------------------
  // Update columns from R
  // ---------------------------------------------------------------------------
  JoinUnified.prototype.updateColumns = function(xColumns, yColumns) {
    this.xColumns = xColumns || [];
    this.yColumns = yColumns || [];
    var self = this;

    // Update key row selectizes
    this.keys.forEach(function(k) {
      if (k._xSelectize) {
        var curX = k._xSelectize.getValue();
        k._xSelectize.setOptions(self.xColumns, curX);
        k.xCol = k._xSelectize.getValue();
      }
      if (k._ySelectize) {
        var curY = k._ySelectize.getValue();
        k._ySelectize.setOptions(self.yColumns, curY);
        k.yCol = k._ySelectize.getValue();
      }
    });

    // Update ACE completers with combined columns
    var allCols = this.xColumns.concat(this.yColumns);
    this.exprRows.forEach(function(er) {
      if (er.exprEl && er.exprEl._aceEditor) {
        er.exprEl._aceEditor.completers = [makeCompleter(allCols)];
      }
    });
  };

  // ---------------------------------------------------------------------------
  // Shiny input binding
  // ---------------------------------------------------------------------------
  var Binding = new Shiny.InputBinding();

  $.extend(Binding, {
    find: function(scope) { return $(scope).find(".join-unified-container"); },
    getId: function(el) { return el.id || null; },
    getValue: function(el) {
      if (!el._joinUnified) return null;
      return el._joinUnified.getValue();
    },
    setValue: function(el, value) {
      if (el._joinUnified) el._joinUnified.setValue(value);
    },
    subscribe: function(el, callback) {
      if (el._joinUnified) el._joinUnified._callback = function() { callback(true); };
    },
    unsubscribe: function(el) {
      if (el._joinUnified) el._joinUnified._callback = null;
    },
    initialize: function(el) {
      el._joinUnified = new JoinUnified(el);
      if (el._pendingColumns) {
        el._joinUnified.updateColumns(
          el._pendingColumns.xColumns,
          el._pendingColumns.yColumns
        );
        delete el._pendingColumns;
      }
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);
    }
  });

  Shiny.inputBindings.register(Binding, "blockr.joinUnified");

  Shiny.addCustomMessageHandler("join-unified-update-columns", function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._joinUnified) {
      el._joinUnified.updateColumns(msg.xColumns, msg.yColumns);
    } else if (el) {
      el._pendingColumns = { xColumns: msg.xColumns, yColumns: msg.yColumns };
    } else {
      var a = 0, t = setInterval(function() {
        a++;
        var el2 = document.getElementById(msg.id);
        if (el2 && el2._joinUnified) {
          el2._joinUnified.updateColumns(msg.xColumns, msg.yColumns);
          clearInterval(t);
        } else if (el2) {
          el2._pendingColumns = { xColumns: msg.xColumns, yColumns: msg.yColumns };
          clearInterval(t);
        }
        if (a > 50) clearInterval(t);
      }, 100);
    }
  });

})();
