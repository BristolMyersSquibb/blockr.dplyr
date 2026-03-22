/**
 * BlockrInput — lightweight code input with autocomplete
 *
 * Replaces ACE editor in blockr unified blocks.
 * Self-contained IIFE, no dependencies beyond basic DOM APIs.
 *
 * API:
 *   BlockrInput.create(container, config) -> { el, getValue, setValue, setColumns, destroy }
 */
(function() {
  "use strict";

  var _uid = 0;
  function uid() { return "binp-" + (++_uid); }

  // Token boundary characters — autocomplete triggers after these
  var BOUNDARY = /[(\s,+\-*/><!=&|~^%:[\]{}]/;

  function backtickIfNeeded(n) {
    return /^[a-zA-Z.][a-zA-Z0-9._]*$/.test(n) ? n : "`" + n + "`";
  }

  // -------------------------------------------------------------------------
  // Core: create an input instance
  // -------------------------------------------------------------------------

  function createInput(container, config) {
    var id = uid();
    var popupId = id + "-popup";

    // Config
    var columns = (config.columns || []).slice();
    var categories = config.categories || {};
    var placeholder = config.placeholder || "";
    var multiline = config.multiline === true;
    var onChange = config.onChange || null;
    var onConfirm = config.onConfirm || null;
    var destroyed = false;

    // Autocomplete state
    var completions = [];    // built from columns + categories
    var filtered = [];       // completions matching current token
    var highlightIdx = -1;
    var popupOpen = false;
    var tokenStart = 0;      // char index where current token begins
    var tokenEnd = 0;        // char index where current token ends

    // DOM
    var root = document.createElement("div");
    root.className = "blockr-input";

    var field;
    if (multiline) {
      field = document.createElement("textarea");
      field.rows = 1;
    } else {
      field = document.createElement("input");
      field.type = "text";
    }
    field.className = "blockr-input__field";
    field.placeholder = placeholder;
    field.setAttribute("autocomplete", "off");
    field.setAttribute("autocorrect", "off");
    field.setAttribute("autocapitalize", "off");
    field.setAttribute("spellcheck", "false");
    if (config.value) field.value = config.value;

    var popup = document.createElement("div");
    popup.className = "blockr-input__popup";
    popup.id = popupId;
    popup.setAttribute("role", "listbox");

    root.appendChild(field);
    root.appendChild(popup);
    container.appendChild(root);

    // -----------------------------------------------------------------------
    // Completion list building
    // -----------------------------------------------------------------------

    function rebuildCompletions() {
      completions = [];
      // Columns first (higher score)
      var cols = columns.map(backtickIfNeeded);
      for (var i = 0; i < cols.length; i++) {
        completions.push({
          text: cols[i],
          insertText: cols[i],
          meta: "column",
          isFunc: false,
          score: 1001
        });
      }
      // Functions by category
      var catKeys = Object.keys(categories);
      for (var c = 0; c < catKeys.length; c++) {
        var cat = catKeys[c];
        var fns = categories[cat];
        for (var f = 0; f < fns.length; f++) {
          completions.push({
            text: fns[f],
            insertText: fns[f] + "()",
            meta: cat,
            isFunc: true,
            score: 1000
          });
        }
      }
      // Sort: columns first, then alphabetical within each score group
      completions.sort(function(a, b) {
        if (a.score !== b.score) return b.score - a.score;
        return a.text.localeCompare(b.text);
      });
    }

    rebuildCompletions();

    // -----------------------------------------------------------------------
    // Token extraction
    // -----------------------------------------------------------------------

    function getCurrentToken() {
      var val = field.value;
      var cursor = field.selectionStart || 0;

      // Walk backwards from cursor to find token start
      var start = cursor;
      while (start > 0) {
        var ch = val.charAt(start - 1);
        if (BOUNDARY.test(ch)) break;
        // Allow backtick-quoted tokens
        if (ch === "`") { start--; break; }
        start--;
      }

      tokenStart = start;
      tokenEnd = cursor;
      return val.substring(start, cursor);
    }

    // -----------------------------------------------------------------------
    // Popup rendering
    // -----------------------------------------------------------------------

    function filterCompletions(token) {
      if (!token) { filtered = []; return; }
      var q = token.toLowerCase();
      // Strip leading backtick for matching
      if (q.charAt(0) === "`") q = q.substring(1);
      filtered = [];
      for (var i = 0; i < completions.length; i++) {
        var c = completions[i];
        var matchText = c.text.toLowerCase();
        // Strip leading backtick for matching
        if (matchText.charAt(0) === "`") matchText = matchText.substring(1);
        if (matchText.indexOf(q) === 0) {
          filtered.push(c);
        }
      }
    }

    function renderPopup() {
      popup.innerHTML = "";

      if (filtered.length === 0) {
        closePopup();
        return;
      }

      // Clamp highlight
      if (highlightIdx >= filtered.length) highlightIdx = filtered.length - 1;
      if (highlightIdx < 0) highlightIdx = 0;

      for (var i = 0; i < filtered.length; i++) {
        var c = filtered[i];
        var item = document.createElement("div");
        item.className = "blockr-input__item";
        if (i === highlightIdx) item.className += " blockr-input__item--highlighted";
        item.setAttribute("role", "option");
        item.setAttribute("data-idx", i);

        var textSpan = document.createElement("span");
        textSpan.className = "blockr-input__item-text";
        textSpan.textContent = c.text;
        item.appendChild(textSpan);

        if (c.isFunc) {
          var parens = document.createElement("span");
          parens.className = "blockr-input__item-parens";
          parens.textContent = "()";
          item.appendChild(parens);
        }

        var metaSpan = document.createElement("span");
        metaSpan.className = "blockr-input__item-meta";
        metaSpan.textContent = c.meta;
        item.appendChild(metaSpan);

        popup.appendChild(item);
      }

      if (!popupOpen) openPopup();
      scrollHighlightIntoView();
    }

    function scrollHighlightIntoView() {
      var el = popup.querySelector(".blockr-input__item--highlighted");
      if (el) el.scrollIntoView({ block: "nearest" });
    }

    // -----------------------------------------------------------------------
    // Popup open / close
    // -----------------------------------------------------------------------

    function openPopup() {
      if (popupOpen || destroyed) return;
      popupOpen = true;
      root.classList.add("blockr-input--popup-open");
    }

    function closePopup() {
      if (!popupOpen) return;
      popupOpen = false;
      root.classList.remove("blockr-input--popup-open");
      popup.innerHTML = "";
      highlightIdx = 0;
    }

    // -----------------------------------------------------------------------
    // Completion insertion
    // -----------------------------------------------------------------------

    function acceptCompletion(idx) {
      if (idx < 0 || idx >= filtered.length) return;
      var c = filtered[idx];
      var val = field.value;

      // Replace the current token with the completion
      var before = val.substring(0, tokenStart);
      var after = val.substring(tokenEnd);
      var newVal = before + c.insertText + after;
      field.value = newVal;

      // Place cursor: for functions, between parens; otherwise after the text
      var cursorPos;
      if (c.isFunc) {
        // cursor between the ()
        cursorPos = tokenStart + c.insertText.length - 1;
      } else {
        cursorPos = tokenStart + c.insertText.length;
      }
      field.setSelectionRange(cursorPos, cursorPos);

      closePopup();

      // Fire onChange
      if (onChange) onChange();
    }

    // -----------------------------------------------------------------------
    // Event handlers
    // -----------------------------------------------------------------------

    function onFieldInput() {
      var token = getCurrentToken();
      filterCompletions(token);
      highlightIdx = 0;

      if (filtered.length > 0 && token.length > 0) {
        renderPopup();
      } else {
        closePopup();
      }

      if (onChange) onChange();
    }

    function onFieldKeydown(e) {
      if (popupOpen) {
        switch (e.key) {
          case "ArrowDown":
            e.preventDefault();
            highlightIdx = (highlightIdx + 1) % filtered.length;
            renderPopup();
            break;

          case "ArrowUp":
            e.preventDefault();
            highlightIdx = (highlightIdx - 1 + filtered.length) % filtered.length;
            renderPopup();
            break;

          case "Enter":
            e.preventDefault();
            acceptCompletion(highlightIdx);
            break;

          case "Tab":
            e.preventDefault();
            acceptCompletion(highlightIdx);
            break;

          case "Escape":
            e.preventDefault();
            closePopup();
            break;
        }
      } else {
        // Popup closed
        if (e.key === "Enter" && !multiline) {
          e.preventDefault();
          if (onConfirm) onConfirm(field.value.trim());
        }
      }
    }

    function onPopupClick(e) {
      var item = e.target.closest ? e.target.closest(".blockr-input__item") : null;
      if (!item) {
        var t = e.target;
        while (t && t !== popup) {
          if (t.classList && t.classList.contains("blockr-input__item")) { item = t; break; }
          t = t.parentNode;
        }
      }
      if (item) {
        var idx = parseInt(item.getAttribute("data-idx"), 10);
        if (!isNaN(idx)) {
          // Re-extract token since cursor may have moved
          getCurrentToken();
          acceptCompletion(idx);
          field.focus();
        }
      }
    }

    function onDocumentClick(e) {
      if (!root.contains(e.target)) closePopup();
    }

    function onFieldBlur() {
      // Delay to allow popup click to fire first
      setTimeout(function() {
        if (!root.contains(document.activeElement)) {
          closePopup();
        }
      }, 150);
    }

    // -----------------------------------------------------------------------
    // Bind events
    // -----------------------------------------------------------------------

    field.addEventListener("input", onFieldInput);
    field.addEventListener("keydown", onFieldKeydown);
    field.addEventListener("blur", onFieldBlur);
    popup.addEventListener("mousedown", function(e) {
      // Prevent blur on popup click
      e.preventDefault();
    });
    popup.addEventListener("click", onPopupClick);
    document.addEventListener("click", onDocumentClick, true);

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    var instance = {
      el: root,

      getValue: function() {
        return field.value.trim();
      },

      setValue: function(v) {
        field.value = v || "";
      },

      setColumns: function(cols) {
        columns = (cols || []).slice();
        rebuildCompletions();
      },

      focus: function() {
        field.focus();
      },

      destroy: function() {
        if (destroyed) return;
        destroyed = true;
        closePopup();
        field.removeEventListener("input", onFieldInput);
        field.removeEventListener("keydown", onFieldKeydown);
        field.removeEventListener("blur", onFieldBlur);
        popup.removeEventListener("click", onPopupClick);
        document.removeEventListener("click", onDocumentClick, true);
        if (root.parentNode) root.parentNode.removeChild(root);
      }
    };

    return instance;
  }

  // -------------------------------------------------------------------------
  // Public API
  // -------------------------------------------------------------------------

  window.BlockrInput = {
    create: function(container, config) {
      return createInput(container, config);
    }
  };

})();
