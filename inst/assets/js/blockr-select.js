/**
 * BlockrSelect — lightweight select component (single + multi)
 *
 * Replaces selectize.js in blockr unified blocks.
 * Self-contained IIFE, no dependencies beyond basic DOM APIs.
 *
 * API:
 *   BlockrSelect.single(container, config) -> { el, setOptions, getValue, destroy }
 *   BlockrSelect.multi(container, config)  -> { el, setOptions, getValue, destroy }
 */
(function() {
  "use strict";

  var _uid = 0;
  function uid() { return "bsel-" + (++_uid); }

  // -------------------------------------------------------------------------
  // Shared helpers
  // -------------------------------------------------------------------------

  function escapeHtml(s) {
    var div = document.createElement("div");
    div.appendChild(document.createTextNode(s));
    return div.innerHTML;
  }

  function removeNode(node) {
    if (node && node.parentNode) node.parentNode.removeChild(node);
  }

  // SVG icons (inline, no external deps)
  var CHEVRON_SVG =
    '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round">' +
    '<polyline points="3 4.5 6 7.5 9 4.5"></polyline></svg>';

  var REMOVE_SVG =
    '<svg width="10" height="10" viewBox="0 0 10 10" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round">' +
    '<line x1="2.5" y1="2.5" x2="7.5" y2="7.5"></line><line x1="7.5" y1="2.5" x2="2.5" y2="7.5"></line></svg>';

  // -------------------------------------------------------------------------
  // Core: create a select instance (shared by single + multi)
  // -------------------------------------------------------------------------

  function createSelect(container, config, mode) {
    var id = uid();
    var dropdownId = id + "-lb";

    // State
    var options = config.options || [];
    var selected = mode === "multi"
      ? (config.selected || []).slice()
      : (config.selected != null ? config.selected : (options.length > 0 ? options[0] : ""));
    var placeholder = config.placeholder || "";
    var reorderable = mode === "multi" && config.reorderable !== false;
    var onChange = config.onChange || null;
    var isOpen = false;
    var searchQuery = "";
    var highlightIdx = -1;
    var destroyed = false;

    // Drag state (multi only)
    var dragValue = null;
    var dragOverTag = null;
    var dragSide = null; // "before" | "after"

    // DOM
    var root = document.createElement("div");
    root.className = "blockr-select blockr-select--" + mode;
    root.setAttribute("role", "combobox");
    root.setAttribute("aria-expanded", "false");
    root.setAttribute("aria-haspopup", "listbox");
    root.setAttribute("aria-owns", dropdownId);

    var control = document.createElement("div");
    control.className = "blockr-select__control";

    var tagsEl = null; // multi only
    var valueEl = null; // single only

    if (mode === "multi") {
      tagsEl = document.createElement("div");
      tagsEl.className = "blockr-select__tags";
      control.appendChild(tagsEl);
    } else {
      valueEl = document.createElement("span");
      valueEl.className = "blockr-select__value";
      control.appendChild(valueEl);
    }

    var searchInput = document.createElement("input");
    searchInput.type = "text";
    searchInput.className = "blockr-select__search";
    searchInput.setAttribute("aria-autocomplete", "list");
    searchInput.setAttribute("aria-controls", dropdownId);
    searchInput.setAttribute("autocomplete", "off");
    searchInput.setAttribute("autocorrect", "off");
    searchInput.setAttribute("autocapitalize", "off");
    searchInput.setAttribute("spellcheck", "false");
    if (mode === "multi") {
      searchInput.setAttribute("placeholder", placeholder);
    }
    control.appendChild(searchInput);

    if (mode === "single") {
      var arrow = document.createElement("span");
      arrow.className = "blockr-select__arrow";
      arrow.innerHTML = CHEVRON_SVG;
      control.appendChild(arrow);
    }

    var dropdown = document.createElement("div");
    dropdown.className = "blockr-select__dropdown";
    dropdown.id = dropdownId;
    dropdown.setAttribute("role", "listbox");

    root.appendChild(control);
    root.appendChild(dropdown);
    container.appendChild(root);

    // -----------------------------------------------------------------------
    // Rendering
    // -----------------------------------------------------------------------

    function getFiltered() {
      var q = searchQuery.toLowerCase();
      var result = [];
      for (var i = 0; i < options.length; i++) {
        var opt = options[i];
        // In multi mode, hide already-selected
        if (mode === "multi" && selected.indexOf(opt) >= 0) continue;
        if (q && opt.toLowerCase().indexOf(q) < 0) continue;
        result.push(opt);
      }
      return result;
    }

    function renderDropdown() {
      var filtered = getFiltered();
      dropdown.innerHTML = "";

      if (filtered.length === 0) {
        var empty = document.createElement("div");
        empty.className = "blockr-select__empty";
        empty.textContent = searchQuery ? "No matches" : (mode === "multi" ? "All selected" : "No options");
        dropdown.appendChild(empty);
        highlightIdx = -1;
        return;
      }

      // Clamp highlight
      if (highlightIdx >= filtered.length) highlightIdx = filtered.length - 1;
      if (highlightIdx < 0 && filtered.length > 0) highlightIdx = 0;

      for (var i = 0; i < filtered.length; i++) {
        var opt = filtered[i];
        var div = document.createElement("div");
        div.className = "blockr-select__option";
        if (i === highlightIdx) div.className += " blockr-select__option--highlighted";
        if (mode === "single" && opt === selected) div.className += " blockr-select__option--selected";
        div.setAttribute("role", "option");
        div.setAttribute("id", id + "-opt-" + i);
        div.setAttribute("aria-selected", (mode === "single" && opt === selected) ? "true" : "false");
        div.setAttribute("data-value", opt);
        div.textContent = opt;
        dropdown.appendChild(div);
      }

      // Update activedescendant
      if (highlightIdx >= 0) {
        searchInput.setAttribute("aria-activedescendant", id + "-opt-" + highlightIdx);
      }
    }

    function renderValue() {
      if (mode === "single") {
        if (selected) {
          valueEl.textContent = selected;
          valueEl.classList.remove("blockr-select__value--placeholder");
          searchInput.setAttribute("placeholder", "");
        } else {
          valueEl.textContent = placeholder;
          valueEl.classList.add("blockr-select__value--placeholder");
          searchInput.setAttribute("placeholder", placeholder);
        }
      }
    }

    function renderTags() {
      if (mode !== "multi") return;
      tagsEl.innerHTML = "";
      for (var i = 0; i < selected.length; i++) {
        var val = selected[i];
        var tag = document.createElement("span");
        tag.className = "blockr-select__tag";
        tag.setAttribute("data-value", val);
        if (reorderable) tag.setAttribute("draggable", "true");

        var label = document.createElement("span");
        label.className = "blockr-select__tag-label";
        label.textContent = val;
        tag.appendChild(label);

        var removeBtn = document.createElement("button");
        removeBtn.type = "button";
        removeBtn.className = "blockr-select__tag-remove";
        removeBtn.setAttribute("aria-label", "Remove " + val);
        removeBtn.setAttribute("tabindex", "-1");
        removeBtn.innerHTML = REMOVE_SVG;
        tag.appendChild(removeBtn);

        tagsEl.appendChild(tag);
      }
      // Update placeholder visibility
      if (selected.length === 0) {
        searchInput.setAttribute("placeholder", placeholder);
      } else {
        searchInput.setAttribute("placeholder", "");
      }
    }

    function render() {
      if (mode === "single") renderValue();
      if (mode === "multi") renderTags();
      if (isOpen) renderDropdown();
    }

    // -----------------------------------------------------------------------
    // Open / close
    // -----------------------------------------------------------------------

    function open() {
      if (isOpen || destroyed) return;
      isOpen = true;
      searchQuery = "";
      searchInput.value = "";
      highlightIdx = 0;

      // Position: check if we need to open above
      var rect = root.getBoundingClientRect();
      var spaceBelow = window.innerHeight - rect.bottom - 8;
      var dropMaxH = 240; // matches CSS max-height
      var openAbove = spaceBelow < dropMaxH && rect.top > dropMaxH;

      root.classList.add("blockr-select--open");
      root.classList.toggle("blockr-select--above", openAbove);
      root.setAttribute("aria-expanded", "true");

      if (mode === "single") {
        valueEl.style.display = "none";
        searchInput.style.width = "";
        searchInput.setAttribute("placeholder", selected || placeholder);
      }

      renderDropdown();
      searchInput.focus();
    }

    function close() {
      if (!isOpen) return;
      isOpen = false;
      searchQuery = "";
      searchInput.value = "";

      root.classList.remove("blockr-select--open", "blockr-select--above");
      root.setAttribute("aria-expanded", "false");
      searchInput.removeAttribute("aria-activedescendant");

      if (mode === "single") {
        valueEl.style.display = "";
        searchInput.style.width = "";
        searchInput.setAttribute("placeholder", "");
      }

      dropdown.innerHTML = "";
    }

    function toggle() {
      if (isOpen) close(); else open();
    }

    // -----------------------------------------------------------------------
    // Selection
    // -----------------------------------------------------------------------

    function selectOption(value) {
      if (mode === "single") {
        var changed = selected !== value;
        selected = value;
        close();
        render();
        if (changed && onChange) onChange(selected);
      } else {
        if (selected.indexOf(value) < 0) {
          selected.push(value);
          searchQuery = "";
          searchInput.value = "";
          highlightIdx = 0;
          render();
          renderDropdown();
          if (onChange) onChange(selected.slice());
        }
      }
    }

    function removeTag(value) {
      var idx = selected.indexOf(value);
      if (idx >= 0) {
        selected.splice(idx, 1);
        render();
        if (isOpen) renderDropdown();
        if (onChange) onChange(selected.slice());
      }
    }

    // -----------------------------------------------------------------------
    // Event handlers
    // -----------------------------------------------------------------------

    function onControlClick(e) {
      // Don't toggle if clicking a tag remove button
      if (e.target.closest && e.target.closest(".blockr-select__tag-remove")) return;
      if (mode === "single") {
        toggle();
      } else {
        if (!isOpen) open();
        searchInput.focus();
      }
    }

    function onDropdownClick(e) {
      var optEl = e.target.closest ? e.target.closest(".blockr-select__option") : null;
      if (!optEl) {
        // fallback for no .closest
        var t = e.target;
        while (t && t !== dropdown) {
          if (t.classList && t.classList.contains("blockr-select__option")) { optEl = t; break; }
          t = t.parentNode;
        }
      }
      if (optEl) {
        var val = optEl.getAttribute("data-value");
        if (val != null) selectOption(val);
      }
    }

    function onTagRemoveClick(e) {
      var btn = e.target.closest ? e.target.closest(".blockr-select__tag-remove") : null;
      if (!btn) return;
      var tag = btn.closest ? btn.closest(".blockr-select__tag") : btn.parentNode;
      if (tag) {
        var val = tag.getAttribute("data-value");
        if (val != null) removeTag(val);
      }
      e.stopPropagation();
    }

    function onSearchInput() {
      searchQuery = searchInput.value;
      highlightIdx = 0;
      if (!isOpen) open();
      else renderDropdown();
    }

    function onSearchKeydown(e) {
      var filtered = getFiltered();
      switch (e.key) {
        case "ArrowDown":
          e.preventDefault();
          if (!isOpen) { open(); return; }
          highlightIdx = (highlightIdx + 1) % (filtered.length || 1);
          renderDropdown();
          scrollHighlightIntoView();
          break;

        case "ArrowUp":
          e.preventDefault();
          if (!isOpen) { open(); return; }
          highlightIdx = (highlightIdx - 1 + (filtered.length || 1)) % (filtered.length || 1);
          renderDropdown();
          scrollHighlightIntoView();
          break;

        case "Enter":
          e.preventDefault();
          if (!isOpen) { open(); return; }
          if (highlightIdx >= 0 && highlightIdx < filtered.length) {
            selectOption(filtered[highlightIdx]);
          }
          break;

        case "Escape":
          e.preventDefault();
          close();
          root.focus();
          break;

        case "Backspace":
          if (mode === "multi" && searchInput.value === "" && selected.length > 0) {
            removeTag(selected[selected.length - 1]);
          }
          break;

        case "Tab":
          close();
          break;
      }
    }

    function scrollHighlightIntoView() {
      var el = dropdown.querySelector(".blockr-select__option--highlighted");
      if (el) el.scrollIntoView({ block: "nearest" });
    }

    // Click outside to close
    function onDocumentClick(e) {
      if (!root.contains(e.target)) close();
    }

    // Open on focus (single mode — clicking the root should open)
    function onRootFocus(e) {
      // Only if focus came from outside (tab into)
      if (mode === "single" && !isOpen && e.target === root) {
        // Don't auto-open on tab — let the user press Enter/Space/Arrow
      }
    }

    function onRootKeydown(e) {
      if (e.target === root && !isOpen) {
        if (e.key === "Enter" || e.key === " " || e.key === "ArrowDown" || e.key === "ArrowUp") {
          e.preventDefault();
          open();
        }
      }
    }

    // -----------------------------------------------------------------------
    // Drag and drop (multi mode)
    // -----------------------------------------------------------------------

    function onDragStart(e) {
      var tag = e.target.closest ? e.target.closest(".blockr-select__tag") : null;
      if (!tag) return;
      dragValue = tag.getAttribute("data-value");
      e.dataTransfer.effectAllowed = "move";
      // Required for Firefox
      e.dataTransfer.setData("text/plain", dragValue);
      tag.classList.add("blockr-select__tag--dragging");
    }

    function onDragOver(e) {
      if (dragValue == null) return;
      e.preventDefault();
      e.dataTransfer.dropEffect = "move";

      var tag = e.target.closest ? e.target.closest(".blockr-select__tag") : null;
      if (!tag || tag.getAttribute("data-value") === dragValue) {
        clearDropIndicators();
        return;
      }

      var rect = tag.getBoundingClientRect();
      var mid = rect.left + rect.width / 2;
      var side = e.clientX < mid ? "before" : "after";

      if (tag !== dragOverTag || side !== dragSide) {
        clearDropIndicators();
        dragOverTag = tag;
        dragSide = side;
        tag.classList.add("blockr-select__tag--drop-" + side);
      }
    }

    function onDragEnd(e) {
      clearDropIndicators();
      var tags = tagsEl ? tagsEl.querySelectorAll(".blockr-select__tag--dragging") : [];
      for (var i = 0; i < tags.length; i++) tags[i].classList.remove("blockr-select__tag--dragging");
      dragValue = null;
      dragOverTag = null;
      dragSide = null;
    }

    function onDrop(e) {
      e.preventDefault();
      if (dragValue == null || !dragOverTag) { onDragEnd(e); return; }

      var targetVal = dragOverTag.getAttribute("data-value");
      var fromIdx = selected.indexOf(dragValue);
      var toIdx = selected.indexOf(targetVal);
      if (fromIdx < 0 || toIdx < 0 || fromIdx === toIdx) { onDragEnd(e); return; }

      // Remove from old position
      selected.splice(fromIdx, 1);
      // Recalculate target index after removal
      toIdx = selected.indexOf(targetVal);
      var insertIdx = dragSide === "after" ? toIdx + 1 : toIdx;
      selected.splice(insertIdx, 0, dragValue);

      onDragEnd(e);
      render();
      if (onChange) onChange(selected.slice());
    }

    function clearDropIndicators() {
      if (!tagsEl) return;
      var indicators = tagsEl.querySelectorAll(".blockr-select__tag--drop-before, .blockr-select__tag--drop-after");
      for (var i = 0; i < indicators.length; i++) {
        indicators[i].classList.remove("blockr-select__tag--drop-before", "blockr-select__tag--drop-after");
      }
    }

    // -----------------------------------------------------------------------
    // Bind events
    // -----------------------------------------------------------------------

    control.addEventListener("click", onControlClick);
    dropdown.addEventListener("click", onDropdownClick);
    searchInput.addEventListener("input", onSearchInput);
    searchInput.addEventListener("keydown", onSearchKeydown);
    document.addEventListener("click", onDocumentClick, true);
    root.addEventListener("keydown", onRootKeydown);

    if (mode === "multi") {
      control.addEventListener("click", onTagRemoveClick);
      if (reorderable && tagsEl) {
        tagsEl.addEventListener("dragstart", onDragStart);
        tagsEl.addEventListener("dragover", onDragOver);
        tagsEl.addEventListener("dragend", onDragEnd);
        tagsEl.addEventListener("drop", onDrop);
      }
    }

    // Initial render
    render();

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    var instance = {
      el: root,

      setOptions: function(opts, sel) {
        options = opts || [];
        if (mode === "single") {
          if (sel != null && options.indexOf(sel) >= 0) {
            selected = sel;
          } else if (options.length > 0) {
            selected = options[0];
          } else {
            selected = "";
          }
        } else {
          // Multi: preserve valid selections
          if (sel != null) {
            selected = sel.filter(function(v) { return options.indexOf(v) >= 0; });
          } else {
            selected = selected.filter(function(v) { return options.indexOf(v) >= 0; });
          }
        }
        render();
        if (isOpen) renderDropdown();
      },

      getValue: function() {
        if (mode === "single") return selected || "";
        return selected.slice();
      },

      destroy: function() {
        if (destroyed) return;
        destroyed = true;
        close();
        control.removeEventListener("click", onControlClick);
        dropdown.removeEventListener("click", onDropdownClick);
        searchInput.removeEventListener("input", onSearchInput);
        searchInput.removeEventListener("keydown", onSearchKeydown);
        document.removeEventListener("click", onDocumentClick, true);
        root.removeEventListener("keydown", onRootKeydown);

        if (mode === "multi") {
          control.removeEventListener("click", onTagRemoveClick);
          if (reorderable && tagsEl) {
            tagsEl.removeEventListener("dragstart", onDragStart);
            tagsEl.removeEventListener("dragover", onDragOver);
            tagsEl.removeEventListener("dragend", onDragEnd);
            tagsEl.removeEventListener("drop", onDrop);
          }
        }

        removeNode(root);
      }
    };

    return instance;
  }

  // -------------------------------------------------------------------------
  // Public API
  // -------------------------------------------------------------------------

  window.BlockrSelect = {
    single: function(container, config) {
      return createSelect(container, config, "single");
    },
    multi: function(container, config) {
      return createSelect(container, config, "multi");
    }
  };

})();
