// @ts-check
/**
 * Blockr.Select — lightweight select component (single + multi)
 *
 * Replaces selectize.js in blockr blocks.
 * Depends on blockr-core.js (Blockr namespace, icons, utilities).
 *
 * API:
 *   Blockr.Select.single(container, config) -> { el, setOptions, getValue, updateOptions, setLoading, destroy }
 *   Blockr.Select.multi(container, config)  -> { el, setOptions, getValue, updateOptions, setLoading, destroy }
 *
 * config.onOpen fires on every dropdown-open; with config.loading = true the
 * dropdown shows "Loading…" until setLoading(false). Together these support
 * lazily fetching the option list on first open (see filter-block.js).
 */
(() => {
  'use strict';

  // Helpers for {value, label} option objects
  /** @param {BlockrSelectOption} o @returns {string} */
  const optValue = (o) => typeof o === 'object' && o !== null ? o.value : o;
  /** @param {BlockrSelectOption} o @returns {string} */
  const optLabel = (o) => typeof o === 'object' && o !== null ? (o.label || '') : '';
  /** @param {BlockrSelectOption[]} opts @param {string} val */
  const findOpt = (opts, val) => opts.find(o => optValue(o) === val);
  /** @param {HTMLElement} el @param {BlockrSelectOption} o */
  const fillOptContent = (el, o) => {
    el.textContent = '';
    const val = optValue(o);
    const lbl = optLabel(o);
    el.appendChild(document.createTextNode(val));
    if (lbl) {
      const span = document.createElement('span');
      span.className = 'blockr-select__opt-label';
      span.textContent = lbl;
      span.setAttribute('title', lbl);
      el.appendChild(span);
    }
  };

  /**
   * @param {HTMLElement} container
   * @param {BlockrSelectConfig} config
   * @param {'single' | 'multi'} mode
   */
  const createSelect = (container, config, mode) => {
    const id = Blockr.uid('bsel');
    const dropdownId = `${id}-lb`;

    // State
    let options = config.options || [];
    /** @type {string | string[]} string in 'single' mode, string[] in 'multi' */
    let selected = mode === 'multi'
      ? (config.selected || []).slice()
      : (config.selected != null ? config.selected : (options.length > 0 ? optValue(options[0]) : ''));
    const placeholder = config.placeholder || '';
    const reorderable = mode === 'multi' && config.reorderable !== false;
    const onChange = config.onChange || null;
    const onOpen = config.onOpen || null;
    const onSearch = config.onSearch || null;
    // Cap how many options get DOM nodes per render. The full list stays
    // searchable; rendering 50K divs froze the tab on every open/keystroke.
    const maxRendered = config.maxRendered || 200;
    let truncated = 0;
    // Server-search mode (high-cardinality columns): the option list is a
    // server-truncated page; typing re-queries via onSearch instead of
    // relying on client-side filtering alone. Activated by setSearchInfo.
    let serverTruncated = false;
    let serverTotal = 0;
    /** @type {ReturnType<typeof setTimeout> | null} */
    let searchTimer = null;
    let loading = !!config.loading;
    let isOpen = false;
    let searchQuery = '';
    let highlightIdx = -1;
    let destroyed = false;

    // Drag state (multi only)
    /** @type {string | null} */
    let dragValue = null;
    /** @type {Element | null} */
    let dragOverTag = null;
    /** @type {'before' | 'after' | null} */
    let dragSide = null;

    // DOM
    const root = document.createElement('div');
    root.className = `blockr-select blockr-select--${mode}`;
    root.setAttribute('role', 'combobox');
    root.setAttribute('aria-expanded', 'false');
    root.setAttribute('aria-haspopup', 'listbox');
    root.setAttribute('aria-owns', dropdownId);

    const control = document.createElement('div');
    control.className = 'blockr-select__control';

    // Mode-correlated: tagsEl is set iff mode === 'multi', valueEl iff
    // 'single'; every use is behind the matching mode check.
    /** @type {HTMLDivElement} */
    let tagsEl = /** @type {any} */ (null);
    /** @type {HTMLSpanElement} */
    let valueEl = /** @type {any} */ (null);

    if (mode === 'multi') {
      tagsEl = document.createElement('div');
      tagsEl.className = 'blockr-select__tags';
      control.appendChild(tagsEl);
    } else {
      valueEl = document.createElement('span');
      valueEl.className = 'blockr-select__value';
      control.appendChild(valueEl);
    }

    const searchInput = document.createElement('input');
    searchInput.type = 'text';
    searchInput.className = 'blockr-select__search';
    searchInput.setAttribute('aria-autocomplete', 'list');
    searchInput.setAttribute('aria-controls', dropdownId);
    searchInput.setAttribute('autocomplete', 'off');
    searchInput.setAttribute('autocorrect', 'off');
    searchInput.setAttribute('autocapitalize', 'off');
    searchInput.setAttribute('spellcheck', 'false');
    if (mode === 'multi') {
      searchInput.setAttribute('placeholder', placeholder);
      tagsEl.appendChild(searchInput);
    } else {
      control.appendChild(searchInput);
    }

    if (mode === 'single') {
      const arrow = document.createElement('span');
      arrow.className = 'blockr-select__arrow';
      arrow.innerHTML = Blockr.icons.chevron;
      control.appendChild(arrow);
    }

    const dropdown = document.createElement('div');
    dropdown.className = 'blockr-select__dropdown';
    dropdown.id = dropdownId;
    dropdown.setAttribute('role', 'listbox');

    root.appendChild(control);
    container.appendChild(root);

    // Portal: dropdown lives on document.body while open so it escapes any
    // clipping / paint-containment / stacking-context ancestors (Dockview
    // panels, offcanvas, modals, …). See blockr.design/open/blockr-select-portal.

    const computePosition = () => {
      const r = root.getBoundingClientRect();
      const dropH = dropdown.offsetHeight || 240;
      const spaceBelow = window.innerHeight - r.bottom - 8;
      const flipAbove = spaceBelow < dropH && r.top > dropH;

      dropdown.style.position = 'fixed';
      dropdown.style.width    = r.width + 'px';
      dropdown.style.left     = r.left + 'px';
      dropdown.style.bottom   = 'auto';

      if (flipAbove) {
        dropdown.style.top = (r.top - dropH - 4) + 'px';
        root.classList.add('blockr-select--above');
      } else {
        dropdown.style.top = (r.bottom + 4) + 'px';
        root.classList.remove('blockr-select--above');
      }
    };

    const onScrollOrResize = () => { if (isOpen) computePosition(); };

    // --- Rendering ---

    const getFiltered = () => {
      const q = searchQuery.toLowerCase();
      /** @type {BlockrSelectOption[]} */
      const result = [];
      truncated = 0;
      for (let i = 0; i < options.length; i++) {
        const opt = options[i];
        const val = optValue(opt);
        if (mode === 'multi' && selected.indexOf(val) >= 0) continue;
        if (q) {
          const matchVal = val.toLowerCase().indexOf(q) >= 0;
          const matchLabel = optLabel(opt).toLowerCase().indexOf(q) >= 0;
          if (!matchVal && !matchLabel) continue;
        }
        if (result.length < maxRendered) result.push(opt);
        else truncated++;
      }
      return result;
    };

    const renderDropdown = () => {
      const filtered = getFiltered();
      dropdown.innerHTML = '';

      if (loading) {
        const empty = document.createElement('div');
        empty.className = 'blockr-select__empty';
        empty.textContent = 'Loading…';
        dropdown.appendChild(empty);
        highlightIdx = -1;
        return;
      }

      if (filtered.length === 0) {
        const empty = document.createElement('div');
        empty.className = 'blockr-select__empty';
        empty.textContent = searchQuery
          ? 'No matches'
          : (mode === 'multi' ? 'All selected' : 'No options');
        dropdown.appendChild(empty);
        highlightIdx = -1;
        return;
      }

      if (highlightIdx >= filtered.length) highlightIdx = filtered.length - 1;
      if (highlightIdx < 0 && filtered.length > 0) highlightIdx = 0;

      for (let i = 0; i < filtered.length; i++) {
        const opt = filtered[i];
        const val = optValue(opt);
        const div = document.createElement('div');
        div.className = 'blockr-select__option';
        if (i === highlightIdx) div.className += ' blockr-select__option--highlighted';
        if (mode === 'single' && val === selected) div.className += ' blockr-select__option--selected';
        div.setAttribute('role', 'option');
        div.setAttribute('id', `${id}-opt-${i}`);
        div.setAttribute('aria-selected', (mode === 'single' && val === selected) ? 'true' : 'false');
        div.setAttribute('data-value', val);
        fillOptContent(div, opt);
        dropdown.appendChild(div);
      }

      if (serverTruncated) {
        const more = document.createElement('div');
        more.className = 'blockr-select__empty';
        more.textContent = `${serverTotal.toLocaleString()} values — type to search`;
        dropdown.appendChild(more);
      } else if (truncated > 0) {
        const more = document.createElement('div');
        more.className = 'blockr-select__empty';
        more.textContent = `+${truncated.toLocaleString()} more — type to narrow`;
        dropdown.appendChild(more);
      }

      if (highlightIdx >= 0) {
        searchInput.setAttribute('aria-activedescendant', `${id}-opt-${highlightIdx}`);
      }
    };

    const renderValue = () => {
      if (mode !== 'single') return;
      if (selected) {
        const opt = findOpt(options, /** @type {string} */ (selected));
        if (opt) { fillOptContent(valueEl, opt); } else { valueEl.textContent = /** @type {string} */ (selected); }
        valueEl.classList.remove('blockr-select__value--placeholder');
        searchInput.setAttribute('placeholder', '');
      } else {
        valueEl.textContent = placeholder;
        valueEl.classList.add('blockr-select__value--placeholder');
        searchInput.setAttribute('placeholder', placeholder);
      }
    };

    const renderTags = () => {
      if (mode !== 'multi') return;
      // Remove only tag elements, preserve the search input
      tagsEl.querySelectorAll('.blockr-select__tag').forEach(t => t.remove());
      for (let i = 0; i < selected.length; i++) {
        const val = selected[i];
        const tag = document.createElement('span');
        tag.className = 'blockr-select__tag';
        tag.setAttribute('data-value', val);
        if (reorderable) tag.setAttribute('draggable', 'true');

        const label = document.createElement('span');
        label.className = 'blockr-select__tag-label';
        label.textContent = val;
        tag.appendChild(label);

        const removeBtn = document.createElement('button');
        removeBtn.type = 'button';
        removeBtn.className = 'blockr-select__tag-remove';
        removeBtn.setAttribute('aria-label', `Remove ${val}`);
        removeBtn.setAttribute('tabindex', '-1');
        removeBtn.innerHTML = Blockr.icons.remove;
        tag.appendChild(removeBtn);

        tagsEl.insertBefore(tag, searchInput);
      }
      searchInput.setAttribute('placeholder', selected.length === 0 ? placeholder : '');
    };

    const render = () => {
      if (mode === 'single') renderValue();
      if (mode === 'multi') renderTags();
      if (isOpen) renderDropdown();
    };

    // --- Open / close ---

    const open = () => {
      if (isOpen || destroyed) return;
      isOpen = true;
      searchQuery = '';
      searchInput.value = '';
      highlightIdx = 0;

      if (dropdown.parentElement !== document.body) {
        document.body.appendChild(dropdown);
      }
      dropdown.style.display = 'block';

      root.classList.add('blockr-select--open');
      root.setAttribute('aria-expanded', 'true');

      if (mode === 'single') {
        valueEl.style.display = 'none';
        searchInput.style.width = '';
        searchInput.setAttribute('placeholder', /** @type {string} */ (selected) || placeholder);
      }

      renderDropdown();
      computePosition();
      window.addEventListener('scroll', onScrollOrResize, { capture: true, passive: true });
      window.addEventListener('resize', onScrollOrResize, { passive: true });
      searchInput.focus();

      if (onOpen) onOpen();
    };

    const close = () => {
      if (!isOpen) return;
      isOpen = false;
      searchQuery = '';
      searchInput.value = '';

      window.removeEventListener('scroll', onScrollOrResize, { capture: true });
      window.removeEventListener('resize', onScrollOrResize);

      dropdown.style.display = '';

      root.classList.remove('blockr-select--open', 'blockr-select--above');
      root.setAttribute('aria-expanded', 'false');
      searchInput.removeAttribute('aria-activedescendant');

      if (mode === 'single') {
        valueEl.style.display = '';
        searchInput.style.width = '';
        searchInput.setAttribute('placeholder', '');
      }

      dropdown.innerHTML = '';
    };

    const toggle = () => { isOpen ? close() : open(); };

    // --- Selection ---

    /** @param {string} value */
    const selectOption = (value) => {
      if (mode === 'single') {
        const changed = selected !== value;
        selected = value;
        close();
        render();
        if (changed && onChange) onChange(selected);
      } else {
        if (selected.indexOf(value) < 0) {
          /** @type {string[]} */ (selected).push(value);
          searchQuery = '';
          searchInput.value = '';
          highlightIdx = 0;
          render();
          renderDropdown();
          if (onChange) onChange(selected.slice());
        }
      }
    };

    /** @param {string} value */
    const removeTag = (value) => {
      const idx = selected.indexOf(value);
      if (idx >= 0) {
        /** @type {string[]} */ (selected).splice(idx, 1);
        render();
        if (isOpen) renderDropdown();
        if (onChange) onChange(selected.slice());
      }
    };

    // --- Event handlers ---

    /** @param {MouseEvent} e */
    const onControlClick = (e) => {
      if (/** @type {Element} */ (e.target).closest('.blockr-select__tag-remove')) return;
      if (mode === 'single') {
        toggle();
      } else {
        if (!isOpen) open();
        searchInput.focus();
      }
    };

    /** @param {MouseEvent} e */
    const onDropdownClick = (e) => {
      const optEl = /** @type {Element} */ (e.target).closest('.blockr-select__option');
      if (optEl) {
        const val = optEl.getAttribute('data-value');
        if (val != null) selectOption(val);
      }
    };

    /** @param {MouseEvent} e */
    const onTagRemoveClick = (e) => {
      const btn = /** @type {Element} */ (e.target).closest('.blockr-select__tag-remove');
      if (!btn) return;
      const tag = btn.closest('.blockr-select__tag');
      if (tag) {
        const val = tag.getAttribute('data-value');
        if (val != null) removeTag(val);
      }
      e.stopPropagation();
    };

    const onSearchInput = () => {
      searchQuery = searchInput.value;
      highlightIdx = 0;
      if (!isOpen) open();
      else renderDropdown();
      // Server-search mode: re-query after the user pauses typing. The
      // client-side filter above gives instant feedback on the loaded page;
      // the server response then replaces the option list via updateOptions.
      if (serverTruncated && onSearch) {
        if (searchTimer) clearTimeout(searchTimer);
        searchTimer = setTimeout(() => onSearch(searchQuery), 250);
      }
    };

    /** @param {KeyboardEvent} e */
    const onSearchKeydown = (e) => {
      const filtered = getFiltered();
      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();
          if (!isOpen) { open(); return; }
          highlightIdx = (highlightIdx + 1) % (filtered.length || 1);
          renderDropdown();
          scrollHighlightIntoView();
          break;
        case 'ArrowUp':
          e.preventDefault();
          if (!isOpen) { open(); return; }
          highlightIdx = (highlightIdx - 1 + (filtered.length || 1)) % (filtered.length || 1);
          renderDropdown();
          scrollHighlightIntoView();
          break;
        case 'Enter':
          e.preventDefault();
          if (!isOpen) { open(); return; }
          if (highlightIdx >= 0 && highlightIdx < filtered.length) {
            selectOption(optValue(filtered[highlightIdx]));
          }
          break;
        case 'Escape':
          e.preventDefault();
          close();
          root.focus();
          break;
        case 'Backspace':
          if (mode === 'multi' && searchInput.value === '' && selected.length > 0) {
            removeTag(selected[selected.length - 1]);
          }
          break;
        case 'Tab':
          close();
          break;
      }
    };

    const scrollHighlightIntoView = () => {
      dropdown.querySelector('.blockr-select__option--highlighted')
        ?.scrollIntoView({ block: 'nearest' });
    };

    /** @param {MouseEvent} e */
    const onDocumentClick = (e) => {
      if (root.contains(/** @type {Node | null} */ (e.target)) || dropdown.contains(/** @type {Node | null} */ (e.target))) return;
      close();
    };

    /** @param {KeyboardEvent} e */
    const onRootKeydown = (e) => {
      if (e.target === root && !isOpen) {
        if (e.key === 'Enter' || e.key === ' ' || e.key === 'ArrowDown' || e.key === 'ArrowUp') {
          e.preventDefault();
          open();
        }
      }
    };

    // --- Drag and drop (multi mode) ---

    const clearDropIndicators = () => {
      if (!tagsEl) return;
      tagsEl.querySelectorAll('.blockr-select__tag--drop-before, .blockr-select__tag--drop-after')
        .forEach(el => el.classList.remove('blockr-select__tag--drop-before', 'blockr-select__tag--drop-after'));
    };

    /** @param {DragEvent} e */
    const onDragStart = (e) => {
      const tag = /** @type {Element} */ (e.target).closest('.blockr-select__tag');
      if (!tag) return;
      dragValue = tag.getAttribute('data-value');
      /** @type {DataTransfer} */ (e.dataTransfer).effectAllowed = 'move';
      /** @type {DataTransfer} */ (e.dataTransfer).setData('text/plain', /** @type {string} */ (dragValue));
      tag.classList.add('blockr-select__tag--dragging');
    };

    /** @param {DragEvent} e */
    const onDragOver = (e) => {
      if (dragValue == null) return;
      e.preventDefault();
      /** @type {DataTransfer} */ (e.dataTransfer).dropEffect = 'move';

      const tag = /** @type {Element} */ (e.target).closest('.blockr-select__tag');
      if (!tag || tag.getAttribute('data-value') === dragValue) {
        clearDropIndicators();
        return;
      }

      const rect = tag.getBoundingClientRect();
      const mid = rect.left + rect.width / 2;
      const side = e.clientX < mid ? 'before' : 'after';

      if (tag !== dragOverTag || side !== dragSide) {
        clearDropIndicators();
        dragOverTag = tag;
        dragSide = side;
        tag.classList.add(`blockr-select__tag--drop-${side}`);
      }
    };

    const onDragEnd = () => {
      clearDropIndicators();
      tagsEl?.querySelectorAll('.blockr-select__tag--dragging')
        .forEach(el => el.classList.remove('blockr-select__tag--dragging'));
      dragValue = null;
      dragOverTag = null;
      dragSide = null;
    };

    /** @param {DragEvent} e */
    const onDrop = (e) => {
      e.preventDefault();
      if (dragValue == null || !dragOverTag) { onDragEnd(); return; }

      const targetVal = /** @type {string} */ (dragOverTag.getAttribute('data-value'));
      const fromIdx = selected.indexOf(dragValue);
      let toIdx = selected.indexOf(targetVal);
      if (fromIdx < 0 || toIdx < 0 || fromIdx === toIdx) { onDragEnd(); return; }

      /** @type {string[]} */ (selected).splice(fromIdx, 1);
      toIdx = selected.indexOf(targetVal);
      const insertIdx = dragSide === 'after' ? toIdx + 1 : toIdx;
      /** @type {string[]} */ (selected).splice(insertIdx, 0, dragValue);

      onDragEnd();
      render();
      if (onChange) onChange(selected.slice());
    };

    // --- Bind events ---

    control.addEventListener('click', onControlClick);
    dropdown.addEventListener('click', onDropdownClick);
    searchInput.addEventListener('input', onSearchInput);
    searchInput.addEventListener('keydown', onSearchKeydown);
    document.addEventListener('click', onDocumentClick, true);
    root.addEventListener('keydown', onRootKeydown);

    if (mode === 'multi') {
      control.addEventListener('click', onTagRemoveClick);
      if (reorderable && tagsEl) {
        tagsEl.addEventListener('dragstart', onDragStart);
        tagsEl.addEventListener('dragover', onDragOver);
        tagsEl.addEventListener('dragend', onDragEnd);
        tagsEl.addEventListener('drop', onDrop);
      }
    }

    render();

    // --- Public API ---

    return {
      el: root,

      /**
       * @param {BlockrSelectOption[] | BlockrSelectOption | null | undefined} opts
       * @param {string | string[] | null} [sel]
       */
      setOptions(opts, sel) {
        options = Array.isArray(opts) ? opts : (opts != null ? [opts] : []);
        const vals = options.map(optValue);
        if (mode === 'single') {
          if (sel != null && vals.indexOf(/** @type {string} */ (sel)) >= 0) {
            selected = sel;
          } else if (options.length > 0) {
            selected = optValue(options[0]);
          } else {
            selected = '';
          }
        } else {
          if (sel != null) {
            selected = /** @type {string[]} */ (sel).filter(v => vals.indexOf(v) >= 0);
          } else {
            selected = /** @type {string[]} */ (selected).filter(v => vals.indexOf(v) >= 0);
          }
        }
        render();
        if (isOpen) renderDropdown();
      },

      getValue() {
        return mode === 'single' ? (selected || '') : selected.slice();
      },

      // Swap the option list without touching the current selection (setOptions
      // filters selected against the new options, which would drop chips whose
      // value list hasn't arrived yet). Used by lazy value loading.
      /** @param {BlockrSelectOption[] | BlockrSelectOption | null | undefined} opts */
      updateOptions(opts) {
        options = Array.isArray(opts) ? opts : (opts != null ? [opts] : []);
        render();
        if (isOpen) computePosition();
      },

      /** @param {boolean} flag */
      setLoading(flag) {
        loading = !!flag;
        if (isOpen) renderDropdown();
      },

      // Enter/leave server-search mode from a column-values response.
      // `truncated` means the full value list exceeds the server's limit
      // (sticky across queries); `total` is the full distinct count.
      /** @param {{ total?: number, truncated?: boolean } | null | undefined} info */
      setSearchInfo(info) {
        serverTruncated = !!(info && info.truncated);
        serverTotal = (info && info.total) || 0;
        if (isOpen) renderDropdown();
      },

      destroy() {
        if (destroyed) return;
        destroyed = true;
        if (searchTimer) clearTimeout(searchTimer);
        close();

        if (dropdown.parentElement === document.body) {
          dropdown.remove();
        }

        control.removeEventListener('click', onControlClick);
        dropdown.removeEventListener('click', onDropdownClick);
        searchInput.removeEventListener('input', onSearchInput);
        searchInput.removeEventListener('keydown', onSearchKeydown);
        document.removeEventListener('click', onDocumentClick, true);
        root.removeEventListener('keydown', onRootKeydown);

        if (mode === 'multi') {
          control.removeEventListener('click', onTagRemoveClick);
          if (reorderable && tagsEl) {
            tagsEl.removeEventListener('dragstart', onDragStart);
            tagsEl.removeEventListener('dragover', onDragOver);
            tagsEl.removeEventListener('dragend', onDragEnd);
            tagsEl.removeEventListener('drop', onDrop);
          }
        }

        Blockr.removeNode(root);
      }
    };
  };

  Blockr.Select = {
    single: (container, config) => /** @type {BlockrSelectSingleHandle} */ (createSelect(container, config, 'single')),
    multi: (container, config) => /** @type {BlockrSelectMultiHandle} */ (createSelect(container, config, 'multi'))
  };
})();
