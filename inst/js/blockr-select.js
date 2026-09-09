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
  /* Both facts, and `labelFirst` decides which one leads.
   *
   * A menu opened from a word has to lead with the word that was clicked. A
   * sentence printing `{label(@color)}` says "Actual Treatment", and a list
   * whose rows read "TRTA *Actual Treatment*" puts what the reader just
   * clicked in the muted half of the row and bolds a string that is nowhere
   * on screen. blockr.docs design-system/pinned-controls.md.
   *
   * @param {HTMLElement} el @param {BlockrSelectOption} o @param {boolean} [labelFirst]
   */
  const fillOptContent = (el, o, labelFirst) => {
    el.textContent = '';
    const val = optValue(o);
    const lbl = optLabel(o);
    const lead = (labelFirst && lbl) ? lbl : val;
    const trail = (labelFirst && lbl) ? val : lbl;
    el.appendChild(document.createTextNode(lead));
    // The element (or an ancestor) ellipsizes on overflow, so always offer
    // the full text on hover.
    el.title = lbl ? `${val} — ${lbl}` : val;
    if (trail) {
      const span = document.createElement('span');
      span.className = 'blockr-select__opt-label';
      span.textContent = trail;
      el.appendChild(span);
    }
  };

  /* Shorten a tag label from the MIDDLE.
   *
   * CSS can only ellipsize at the end, and for the values these controls carry
   * the distinguishing word is as often the last one as the first: "Xanomeline
   * High Dose" and "Xanomeline Low Dose" both end-ellipsize to "Xanomelin…".
   * Cutting the middle costs the same width and keeps both ends. The full value
   * stays on the tag's title.
   *
   * @param {string} value @param {number} cap @returns {string}
   */
  const midTruncate = (value, cap) => {
    if (!cap || value.length <= cap) return value;
    const head = Math.ceil((cap - 1) / 2);
    const tail = Math.floor((cap - 1) / 2);
    return value.slice(0, head) + '\u2026' + value.slice(value.length - tail);
  };

  /* How many tags fit on one row, given their measured widths.
   *
   * Split out from the DOM work because it is the part worth testing: the chip
   * has to fit too, so dropping a tag can force dropping the next one up when
   * the count goes from "+9" to "+10". Widths are in visual order and exclude
   * the gap, which is added between neighbours only.
   *
   * @param {number[]} widths @param {number} avail
   * @param {number} gap @param {number} chipWidth
   * @returns {number}
   */
  const fitCount = (widths, avail, gap, chipWidth) => {
    let used = 0;
    let shown = 0;
    for (let i = 0; i < widths.length; i++) {
      const w = widths[i] + (shown ? gap : 0);
      if (used + w > avail) break;
      used += w;
      shown++;
    }
    if (shown === widths.length) return shown;
    while (shown > 0 && used + gap + chipWidth > avail) {
      shown--;
      used -= widths[shown] + (shown ? gap : 0);
    }
    return shown;
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
    // Single mode only: opt out of the first-option fallback so '' means
    // "nothing selected" and survives setOptions(). Without it, any option
    // refresh silently picks option 0 — fine for a column picker, wrong
    // wherever an unrequested pick would change what the user is looking at.
    const allowEmpty = mode === 'single' && config.allowEmpty === true;
    // Headless: the dropdown IS the widget, anchored to something the caller
    // owns (a word in a block's sentence) instead of to a control this
    // builds. Same options, same tick, same search, same keyboard -- a menu
    // that drifts from the select is a second idiom nobody asked for.
    const headless = config.headless === true;
    const menuTitle = config.title || '';
    const labelFirst = config.labelFirst === true;
    // The select's own rule for when a list needs filtering, in one place.
    const searchAfter = config.searchAfter != null ? config.searchAfter : 8;
    const onClose = config.onClose || null;
    /** @type {string | string[]} string in 'single' mode, string[] in 'multi' */
    let selected = mode === 'multi'
      ? (config.selected || []).slice()
      : (config.selected != null
          ? config.selected
          : (allowEmpty || options.length === 0 ? '' : optValue(options[0])));
    const placeholder = config.placeholder || '';
    const reorderable = mode === 'multi' && config.reorderable !== false;
    // Keep the tags on one row and collapse the overflow into a "+N" chip,
    // instead of wrapping and growing the control a row per tag. Auto-generated
    // parameter bands (function block, code block) sit in a grid where the
    // tallest field sets the row height, so a wrapping select there is what
    // makes the whole band tall.
    const singleLine = mode === 'multi' && config.singleLine === true;
    // Tag labels longer than this are shortened in the middle (0 = never).
    const maxTagChars = mode === 'multi' && config.maxTagChars > 0
      ? config.maxTagChars
      : 0;
    // Single-line only: the chip has been clicked, so the control wraps and
    // shows every tag until the click lands somewhere else. The dropdown lists
    // only UNselected options, so without this a tag past the first row could
    // be neither seen nor removed.
    let expanded = false;
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
    if (singleLine) root.classList.add('blockr-select--single-line');
    if (headless) root.classList.add('blockr-select--headless');
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

    /** @type {HTMLSpanElement} */
    let moreEl = /** @type {any} */ (null);

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

    // In headless mode the control is never shown, so it is never mounted;
    // `searchInput` moves into the dropdown's head below and `valueEl` simply
    // renders into a detached node.
    if (!headless) root.appendChild(control);
    container.appendChild(root);

    // Head of a headless dropdown: the role's name, and the filter box past
    // the same threshold the select uses elsewhere. Everything after
    // `headEnd` is options, and only that part is cleared on re-render, so a
    // keystroke cannot move the input the user is typing into.
    /** @type {Comment | null} */
    let headEnd = null;
    if (headless) {
      if (menuTitle) {
        const t = document.createElement('div');
        t.className = 'blockr-select__menu-title';
        t.textContent = menuTitle;
        dropdown.appendChild(t);
      }
      // Always mounted, because focus is what makes the arrows, Enter and
      // type-ahead work; only shown once the list is long enough to need
      // filtering. A short menu still filters as you type, which is what a
      // native menu does.
      searchInput.className += (options.length > searchAfter)
        ? ' blockr-select__search--menu'
        : ' blockr-select__search--menu blockr-select__search--offscreen';
      searchInput.setAttribute('placeholder', config.searchPlaceholder || 'Filter');
      dropdown.appendChild(searchInput);
      headEnd = document.createComment('opts');
      dropdown.appendChild(headEnd);
    }

    // Portal: dropdown lives on document.body while open so it escapes any
    // clipping / paint-containment / stacking-context ancestors (Dockview
    // panels, offcanvas, modals, …). See blockr.design/open/blockr-select-portal.

    const computePosition = () => {
      // Anchored to the caller's element when there is no control of our own.
      const r = (config.anchor || root).getBoundingClientRect();
      const dropH = dropdown.offsetHeight || 240;
      const spaceBelow = window.innerHeight - r.bottom - 8;
      const flipAbove = spaceBelow < dropH && r.top > dropH;

      dropdown.style.position = 'fixed';
      dropdown.style.bottom   = 'auto';
      if (headless) {
        // A word is not a control: the panel sizes to its own content, and
        // has to be pulled back inside the viewport rather than lining up.
        // `right` is 0 in the stylesheet (a dropdown normally spans its
        // control), which would stretch a fixed box to the window edge and
        // make every menu exactly max-width wide.
        dropdown.style.right = 'auto';
        dropdown.style.width = '';
        dropdown.style.minWidth = (config.minWidth || 190) + 'px';
        dropdown.style.maxWidth = (config.maxWidth || 320) + 'px';
        const w = dropdown.offsetWidth || 190;
        dropdown.style.left = Math.max(8, Math.min(r.left,
          document.documentElement.clientWidth - w - 8)) + 'px';
      } else {
        dropdown.style.width = r.width + 'px';
        dropdown.style.left  = r.left + 'px';
      }

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
          // `val` may be a number (numeric value pickers send JSON numbers),
          // so stringify before the case-insensitive substring match.
          const matchVal = String(val).toLowerCase().indexOf(q) >= 0;
          const matchLabel = optLabel(opt).toLowerCase().indexOf(q) >= 0;
          if (!matchVal && !matchLabel) continue;
        }
        if (result.length < maxRendered) result.push(opt);
        else truncated++;
      }
      return result;
    };

    // Everything after `headEnd` is options. In headless mode the title and
    // the filter box sit before it and survive a re-render; without this the
    // input would be re-created (or moved, which blurs it) on every keystroke.
    const clearOptions = () => {
      if (!headEnd) { dropdown.innerHTML = ''; return; }
      while (headEnd.nextSibling) dropdown.removeChild(headEnd.nextSibling);
    };

    const renderDropdown = () => {
      const filtered = getFiltered();
      clearOptions();

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
        fillOptContent(div, opt, labelFirst);
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
      // Headless has no control to render into, and writing the search
      // placeholder here would wipe the menu's "Filter" prompt.
      if (headless) return;
      if (selected) {
        const opt = findOpt(options, /** @type {string} */ (selected));
        if (opt) { fillOptContent(valueEl, opt); } else { valueEl.textContent = /** @type {string} */ (selected); valueEl.title = /** @type {string} */ (selected); }
        // valueEl has pointer-events: none, so its title never triggers —
        // hover happens on the control.
        control.title = valueEl.title;
        valueEl.classList.remove('blockr-select__value--placeholder');
        searchInput.setAttribute('placeholder', '');
      } else {
        valueEl.textContent = placeholder;
        valueEl.title = '';
        control.title = '';
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
        // Match the dropdown option and single-select value display: show the
        // value in normal font with the option's label muted on the side
        // (`.blockr-select__opt-label`). Unnamed options fall back to bare value.
        const opt = findOpt(options, val);
        if (maxTagChars && val.length > maxTagChars) {
          const lbl = opt ? optLabel(opt) : '';
          label.textContent = midTruncate(val, maxTagChars);
          label.title = lbl ? `${val} \u2014 ${lbl}` : val;
        } else if (opt) {
          fillOptContent(label, opt);
        } else {
          label.textContent = val;
          label.title = val;
        }
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

    /* Hide the tags past the first row and count them on the chip.
     *
     * Runs after every render and from a ResizeObserver, so widening the block
     * gives tags back without anything else having to notice. A zero width
     * means the control is not laid out yet (a deferred dock panel, a hidden
     * tab): leave every tag visible and let the observer's first delivery do
     * the fit, rather than measuring against nothing and hiding all of them.
     */
    const fitTags = () => {
      if (!singleLine || destroyed) return;
      const tags = /** @type {HTMLElement[]} */ (
        Array.from(tagsEl.querySelectorAll('.blockr-select__tag'))
      );
      tags.forEach(t => t.classList.remove('blockr-select__tag--hidden'));
      if (moreEl) moreEl.style.display = 'none';
      if (!tags.length || expanded) return;

      const avail = tagsEl.clientWidth;
      if (!avail) return;

      const gap = parseFloat(getComputedStyle(tagsEl).columnGap) || 3;
      // While the dropdown is open the search input is back in flow and needs
      // its min-width, so the tags get that much less room.
      const reserve = isOpen ? 40 + gap : 0;

      if (!moreEl) {
        moreEl = document.createElement('span');
        moreEl.className = 'blockr-select__more';
        moreEl.setAttribute('aria-hidden', 'true');
      }
      // Measure the chip at its widest possible count: the count can only
      // shrink as tags are dropped, never grow past the total.
      moreEl.textContent = `+${tags.length}`;
      moreEl.style.display = '';
      if (moreEl.parentElement !== tagsEl) tagsEl.insertBefore(moreEl, searchInput);
      const chipWidth = moreEl.getBoundingClientRect().width;

      const widths = tags.map(t => t.getBoundingClientRect().width);
      const shown = fitCount(widths, avail - reserve, gap, chipWidth);

      if (shown >= tags.length) {
        moreEl.style.display = 'none';
        return;
      }
      const hidden = tags.slice(shown);
      hidden.forEach(t => t.classList.add('blockr-select__tag--hidden'));
      moreEl.textContent = `+${hidden.length}`;
      moreEl.title = hidden
        .map(t => t.getAttribute('data-value'))
        .filter(Boolean)
        .join(', ');
    };

    const render = () => {
      if (mode === 'single') renderValue();
      if (mode === 'multi') renderTags();
      if (mode === 'multi') fitTags();
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
      // The search input is back in flow now, so the row is narrower.
      if (mode === 'multi') fitTags();

      if (mode === 'single' && !headless) {
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
      if (mode === 'multi') fitTags();

      if (mode === 'single') {
        valueEl.style.display = '';
        searchInput.style.width = '';
        searchInput.setAttribute('placeholder', '');
      }

      dropdown.innerHTML = '';
      // Last, and after the DOM is settled: a headless caller tears the whole
      // widget down from here.
      if (onClose) onClose();
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

    /* The "+N" chip: show the rest rather than open the dropdown, which in
     * multi mode lists only what is NOT selected. */
    /** @param {MouseEvent} e */
    const onMoreClick = (e) => {
      if (!singleLine) return;
      if (!(/** @type {Element} */ (e.target).closest('.blockr-select__more'))) return;
      e.stopPropagation();
      expanded = true;
      root.classList.add('blockr-select--expanded');
      fitTags();
    };

    const collapse = () => {
      if (!expanded) return;
      expanded = false;
      root.classList.remove('blockr-select--expanded');
      fitTags();
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
      // The anchor is not outside. A click on it is the caller's toggle, and
      // closing here first would have it re-open on the same click.
      if (config.anchor && config.anchor.contains(/** @type {Node | null} */ (e.target))) return;
      collapse();
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
      if (singleLine) control.addEventListener('click', onMoreClick, true);
      if (reorderable && tagsEl) {
        tagsEl.addEventListener('dragstart', onDragStart);
        tagsEl.addEventListener('dragover', onDragOver);
        tagsEl.addEventListener('dragend', onDragEnd);
        tagsEl.addEventListener('drop', onDrop);
      }
    }

    render();

    /* Re-fit on width changes: a dock panel resize, a grid reflow, or the
     * control's first layout after a deferred panel mounts. Observing the
     * control rather than the tags row keeps this out of a feedback loop:
     * hiding a tag changes the row's content, never the control's width. */
    /** @type {ResizeObserver | null} */
    let resizeObs = null;
    if (singleLine && typeof ResizeObserver !== 'undefined') {
      let pending = false;
      resizeObs = new ResizeObserver(() => {
        if (pending) return;
        pending = true;
        requestAnimationFrame(() => { pending = false; fitTags(); });
      });
      resizeObs.observe(control);
    }

    // --- Public API ---

    return {
      el: root,

      // Headless callers own the opening: there is no control to click.
      open() { open(); },

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
          } else if (allowEmpty) {
            // '' survives, and a selection that is no longer among the options
            // CLEARS rather than sliding onto option 0. Omitting `sel` keeps a
            // still-valid selection instead of resetting it.
            selected = (sel == null &&
                        vals.indexOf(/** @type {string} */ (selected)) >= 0)
              ? selected
              : '';
          } else if (options.length > 0) {
            selected = optValue(options[0]);
          } else {
            selected = '';
          }
        } else {
          // A multi-select's selection is an array, but a caller may hand us a
          // scalar (e.g. a block constructed with `names_from = "y"`). Coerce to
          // an array so `.filter` never throws -- an unhandled throw here aborts
          // the rest of the Shiny message batch, which can drop later custom
          // messages (e.g. a chart's `drilldown-data`), leaving those blocks blank.
          const src = sel != null ? sel : selected;
          const arr = Array.isArray(src)
            ? src
            : (src != null && src !== '' ? [src] : []);
          selected = arr.filter(v => vals.indexOf(v) >= 0);
        }
        render();
        if (isOpen) renderDropdown();
      },

      getValue() {
        return mode === 'single' ? (selected || '') : selected.slice();
      },

      // Swap the option list without reconciling the selection against it
      // (setOptions filters selected against the new options, which would drop
      // chips whose value list hasn't arrived yet). Used by lazy value loading.
      //
      // `sel` forces the selection instead of leaving it alone — for a caller
      // that OWNS the value and is only borrowing the widget to display it,
      // e.g. a column picker showing the column a board restored while the
      // frame that has it is still loading (Blockr.reconcileColumn). Nothing
      // else may set a selection the option list does not contain: use
      // setOptions() and let it reconcile.
      /**
       * @param {BlockrSelectOption[] | BlockrSelectOption | null | undefined} opts
       * @param {string | string[] | null} [sel]
       */
      updateOptions(opts, sel) {
        options = Array.isArray(opts) ? opts : (opts != null ? [opts] : []);
        if (sel != null) {
          selected = mode === 'multi'
            ? (Array.isArray(sel) ? sel.slice() : [sel])
            : sel;
        }
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
        if (resizeObs) resizeObs.disconnect();
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
          if (singleLine) control.removeEventListener('click', onMoreClick, true);
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

  /* The dropdown on its own, hung off something the caller owns.
   *
   * For a word in a block's sentence that IS one of its settings: clicking it
   * has to give the list, not a popover holding a control that gives the list.
   * Everything below the surface is the single select -- same options, same
   * tick on the current one, same filter box past the same threshold, same
   * arrows / type-ahead / Enter / Escape, same portal and same flip near the
   * bottom of the window -- because a menu that drifts from the select is a
   * second idiom to maintain. See blockr.docs design-system/pinned-controls.md.
   *
   * Opens immediately and destroys itself when it closes, so the caller keeps
   * a handle only to close it early (a re-render under it, say).
   *
   * @param {HTMLElement} anchor The element to hang under, usually a word.
   * @param {any} config `title`, `labelFirst`, plus the usual select options.
   */
  const createMenu = (anchor, config) => {
    const host = document.createElement('div');
    host.className = 'blockr-select-menu-host';
    document.body.appendChild(host);
    let handle = null;
    let done = false;
    const teardown = () => {
      if (done) return;
      done = true;
      // After the click that closed it has finished: `close()` runs before
      // the option's own onChange, and destroying here synchronously would
      // pull the DOM out from under it.
      setTimeout(() => {
        if (handle) handle.destroy();
        Blockr.removeNode(host);
        if (config.onClose) config.onClose();
      }, 0);
    };
    handle = createSelect(host, Object.assign({}, config, {
      headless: true,
      anchor: anchor,
      allowEmpty: true,
      onClose: teardown
    }), 'single');
    handle.open();
    return { close: teardown, handle: handle };
  };

  Blockr.Select = {
    single: (container, config) => /** @type {BlockrSelectSingleHandle} */ (createSelect(container, config, 'single')),
    multi: (container, config) => /** @type {BlockrSelectMultiHandle} */ (createSelect(container, config, 'multi')),
    menu: createMenu,
    // Exposed for tests: the arithmetic, without a layout engine.
    fitCount: fitCount,
    midTruncate: midTruncate
  };
})();
