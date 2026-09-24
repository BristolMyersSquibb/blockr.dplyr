// @ts-check
/**
 * Blockr.Select: the dropdown every blockr block uses.
 *
 * Three forms of one widget. `single` shows its pick in a control and closes
 * on a pick; `multi` shows its picks as tags in the control and stays open;
 * `menu` is the list alone, hung under an element the caller owns (a word in
 * a block's sentence), open from the start and gone when it closes. The
 * list, the filter, the keyboard, the placement and the server search are
 * the same in all three; the forms differ only in where the pick is shown
 * and in what a pick does.
 *
 * The public surface is described in types.d.ts (BlockrSelect*). Two other
 * packages read the markup (blockr.dm reads .blockr-select__dropdown,
 * blockr.extra reads .blockr-select__value) and several style it, so the
 * class names are part of that surface.
 *
 * Depends on: blockr-ui.js (Blockr.uid, icons, removeNode, place).
 */
(() => {
  'use strict';

  /** @param {BlockrSelectOption} o @returns {string} */
  const optValue = (o) => (typeof o === 'object' && o !== null ? o.value : o);
  /** @param {BlockrSelectOption} o @returns {string} */
  const optLabel = (o) => (typeof o === 'object' && o !== null ? (o.label || '') : '');
  /** @param {BlockrSelectOption[]} opts @param {string} val */
  const findOpt = (opts, val) => opts.find((o) => optValue(o) === val);

  /** Both facts, and `labelFirst` decides which one leads.
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
    const lead = labelFirst && lbl ? lbl : val;
    const trail = labelFirst && lbl ? val : lbl;
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

  /** Shorten a tag label from the MIDDLE.
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
    return value.slice(0, head) + '…' + value.slice(value.length - tail);
  };

  /** How many tags fit on one row, given their measured widths.
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

  /** @param {any} x @returns {BlockrSelectOption[]} */
  const toOptions = (x) => (Array.isArray(x) ? x : (x != null ? [x] : []));
  /**
   * A multi-select's selection is an array, but a caller may hand over a
   * scalar (a block constructed with `names_from = "y"`). Coerce rather than
   * throw: an unhandled throw here aborts the rest of the Shiny message
   * batch, which can drop later custom messages (a chart's drilldown-data,
   * say) and leave those blocks blank.
   * @param {any} x @returns {string[]}
   */
  const toArray = (x) => (Array.isArray(x) ? x.slice() : (x != null && x !== '' ? [x] : []));

  // Cap how many options get DOM nodes per render. The full list stays
  // searchable; rendering 50K divs froze the tab on every open and keystroke.
  const MAX_ROWS = 200;
  // Past this many options a menu shows its filter box.
  const SEARCH_AFTER = 8;
  const SEARCH_DEBOUNCE = 250;

  /** @param {string} cls @param {string} [text] */
  const div = (cls, text) => {
    const el = document.createElement('div');
    el.className = cls;
    if (text != null) el.textContent = text;
    return el;
  };

  /**
   * @param {HTMLElement} container
   * @param {BlockrSelectConfig} config
   * @param {'single' | 'multi'} mode
   * @param {HTMLElement | null} anchor Set by Blockr.Select.menu(): the list
   *   alone, hung under `anchor`, open from the start. The control is built
   *   but never mounted; the search input moves into the list's head.
   */
  const createSelect = (container, config, mode, anchor) => {
    const multi = mode === 'multi';
    const headless = !!anchor;
    const id = Blockr.uid('bsel');
    const listId = `${id}-lb`;

    // --- Settings: fixed for the life of the widget ------------------------

    // Single only: opt out of the first-option fallback so '' means "nothing
    // selected" and survives setOptions(). Without it any option refresh
    // silently picks option 0, which is fine for a column picker and wrong
    // wherever an unrequested pick would change what the user is looking at.
    const allowEmpty = !multi && config.allowEmpty === true;
    const placeholder = config.placeholder || '';
    const labelFirst = config.labelFirst === true;
    // `search: false` never shows the filter box (a short fixed list such as
    // operators or join types).
    const searchAfter = config.search === false ? Infinity : SEARCH_AFTER;
    const reorderable = multi && config.reorderable !== false;
    // Keep the tags on one row and collapse the overflow into a "+N" chip,
    // instead of wrapping and growing the control a row per tag. The
    // auto-generated parameter bands sit in a grid where the tallest field
    // sets the row height, so a wrapping select there makes the whole band
    // tall.
    const singleLine = multi && config.singleLine === true;
    const maxTagChars = multi && config.maxTagChars && config.maxTagChars > 0
      ? config.maxTagChars : 0;
    const onChange = config.onChange || null;
    const onOpen = config.onOpen || null;
    const onClose = config.onClose || null;
    const onSearch = config.onSearch || null;

    // --- State: everything render() reads ----------------------------------
    //
    // Events change these fields and call render(); nothing else touches the
    // DOM. The keyboard row is `highlight`, an index into the rows the last
    // render showed, clamped there, so it can never point at a row that is
    // not on screen.
    const st = {
      options: toOptions(config.options),
      /** @type {string | string[]} a string in single mode, an array in multi */
      selected: multi ? toArray(config.selected) : '',
      query: '',
      open: false,
      highlight: 0,
      loading: !!config.loading,
      // Single-line only: the chip has been clicked, so the control wraps and
      // shows every tag until a click lands elsewhere. The list offers only
      // UNselected options, so without this a tag past the first row could
      // be neither seen nor removed.
      expanded: false,
      // Server-search mode (high-cardinality columns): the option list is a
      // server-truncated page; typing re-queries through onSearch instead of
      // relying on the client-side filter alone. Set by setSearchInfo().
      serverSearch: false,
      serverTotal: 0,
      destroyed: false
    };
    if (!multi) {
      st.selected = config.selected != null
        ? config.selected
        : (allowEmpty || st.options.length === 0 ? '' : optValue(st.options[0]));
    }
    /** The rows the last render showed; what Enter and the arrows walk. */
    /** @type {BlockrSelectOption[]} */
    let rows = [];

    // --- DOM ---------------------------------------------------------------

    const root = div(`blockr-select blockr-select--${mode}`);
    // A standalone field (in the grid, in the gear tray) is 42px and
    // bordered; inside a row the select stays bare.
    if (config.bordered === true) root.classList.add('blockr-select--bordered');
    if (singleLine) root.classList.add('blockr-select--single-line');
    if (headless) root.classList.add('blockr-select--headless');

    const control = div('blockr-select__control');
    const face = document.createElement('span');
    face.className = 'blockr-select__value';
    const tagsEl = div('blockr-select__tags');
    const chip = document.createElement('span');
    chip.className = 'blockr-select__more';
    chip.setAttribute('aria-hidden', 'true');
    chip.style.display = 'none';

    // The combobox (WAI-ARIA 1.2) is the input: it is what holds focus, what
    // the arrows and Enter arrive at, and what a screen reader announces. The
    // root is a plain wrapper.
    const input = document.createElement('input');
    input.type = 'text';
    input.className = 'blockr-select__search';
    input.setAttribute('role', 'combobox');
    input.setAttribute('aria-expanded', 'false');
    input.setAttribute('aria-haspopup', 'listbox');
    input.setAttribute('aria-controls', listId);
    input.setAttribute('aria-autocomplete', 'list');
    input.setAttribute('autocomplete', 'off');
    input.setAttribute('autocorrect', 'off');
    input.setAttribute('autocapitalize', 'off');
    input.setAttribute('spellcheck', 'false');

    if (multi) {
      tagsEl.appendChild(input);
      control.appendChild(tagsEl);
    } else {
      control.appendChild(face);
      control.appendChild(input);
      const arrow = document.createElement('span');
      arrow.className = 'blockr-select__arrow';
      arrow.innerHTML = Blockr.icons.chevron;
      control.appendChild(arrow);
    }

    const list = div('blockr-select__dropdown');
    list.id = listId;
    list.setAttribute('role', 'listbox');
    // Everything after this marker is rows, and only that part is rebuilt on
    // a render. A menu keeps its title, its tags and its filter box before
    // it, so a keystroke cannot re-create (or move, which blurs) the input
    // the user is typing into.
    const headEnd = document.createComment('rows');
    if (headless) {
      if (config.title) list.appendChild(div('blockr-select__menu-title', config.title));
      // A multi menu carries its picks in the panel's own head, above the
      // filter box, where the control has them relative to the list. The
      // class goes on the tags element, not on an ancestor: the panel is
      // portalled to <body> while open, so a rule hung off the root would
      // stop matching the moment the panel is on screen.
      if (multi) {
        tagsEl.classList.add('blockr-select__tags--menu');
        list.appendChild(tagsEl);
      }
      // Always mounted, because focus is what makes the arrows, Enter and
      // type-ahead work; shown only once the list is long enough to need
      // filtering (render() decides).
      input.classList.add('blockr-select__search--menu');
      input.setAttribute('placeholder', config.searchPlaceholder || 'Filter');
      list.appendChild(input);
    }
    list.appendChild(headEnd);

    if (!headless) root.appendChild(control);
    container.appendChild(root);

    // --- Render ------------------------------------------------------------

    /** The options the list offers for the current query, capped. */
    const filtered = () => {
      const q = st.query.toLowerCase();
      /** @type {BlockrSelectOption[]} */
      const out = [];
      let extra = 0;
      for (const opt of st.options) {
        const val = optValue(opt);
        if (multi && st.selected.indexOf(val) >= 0) continue;
        // `val` may be a number (numeric value pickers send JSON numbers).
        if (q && String(val).toLowerCase().indexOf(q) < 0 &&
            optLabel(opt).toLowerCase().indexOf(q) < 0) continue;
        if (out.length < MAX_ROWS) out.push(opt);
        else extra++;
      }
      return { rows: out, extra };
    };

    const renderFace = () => {
      // A menu has no face on screen, and its filter box keeps the caller's
      // prompt.
      if (headless) return;
      const sel = /** @type {string} */ (st.selected);
      if (sel !== '') {
        const opt = findOpt(st.options, sel);
        if (opt) fillOptContent(face, opt);
        else { face.textContent = String(sel); face.title = String(sel); }
        face.classList.remove('blockr-select__value--placeholder');
        // While open the input replaces the face, so the pick shows there.
        input.setAttribute('placeholder', st.open ? String(sel) : '');
      } else {
        face.textContent = placeholder;
        face.title = '';
        face.classList.add('blockr-select__value--placeholder');
        input.setAttribute('placeholder', placeholder);
      }
      // The face has pointer-events: none, so hover happens on the control.
      control.title = face.title;
    };

    const renderTags = () => {
      tagsEl.querySelectorAll('.blockr-select__tag').forEach((t) => t.remove());
      for (const val of /** @type {string[]} */ (st.selected)) {
        const tag = document.createElement('span');
        tag.className = 'blockr-select__tag';
        tag.setAttribute('data-value', val);
        if (reorderable) tag.setAttribute('draggable', 'true');
        const label = document.createElement('span');
        label.className = 'blockr-select__tag-label';
        const opt = findOpt(st.options, val);
        if (maxTagChars && val.length > maxTagChars) {
          const lbl = opt ? optLabel(opt) : '';
          label.textContent = midTruncate(val, maxTagChars);
          label.title = lbl ? `${val} — ${lbl}` : val;
        } else if (opt) {
          fillOptContent(label, opt);
        } else {
          label.textContent = val;
          label.title = val;
        }
        tag.appendChild(label);
        const remove = document.createElement('button');
        remove.type = 'button';
        remove.className = 'blockr-select__tag-remove';
        remove.setAttribute('aria-label', `Remove ${val}`);
        remove.setAttribute('tabindex', '-1');
        remove.innerHTML = Blockr.icons.remove;
        tag.appendChild(remove);
        // In a menu the input is the panel's filter box, not a child here.
        if (input.parentElement === tagsEl) tagsEl.insertBefore(tag, input);
        else tagsEl.appendChild(tag);
      }
      // The menu's filter box keeps the caller's prompt ("Filter columns");
      // writing the control's placeholder there would wipe it on the first
      // pick.
      if (!headless) {
        input.setAttribute('placeholder', st.selected.length === 0 ? placeholder : '');
      }
    };

    /* Hide the tags past the first row and count them on the chip.
     *
     * A zero width means the control is not laid out yet (a deferred dock
     * panel, a hidden tab): leave every tag visible and let the resize
     * observer's first delivery do the fit, rather than measuring against
     * nothing and hiding all of them.
     */
    const fitTags = () => {
      const tags = /** @type {HTMLElement[]} */ (
        Array.from(tagsEl.querySelectorAll('.blockr-select__tag'))
      );
      tags.forEach((t) => t.classList.remove('blockr-select__tag--hidden'));
      chip.style.display = 'none';
      if (!tags.length || st.expanded) return;
      const avail = tagsEl.clientWidth;
      if (!avail) return;

      const gap = parseFloat(getComputedStyle(tagsEl).columnGap) || 3;
      // While open the input is back in flow and needs its min-width, so
      // the tags get that much less room.
      const reserve = st.open ? 40 + gap : 0;
      // Measure the chip at its widest possible count: the count can only
      // shrink as tags are dropped, never grow past the total.
      chip.textContent = `+${tags.length}`;
      chip.style.display = '';
      if (chip.parentElement !== tagsEl) {
        if (input.parentElement === tagsEl) tagsEl.insertBefore(chip, input);
        else tagsEl.appendChild(chip);
      }
      const shown = fitCount(
        tags.map((t) => t.getBoundingClientRect().width),
        avail - reserve, gap, chip.getBoundingClientRect().width
      );
      if (shown >= tags.length) { chip.style.display = 'none'; return; }
      const hidden = tags.slice(shown);
      hidden.forEach((t) => t.classList.add('blockr-select__tag--hidden'));
      chip.textContent = `+${hidden.length}`;
      chip.title = hidden.map((t) => t.getAttribute('data-value')).join(', ');
    };

    const renderList = () => {
      while (headEnd.nextSibling) list.removeChild(headEnd.nextSibling);
      input.removeAttribute('aria-activedescendant');
      list.style.display = st.open ? 'block' : '';
      if (!st.open) { rows = []; return; }

      const f = filtered();
      rows = f.rows;
      if (st.loading) {
        list.appendChild(div('blockr-select__empty', 'Loading…'));
        return;
      }
      if (!rows.length) {
        list.appendChild(div('blockr-select__empty',
          st.query ? 'No matches' : (multi ? 'All selected' : 'No options')));
        return;
      }
      st.highlight = Math.max(0, Math.min(st.highlight, rows.length - 1));
      rows.forEach((opt, i) => {
        const val = optValue(opt);
        const row = div('blockr-select__option');
        const picked = !multi && val === st.selected;
        if (i === st.highlight) row.classList.add('blockr-select__option--highlighted');
        if (picked) row.classList.add('blockr-select__option--selected');
        row.setAttribute('role', 'option');
        row.id = `${id}-opt-${i}`;
        row.setAttribute('aria-selected', picked ? 'true' : 'false');
        row.setAttribute('data-value', val);
        fillOptContent(row, opt, labelFirst);
        list.appendChild(row);
      });
      if (st.serverSearch) {
        list.appendChild(div('blockr-select__empty',
          `${st.serverTotal.toLocaleString()} values — type to search`));
      } else if (f.extra > 0) {
        list.appendChild(div('blockr-select__empty',
          `+${f.extra.toLocaleString()} more — type to narrow`));
      }
      input.setAttribute('aria-activedescendant', `${id}-opt-${st.highlight}`);
    };

    const render = () => {
      if (st.destroyed) return;
      if (multi) { renderTags(); if (singleLine) fitTags(); }
      else renderFace();
      root.classList.toggle('blockr-select--open', st.open);
      root.classList.toggle('blockr-select--expanded', st.expanded);
      input.setAttribute('aria-expanded', st.open ? 'true' : 'false');
      // A short menu still filters as you type, as a native menu does; only
      // the box is kept off screen (display: none could not hold focus).
      if (headless) {
        input.classList.toggle('blockr-select__search--offscreen',
          st.options.length <= searchAfter);
      }
      renderList();
      if (placed) placed.update();
    };

    // --- Placement and the document click ----------------------------------

    // The list lives on <body> while open so it escapes any clipping or
    // stacking-context ancestor (dock panels, offcanvas, modals; see
    // blockr.design/open/blockr-select-portal). Blockr.place keeps it under
    // the control, or under the caller's anchor when there is no control.
    /** @type {BlockrPlaceHandle | null} */
    let placed = null;

    /** @param {MouseEvent} e */
    const onDocClick = (e) => {
      const t = /** @type {Node | null} */ (e.target);
      if (root.contains(t) || list.contains(t)) return;
      // The anchor is not outside: a click on it is the caller's toggle, and
      // closing here first would have it re-open on the same click.
      if (anchor && anchor.contains(t)) return;
      collapse();
      close();
    };
    // One listener, only while there is something for an outside click to
    // do. Capture phase, because a pick re-renders the list in the bubble
    // phase and detaches the clicked row: by the time a bubble-phase
    // document listener ran, the target would be outside everything.
    let listening = false;
    const syncDocClick = () => {
      const want = st.open || st.expanded;
      if (want === listening) return;
      listening = want;
      if (want) document.addEventListener('click', onDocClick, true);
      else document.removeEventListener('click', onDocClick, true);
    };

    // --- Open, close, pick -------------------------------------------------

    const open = () => {
      if (st.open || st.destroyed) return;
      st.open = true;
      // A single select opens on its pick, so the keyboard row and the pick
      // start as the same row. Typing into a closed select opens it on the
      // typed query instead, from the top.
      st.highlight = 0;
      if (!multi && !st.query && st.selected !== '') {
        st.highlight = Math.max(0, st.options.findIndex((o) => optValue(o) === st.selected));
      }
      if (list.parentElement !== document.body) document.body.appendChild(list);
      render();
      placed = Blockr.place(list, anchor || root, {
        // A word is not a control: the menu sizes to its own content.
        width: headless ? { min: 190, max: 320 } : 'anchor',
        onFlip: (above) => root.classList.toggle('blockr-select--above', above)
      });
      syncDocClick();
      input.focus();
      if (onOpen) onOpen();
    };

    const close = () => {
      if (!st.open) return;
      st.open = false;
      st.query = '';
      input.value = '';
      if (placed) { placed.stop(); placed = null; }
      root.classList.remove('blockr-select--above');
      syncDocClick();
      render();
      // Last, with the DOM settled: a menu tears the whole widget down from
      // here.
      if (onClose) onClose();
    };

    const emit = () => {
      if (onChange) onChange(multi ? /** @type {string[]} */ (st.selected).slice() : st.selected);
    };

    /** @param {string} value */
    const pick = (value) => {
      if (multi) {
        const sel = /** @type {string[]} */ (st.selected);
        if (sel.indexOf(value) >= 0) return;
        sel.push(value);
        st.query = '';
        input.value = '';
        st.highlight = 0;
        render();
        emit();
      } else {
        const changed = st.selected !== value;
        st.selected = value;
        close();
        render();
        if (changed) emit();
      }
    };

    /** @param {string} value */
    const removeTag = (value) => {
      const sel = /** @type {string[]} */ (st.selected);
      const i = sel.indexOf(value);
      if (i < 0) return;
      sel.splice(i, 1);
      render();
      emit();
    };

    const collapse = () => {
      if (!st.expanded) return;
      st.expanded = false;
      syncDocClick();
      render();
    };

    // --- Events ------------------------------------------------------------

    control.addEventListener('click', (e) => {
      const t = /** @type {Element} */ (e.target);
      const remove = t.closest('.blockr-select__tag-remove');
      if (remove) {
        e.stopPropagation();
        const tag = remove.closest('.blockr-select__tag');
        const val = tag && tag.getAttribute('data-value');
        if (val != null) removeTag(val);
        return;
      }
      // The "+N" chip shows the rest rather than opening the list, which in
      // multi mode offers only what is NOT selected.
      if (t.closest('.blockr-select__more')) {
        e.stopPropagation();
        st.expanded = true;
        syncDocClick();
        render();
        return;
      }
      if (multi) { open(); input.focus(); }
      else if (st.open) close();
      else open();
    });

    list.addEventListener('click', (e) => {
      const t = /** @type {Element} */ (e.target);
      const row = t.closest('.blockr-select__option');
      if (row) {
        const val = row.getAttribute('data-value');
        if (val != null) pick(val);
        return;
      }
      // A multi menu's tags live in the list's head, not in the control.
      const remove = t.closest('.blockr-select__tag-remove');
      if (remove) {
        e.stopPropagation();
        const tag = remove.closest('.blockr-select__tag');
        const val = tag && tag.getAttribute('data-value');
        if (val != null) removeTag(val);
      }
    });

    /** @type {ReturnType<typeof setTimeout> | null} */
    let searchTimer = null;
    input.addEventListener('input', () => {
      st.query = input.value;
      st.highlight = 0;
      if (st.open) render();
      else open();
      // Server search: re-query after the user pauses. The client-side filter
      // gives instant feedback on the loaded page; the server response then
      // replaces the option list through updateOptions.
      if (st.serverSearch && onSearch) {
        if (searchTimer) clearTimeout(searchTimer);
        searchTimer = setTimeout(() => onSearch(st.query), SEARCH_DEBOUNCE);
      }
    });

    /** @param {number} step */
    const move = (step) => {
      const n = rows.length || 1;
      st.highlight = (st.highlight + step + n) % n;
      renderList();
      list.querySelector('.blockr-select__option--highlighted')
        ?.scrollIntoView({ block: 'nearest' });
    };

    input.addEventListener('keydown', (e) => {
      switch (e.key) {
        case 'ArrowDown':
        case 'ArrowUp':
          e.preventDefault();
          if (st.open) move(e.key === 'ArrowDown' ? 1 : -1);
          else open();
          break;
        case 'Enter':
          e.preventDefault();
          if (!st.open) open();
          else if (rows[st.highlight]) pick(optValue(rows[st.highlight]));
          break;
        case ' ':
          // Opens a closed select as a native one does; once open, a space
          // is typing.
          if (!st.open) { e.preventDefault(); open(); }
          break;
        case 'Escape':
          e.preventDefault();
          // Focus stays on the input, which is the combobox; a menu returns
          // it to its anchor when it tears down.
          close();
          break;
        case 'Backspace':
          if (multi && input.value === '' && st.selected.length > 0) {
            removeTag(st.selected[st.selected.length - 1]);
          }
          break;
        case 'Tab':
          close();
          break;
      }
    });

    // --- Drag and drop between tags ----------------------------------------
    //
    // Pointer feedback only: the indicator classes go straight on the tags
    // (re-rendering mid-drag would break the drag), and the order changes
    // once, on the drop.
    if (reorderable) {
      /** @type {string | null} */
      let dragValue = null;
      /** @type {Element | null} */
      let overTag = null;
      /** @type {'before' | 'after'} */
      let side = 'after';
      const clearIndicators = () => {
        tagsEl.querySelectorAll('.blockr-select__tag--drop-before, .blockr-select__tag--drop-after')
          .forEach((t) => t.classList.remove('blockr-select__tag--drop-before', 'blockr-select__tag--drop-after'));
      };
      const dragEnd = () => {
        clearIndicators();
        tagsEl.querySelectorAll('.blockr-select__tag--dragging')
          .forEach((t) => t.classList.remove('blockr-select__tag--dragging'));
        dragValue = null;
        overTag = null;
      };
      tagsEl.addEventListener('dragstart', (e) => {
        const tag = /** @type {Element} */ (e.target).closest('.blockr-select__tag');
        if (!tag) return;
        dragValue = tag.getAttribute('data-value');
        const dt = /** @type {DataTransfer} */ (e.dataTransfer);
        dt.effectAllowed = 'move';
        dt.setData('text/plain', /** @type {string} */ (dragValue));
        tag.classList.add('blockr-select__tag--dragging');
      });
      tagsEl.addEventListener('dragover', (e) => {
        if (dragValue == null) return;
        e.preventDefault();
        /** @type {DataTransfer} */ (e.dataTransfer).dropEffect = 'move';
        const tag = /** @type {Element} */ (e.target).closest('.blockr-select__tag');
        if (!tag || tag.getAttribute('data-value') === dragValue) {
          clearIndicators();
          overTag = null;
          return;
        }
        const r = tag.getBoundingClientRect();
        const s = e.clientX < r.left + r.width / 2 ? 'before' : 'after';
        if (tag !== overTag || s !== side) {
          clearIndicators();
          overTag = tag;
          side = s;
          tag.classList.add(`blockr-select__tag--drop-${s}`);
        }
      });
      tagsEl.addEventListener('dragend', dragEnd);
      tagsEl.addEventListener('drop', (e) => {
        e.preventDefault();
        const sel = /** @type {string[]} */ (st.selected);
        const target = overTag && overTag.getAttribute('data-value');
        const from = dragValue == null ? -1 : sel.indexOf(dragValue);
        const moved = /** @type {string} */ (dragValue);
        const to = target == null ? -1 : sel.indexOf(target);
        const drop = side;
        dragEnd();
        if (from < 0 || to < 0 || from === to) return;
        sel.splice(from, 1);
        sel.splice(sel.indexOf(/** @type {string} */ (target)) + (drop === 'after' ? 1 : 0), 0, moved);
        render();
        emit();
      });
    }

    // Re-fit on width changes: a dock panel resize, a grid reflow, or the
    // control's first layout after a deferred panel mounts. Observing the
    // control rather than the tags row keeps this out of a feedback loop:
    // hiding a tag changes the row's content, never the control's width.
    /** @type {ResizeObserver | null} */
    let resizeObs = null;
    if (singleLine && typeof ResizeObserver !== 'undefined') {
      let frame = 0;
      resizeObs = new ResizeObserver(() => {
        if (frame) return;
        frame = requestAnimationFrame(() => { frame = 0; if (!st.destroyed) fitTags(); });
      });
      resizeObs.observe(control);
    }

    // --- Reconciling -------------------------------------------------------

    /**
     * Point the widget at a new list and settle the pick against it. Single
     * falls back to the first option when `sel` is absent or unknown, unless
     * allowEmpty, where '' survives, an unknown pick clears, and omitting
     * `sel` keeps a still-valid pick. Multi keeps only picks the list carries.
     * @param {any} opts @param {any} sel
     */
    const reconcile = (opts, sel) => {
      st.options = toOptions(opts);
      const vals = st.options.map(optValue);
      if (multi) {
        st.selected = toArray(sel != null ? sel : st.selected).filter((v) => vals.indexOf(v) >= 0);
      } else if (sel != null && vals.indexOf(sel) >= 0) {
        st.selected = sel;
      } else if (allowEmpty) {
        st.selected = sel == null && vals.indexOf(/** @type {string} */ (st.selected)) >= 0
          ? st.selected : '';
      } else {
        st.selected = st.options.length ? optValue(st.options[0]) : '';
      }
      render();
    };

    render();
    // A menu is open from the start: there is no control to click.
    if (headless) open();

    return {
      el: root,
      setOptions: reconcile,
      /**
       * As setOptions with the current list; never reports.
       * @param {string | string[] | null} value
       */
      setValue(value) { reconcile(st.options, value); },
      getValue() {
        return multi ? /** @type {string[]} */ (st.selected).slice() : (st.selected || '');
      },
      /**
       * Swap the list without touching the pick (setOptions would drop tags
       * whose value list has not arrived yet). With `sel`, force the pick
       * even if the list lacks it: for a caller that OWNS the value and only
       * borrows the widget to show it, such as a column picker showing the
       * column a board restored while the frame that has it is still loading
       * (Blockr.reconcileColumn). Nothing else may set a pick the list does
       * not contain.
       * @param {any} opts @param {any} [sel]
       */
      updateOptions(opts, sel) {
        st.options = toOptions(opts);
        if (sel != null) st.selected = multi ? toArray(sel) : sel;
        render();
      },
      /** @param {boolean} flag */
      setLoading(flag) { st.loading = !!flag; render(); },
      // Enter or leave server-search mode from a column-values response.
      // `truncated` means the full value list exceeds the server's limit
      // (sticky across queries); `total` is the full distinct count.
      /** @param {{ total?: number, truncated?: boolean } | null | undefined} info */
      setSearchInfo(info) {
        st.serverSearch = !!(info && info.truncated);
        st.serverTotal = (info && info.total) || 0;
        render();
      },
      destroy() {
        if (st.destroyed) return;
        if (searchTimer) clearTimeout(searchTimer);
        if (resizeObs) resizeObs.disconnect();
        close();
        st.destroyed = true;
        st.expanded = false;
        syncDocClick();
        Blockr.removeNode(list);
        Blockr.removeNode(root);
      }
    };
  };

  /** The list on its own, hung off something the caller owns.
   *
   * For a word in a block's sentence that IS one of its settings: clicking it
   * has to give the list, not a popover holding a control that gives the
   * list. Everything below the surface is the select, because a menu that
   * drifts from the select is a second idiom to maintain. See blockr.docs
   * design-system/pinned-controls.md.
   *
   * Opens immediately and destroys itself when it closes, so the caller keeps
   * a handle only to close it early (a re-render under it, say). `mode:
   * 'multi'` keeps the panel open across picks and shows them as tags in its
   * head, so adding three columns is one gesture.
   *
   * @param {HTMLElement} anchor The element to hang under, usually a word.
   * @param {any} config `title`, `labelFirst`, `mode`, `searchPlaceholder`,
   *   plus the usual options.
   */
  const createMenu = (anchor, config) => {
    const host = div('blockr-select-menu-host');
    document.body.appendChild(host);
    /** @type {ReturnType<typeof createSelect> | null} */
    let handle = null;
    let done = false;
    const teardown = () => {
      if (done) return;
      done = true;
      // After the click that closed it has finished: close() runs before the
      // row's own onChange, and destroying here synchronously would pull the
      // DOM out from under it.
      setTimeout(() => {
        if (handle) handle.destroy();
        Blockr.removeNode(host);
        // The filter box had focus and is gone. Unless the closing click put
        // focus somewhere else, hand it back to the word (a no-op on an
        // element that cannot take it).
        const active = document.activeElement;
        if (!active || active === document.body) anchor.focus();
        if (config.onClose) config.onClose();
      }, 0);
    };
    handle = createSelect(host, Object.assign({}, config, {
      allowEmpty: true,
      onClose: teardown
    }), config.mode === 'multi' ? 'multi' : 'single', anchor);
    return { close: teardown };
  };

  Blockr.Select = {
    single: (container, config) => /** @type {BlockrSelectSingleHandle} */ (createSelect(container, config, 'single', null)),
    multi: (container, config) => /** @type {BlockrSelectMultiHandle} */ (createSelect(container, config, 'multi', null)),
    menu: createMenu,
    // Capability flag for the packages that PAINT the words (blockr.viz,
    // blockr.sandbox) against whatever blockr.dplyr a deployment carries.
    // Without it, a caller asking for `mode: 'multi'` on an older build gets
    // a SINGLE menu that looks right and sends one string where the setting
    // holds a list. A missing flag is meant to be thrown on, not felt out.
    menuMulti: true,
    // Exposed for tests: the arithmetic, without a layout engine.
    fitCount,
    midTruncate
  };
})();
