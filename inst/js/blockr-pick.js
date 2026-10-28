// @ts-check
/**
 * Blockr.Pick: a multi-select that shows each pick once.
 *
 * A trial of the design in _scratch/multiselect (variant A3), kept here until
 * it is settled and can move to blockr.ui next to Blockr.Select. It takes the
 * same config and returns the same handle as Blockr.Select.multi() and the
 * multi form of Blockr.Select.menu(), so a caller switches by swapping the
 * name. What differs:
 *
 * - The field shows the picks as text ("Placebo, Xanomeline High Dose", or
 *   "5 of 48" and the names muted), not as tags. The list is the only place
 *   to change them.
 * - The open list has the picks on top, under "Selected", then the other
 *   options in their own order. A tick moves a row up, an untick moves it back
 *   to its place, both animated, so the list looks the same every time it
 *   opens.
 * - The list head has "All" and "None". With a filter typed they act on the
 *   matching rows. In server-search mode (only part of the values loaded)
 *   "All" waits for a search.
 * - Dragging is off unless the caller passes `reorderable: true`; then the
 *   rows under "Selected" drag.
 *
 * The keyboard, the placement, the layers, the server search and the report
 * on close are Blockr.Select's, copied. The markup keeps Blockr.Select's class
 * names (blockr.dm reads .blockr-select__dropdown), with blockr-pick* classes
 * for what is new.
 *
 * Depends on: blockr.ui's blockr-ui.js (Blockr.uid, removeNode, place, layer,
 * tooltip) and Blockr.icons.
 */
(() => {
  'use strict';

  /** @typedef {string | { value: string, label?: string }} PickOption */

  /** @param {PickOption} o @returns {string} */
  const optValue = (o) => (typeof o === 'object' && o !== null ? o.value : o);
  /** @param {PickOption} o @returns {string} */
  const optLabel = (o) => (typeof o === 'object' && o !== null ? (o.label || '') : '');
  /** @param {string[]} a @param {string[]} b */
  const sameList = (a, b) => a.length === b.length && a.every((v, i) => v === b[i]);
  /** @param {any} x @returns {PickOption[]} */
  const toOptions = (x) => (Array.isArray(x) ? x : (x != null ? [x] : []));
  /** @param {any} x @returns {string[]} */
  const toArray = (x) => (Array.isArray(x) ? x.slice() : (x != null && x !== '' ? [x] : []));

  const MAX_ROWS = 200;
  const SEARCH_AFTER = 8;
  const SEARCH_DEBOUNCE = 250;
  const MOVE_MS = 240;

  /** @param {string} cls @param {string} [text] */
  const div = (cls, text) => {
    const el = document.createElement('div');
    el.className = cls;
    if (text != null) el.textContent = text;
    return el;
  };

  /** @param {HTMLElement} el @param {PickOption} o @param {boolean} [labelFirst] */
  const fillOptContent = (el, o, labelFirst) => {
    el.textContent = '';
    const val = optValue(o);
    const lbl = optLabel(o);
    const lead = labelFirst && lbl ? lbl : val;
    const trail = labelFirst && lbl ? val : lbl;
    const name = document.createElement('span');
    name.className = 'blockr-pick__name';
    name.textContent = String(lead);
    el.appendChild(name);
    Blockr.tooltip.set(el, { name: String(val), label: lbl }, { overflow: true });
    if (trail) {
      const span = document.createElement('span');
      span.className = 'blockr-select__opt-label';
      span.textContent = trail;
      el.appendChild(span);
    }
  };

  const reducedMotion = () => typeof window.matchMedia === 'function' &&
    window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  /**
   * The field's text for a set of picks.
   * @param {string[]} sel @param {number} total @param {string} placeholder
   * @returns {{ lead: string, rest: string, empty: boolean }}
   */
  const summary = (sel, total, placeholder) => {
    if (!sel.length) return { lead: placeholder, rest: '', empty: true };
    if (total > 1 && sel.length === total) return { lead: `All ${total}`, rest: '', empty: false };
    if (sel.length <= 2) return { lead: sel.join(', '), rest: '', empty: false };
    return { lead: `${sel.length} of ${total.toLocaleString()}`, rest: sel.join(', '), empty: false };
  };

  /**
   * @param {HTMLElement} container
   * @param {any} config Blockr.Select's multi config.
   * @param {HTMLElement | null} anchor Set by the menu form: the list alone,
   *   hung under `anchor`, open from the start.
   */
  const createPick = (container, config, anchor) => {
    const headless = !!anchor;
    const id = Blockr.uid('bpick');
    const listId = `${id}-lb`;

    const placeholder = config.placeholder || (headless ? '' : 'None');
    const labelFirst = config.labelFirst === true;
    const label = config.label || (headless && config.title) || '';
    const searchAfter = config.search === false ? Infinity : SEARCH_AFTER;
    const reorderable = config.reorderable === true;
    const onChange = config.onChange || null;
    const onOpen = config.onOpen || null;
    const onClose = config.onClose || null;
    const onSearch = config.onSearch || null;

    const st = {
      options: toOptions(config.options),
      selected: toArray(config.selected),
      query: '',
      open: false,
      highlight: 0,
      keyboard: false,
      loading: !!config.loading,
      serverSearch: false,
      serverTotal: 0,
      disabled: config.disabled === true,
      openedWith: /** @type {string[] | null} */ (null),
      destroyed: false
    };
    /** The option rows the last render showed, in screen order. */
    /** @type {PickOption[]} */
    let rows = [];

    // --- DOM ---------------------------------------------------------------

    const root = div('blockr-select blockr-select--multi blockr-pick');
    if (config.bordered === true) root.classList.add('blockr-select--bordered');
    if (headless) root.classList.add('blockr-select--headless');

    // The field: the picks as text and the chevron. It takes focus itself,
    // since the filter box is in the list.
    const control = div('blockr-select__control');
    control.tabIndex = st.disabled ? -1 : 0;
    control.setAttribute('role', 'button');
    control.setAttribute('aria-haspopup', 'listbox');
    control.setAttribute('aria-controls', listId);
    control.setAttribute('aria-expanded', 'false');
    if (label) control.setAttribute('aria-label', label);
    const face = document.createElement('span');
    face.className = 'blockr-select__value blockr-pick__face';
    const faceLead = document.createElement('span');
    faceLead.className = 'blockr-pick__lead';
    const faceRest = document.createElement('span');
    faceRest.className = 'blockr-pick__rest';
    face.append(faceLead, faceRest);
    control.appendChild(face);
    const arrow = document.createElement('span');
    arrow.className = 'blockr-select__arrow';
    arrow.innerHTML = Blockr.icons.chevron;
    control.appendChild(arrow);

    // The combobox is the filter box in the list head, as in Blockr.Select's
    // menu form: what holds focus while open, what the arrows and Enter reach.
    const input = document.createElement('input');
    input.type = 'text';
    input.className = 'blockr-select__search blockr-select__search--menu';
    input.setAttribute('role', 'combobox');
    input.setAttribute('aria-expanded', 'false');
    input.setAttribute('aria-haspopup', 'listbox');
    input.setAttribute('aria-controls', listId);
    input.setAttribute('aria-autocomplete', 'list');
    input.setAttribute('autocomplete', 'off');
    input.setAttribute('autocorrect', 'off');
    input.setAttribute('autocapitalize', 'off');
    input.setAttribute('spellcheck', 'false');
    input.setAttribute('placeholder', config.searchPlaceholder || 'Filter');
    if (label) input.setAttribute('aria-label', label);

    const list = div('blockr-select__dropdown blockr-select__dropdown--multi blockr-select__dropdown--menu blockr-pick__panel');
    const head = div('blockr-select__head');
    if (headless && config.title) head.appendChild(div('blockr-select__menu-title', config.title));
    head.appendChild(input);
    // "3 of 12 selected", then All and None.
    const bulk = div('blockr-pick__bulk');
    const count = document.createElement('span');
    count.className = 'blockr-pick__count';
    const allBtn = document.createElement('button');
    allBtn.type = 'button';
    allBtn.className = 'blockr-pick__bulk-btn';
    const noneBtn = document.createElement('button');
    noneBtn.type = 'button';
    noneBtn.className = 'blockr-pick__bulk-btn';
    const btns = document.createElement('span');
    btns.className = 'blockr-pick__bulk-btns';
    btns.append(allBtn, noneBtn);
    bulk.append(count, btns);
    head.appendChild(bulk);
    list.appendChild(head);
    const listbox = div('blockr-select__listbox');
    listbox.id = listId;
    listbox.setAttribute('role', 'listbox');
    listbox.setAttribute('aria-multiselectable', 'true');
    if (label) listbox.setAttribute('aria-label', label);
    list.appendChild(listbox);

    if (!headless) root.appendChild(control);
    container.appendChild(root);

    // --- Render ------------------------------------------------------------

    /** @param {string} val */
    const findOpt = (val) => st.options.find((o) => optValue(o) === val);
    /** @param {PickOption} o */
    const matches = (o) => {
      const q = st.query.toLowerCase();
      return !q || String(optValue(o)).toLowerCase().indexOf(q) >= 0 ||
        optLabel(o).toLowerCase().indexOf(q) >= 0;
    };
    const total = () => (st.serverSearch && st.serverTotal) || st.options.length;

    /**
     * The two groups the list shows: the picks (in pick order, each from the
     * option list or, when the list has not loaded it, as a bare value) and
     * the rest of the options, in their own order.
     */
    const groups = () => {
      const sel = st.selected;
      const picked = sel.map((v) => findOpt(v) || v).filter(matches);
      /** @type {PickOption[]} */
      const rest = [];
      let extra = 0;
      for (const o of st.options) {
        if (sel.indexOf(optValue(o)) >= 0 || !matches(o)) continue;
        if (rest.length < MAX_ROWS) rest.push(o);
        else extra++;
      }
      return { picked, rest, extra };
    };

    const renderFace = () => {
      if (headless) return;
      const s = summary(st.selected, total(), placeholder);
      faceLead.textContent = s.lead;
      faceRest.textContent = s.rest;
      face.classList.toggle('blockr-select__value--placeholder', s.empty);
      if (st.selected.length > 2) {
        Blockr.tooltip.set(control, st.selected.map((v) => {
          const o = findOpt(v);
          return { name: v, label: o ? optLabel(o) : '' };
        }));
      } else {
        Blockr.tooltip.set(control, st.selected.join(', '), { overflow: true });
      }
    };

    const renderBulk = () => {
      const g = st.open ? groups() : null;
      const shown = g ? g.picked.length + g.rest.length : 0;
      const filtered = !!st.query;
      const n = st.selected.length;
      count.textContent = `${n} of ${total().toLocaleString()} selected`;
      // All waits for a search while only part of the values are loaded.
      const allOff = !g || (st.serverSearch && !filtered) || !g.rest.length;
      allBtn.textContent = filtered ? `All ${shown.toLocaleString()} shown` : 'All';
      allBtn.disabled = allOff;
      if (st.serverSearch && !filtered) {
        Blockr.tooltip.set(allBtn, 'Type to search first: only part of the values are loaded');
      } else Blockr.tooltip.clear(allBtn);
      noneBtn.textContent = filtered ? 'None shown' : 'None';
      noneBtn.disabled = !g || !g.picked.length;
      // A short list does not need the count; the ticks say it.
      bulk.classList.toggle('blockr-pick__bulk--search', st.serverSearch);
    };

    /** @param {PickOption} opt @param {number} i @param {boolean} picked */
    const optionRow = (opt, i, picked) => {
      const val = optValue(opt);
      const row = div('blockr-select__option blockr-pick__option');
      if (st.keyboard && i === st.highlight) row.classList.add('blockr-select__option--highlighted');
      if (picked) row.classList.add('blockr-select__option--selected');
      row.setAttribute('role', 'option');
      row.id = `${id}-opt-${i}`;
      row.setAttribute('aria-selected', picked ? 'true' : 'false');
      row.setAttribute('data-value', val);
      fillOptContent(row, opt, labelFirst);
      const tick = document.createElement('span');
      tick.className = 'blockr-select__tick';
      tick.setAttribute('aria-hidden', 'true');
      if (picked) tick.innerHTML = Blockr.icons.confirm;
      row.insertBefore(tick, row.firstChild);
      if (picked && reorderable && !st.query) {
        row.setAttribute('draggable', 'true');
        const grip = document.createElement('span');
        grip.className = 'blockr-pick__grip';
        grip.setAttribute('aria-hidden', 'true');
        grip.innerHTML = Blockr.icons.grip || '';
        row.appendChild(grip);
      }
      return row;
    };

    /** A row by its value, a group heading by its name. @param {Element} r */
    const slideKey = (r) => r.hasAttribute('data-group')
      ? `group:${r.getAttribute('data-group')}` : /** @type {string} */ (r.getAttribute('data-value'));

    /** Where each row was, so the next render can slide it from there. */
    const positions = () => {
      /** @type {Map<string, number>} */
      const at = new Map();
      listbox.querySelectorAll('[data-value], [data-group]').forEach((r) => {
        at.set(slideKey(r), r.getBoundingClientRect().top);
      });
      return at;
    };

    /** @param {Map<string, number>} before @param {string | null} moved */
    const slide = (before, moved) => {
      if (!before.size || reducedMotion() || typeof Element.prototype.animate !== 'function') return;
      listbox.querySelectorAll('[data-value], [data-group]').forEach((r) => {
        const v = slideKey(r);
        const was = before.get(v);
        if (was == null) return;
        const dy = was - r.getBoundingClientRect().top;
        if (Math.abs(dy) < 1) return;
        /** @type {Keyframe[]} */
        const frames = v === moved
          ? [{ transform: `translateY(${dy}px)`, backgroundColor: 'var(--blockr-color-bg-accent-subtle)' },
             { transform: 'translateY(0)', backgroundColor: 'transparent' }]
          : [{ transform: `translateY(${dy}px)` }, { transform: 'translateY(0)' }];
        r.animate(frames, { duration: MOVE_MS, easing: 'cubic-bezier(.2,.7,.2,1)' });
      });
    };

    const renderList = () => {
      listbox.textContent = '';
      input.removeAttribute('aria-activedescendant');
      list.style.display = st.open ? 'block' : '';
      if (!st.open) { rows = []; return; }
      renderBulk();
      if (st.loading && !st.selected.length) {
        rows = [];
        listbox.appendChild(div('blockr-select__empty', 'Loading…'));
        return;
      }
      const g = groups();
      rows = g.picked.concat(g.rest);
      if (!rows.length) {
        listbox.appendChild(div('blockr-select__empty', st.query ? 'No matches' : 'No options'));
        return;
      }
      st.highlight = Math.max(0, Math.min(st.highlight, rows.length - 1));
      let i = 0;
      if (g.picked.length) {
        const h = div('blockr-pick__group', 'Selected');
        h.setAttribute('role', 'presentation');
        h.setAttribute('data-group', 'selected');
        listbox.appendChild(h);
        for (const o of g.picked) listbox.appendChild(optionRow(o, i++, true));
        if (g.rest.length || st.loading) {
          const h2 = div('blockr-pick__group', 'Other');
          h2.setAttribute('role', 'presentation');
          h2.setAttribute('data-group', 'other');
          listbox.appendChild(h2);
        }
      }
      if (st.loading) listbox.appendChild(div('blockr-select__empty', 'Loading…'));
      else for (const o of g.rest) listbox.appendChild(optionRow(o, i++, false));
      if (st.serverSearch) {
        listbox.appendChild(div('blockr-select__empty',
          `${st.serverTotal.toLocaleString()} values — type to search`));
      } else if (g.extra > 0) {
        listbox.appendChild(div('blockr-select__empty',
          `+${g.extra.toLocaleString()} more — type to narrow`));
      }
      if (st.keyboard) input.setAttribute('aria-activedescendant', `${id}-opt-${st.highlight}`);
    };

    const render = () => {
      if (st.destroyed) return;
      renderFace();
      root.classList.toggle('blockr-select--open', st.open);
      root.classList.toggle('blockr-select--disabled', st.disabled);
      control.tabIndex = st.disabled ? -1 : 0;
      control.setAttribute('aria-expanded', st.open ? 'true' : 'false');
      input.disabled = st.disabled;
      input.setAttribute('aria-expanded', st.open ? 'true' : 'false');
      // A short list still filters as you type; only the box is kept off
      // screen (display: none could not hold focus).
      input.classList.toggle('blockr-select__search--offscreen',
        st.options.length <= searchAfter && !st.serverSearch);
      renderList();
      if (placed) placed.update();
    };

    const showHighlight = () => {
      const r = list.querySelector('.blockr-select__option--highlighted');
      if (r) r.scrollIntoView({ block: 'nearest' });
    };

    // --- Placement and dismissing ------------------------------------------

    /** @type {any} */
    let placed = null;
    /** @type {any} */
    let listLayer = null;

    /** @param {boolean} [byKeyboard] */
    const open = (byKeyboard) => {
      if (st.open || st.destroyed || st.disabled) return;
      st.open = true;
      st.keyboard = !!byKeyboard;
      st.highlight = 0;
      st.openedWith = st.selected.slice();
      if (list.parentElement !== document.body) document.body.appendChild(list);
      render();
      placed = Blockr.place(list, anchor || root, {
        width: headless ? { min: 180, max: 320 } : 'anchor',
        minWidth: headless ? 180 : 240,
        reanchor: typeof config.reanchor === 'function' ? config.reanchor : undefined,
        onFlip: (/** @type {boolean} */ above) => root.classList.toggle('blockr-select--above', above)
      });
      listLayer = Blockr.layer(list, {
        from: anchor || root,
        escape: () => close('escape'),
        outside: () => close()
      });
      list.scrollTop = 0;
      input.focus();
      if (onOpen) onOpen();
    };

    /** @param {'escape' | 'destroy'} [how] */
    const close = (how) => {
      if (!st.open) return;
      st.open = false;
      st.query = '';
      input.value = '';
      st.keyboard = false;
      if (placed) { placed.stop(); placed = null; }
      if (listLayer) { listLayer.remove(); listLayer = null; }
      root.classList.remove('blockr-select--above');
      let changed = false;
      if (st.openedWith) {
        const was = st.openedWith;
        st.openedWith = null;
        if (how !== 'destroy') changed = !sameList(st.selected, was);
      }
      render();
      if (!headless && how === 'escape') control.focus();
      if (changed) emit();
      if (onClose) onClose();
    };

    const emit = () => {
      if (onChange) onChange(st.selected.slice());
    };

    /** Change the picks with the list open, sliding the rows that move.
     *  @param {string[]} next @param {string | null} moved */
    const repick = (next, moved) => {
      const before = positions();
      st.selected = next;
      render();
      slide(before, moved);
    };

    /** @param {string} value */
    const pick = (value) => {
      const sel = st.selected.slice();
      const i = sel.indexOf(value);
      if (i >= 0) sel.splice(i, 1);
      else sel.push(value);
      // The keyboard row follows the row it was on.
      repick(sel, value);
      if (st.keyboard) {
        const at = rows.findIndex((o) => optValue(o) === value);
        if (at >= 0) { st.highlight = at; renderList(); showHighlight(); }
      }
    };

    const pickAll = () => {
      const g = groups();
      repick(st.selected.concat(g.rest.map(optValue)), null);
    };
    const pickNone = () => {
      const drop = groups().picked.map(optValue);
      repick(st.selected.filter((v) => drop.indexOf(v) < 0), null);
    };

    // --- Events ------------------------------------------------------------

    control.addEventListener('click', () => {
      if (st.disabled) return;
      if (st.open) close();
      else open();
    });
    control.addEventListener('keydown', (e) => {
      if (st.disabled || st.open) return;
      if (e.key === 'Enter' || e.key === ' ' || e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault();
        open(true);
      } else if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        // Typing on the closed field starts the filter.
        open(true);
        input.value = e.key;
        st.query = e.key;
        e.preventDefault();
        render();
      }
    });

    list.addEventListener('mousedown', (e) => {
      // Keep the focus in the filter box while clicking rows.
      if (e.target !== input) e.preventDefault();
    });
    list.addEventListener('click', (e) => {
      const t = /** @type {Element} */ (e.target);
      if (t.closest('.blockr-pick__grip')) return;
      const row = t.closest('.blockr-select__option');
      if (row) {
        const val = row.getAttribute('data-value');
        if (val != null) pick(val);
      }
    });
    allBtn.addEventListener('click', (e) => { e.stopPropagation(); pickAll(); input.focus(); });
    noneBtn.addEventListener('click', (e) => { e.stopPropagation(); pickNone(); input.focus(); });

    /** @type {ReturnType<typeof setTimeout> | null} */
    let searchTimer = null;
    input.addEventListener('input', () => {
      st.query = input.value;
      st.highlight = 0;
      st.keyboard = true;
      if (st.open) render();
      else open(true);
      if (st.serverSearch && onSearch) {
        if (searchTimer) clearTimeout(searchTimer);
        searchTimer = setTimeout(() => onSearch(st.query), SEARCH_DEBOUNCE);
      }
    });

    /** @param {number} step */
    const move = (step) => {
      const n = rows.length || 1;
      if (!st.keyboard) st.keyboard = true;
      else st.highlight = (st.highlight + step + n) % n;
      renderList();
      showHighlight();
    };

    input.addEventListener('keydown', (e) => {
      switch (e.key) {
        case 'ArrowDown':
        case 'ArrowUp':
          e.preventDefault();
          if (st.open) move(e.key === 'ArrowDown' ? 1 : -1);
          else open(true);
          break;
        case 'Enter':
          e.preventDefault();
          if (!st.open) open(true);
          else if (!st.keyboard) { st.keyboard = true; renderList(); showHighlight(); }
          else if (rows[st.highlight]) pick(optValue(rows[st.highlight]));
          break;
        case 'Tab':
          close();
          break;
      }
    });

    // --- Drag the picks into order (reorderable only) ----------------------
    //
    // Pointer feedback goes straight on the rows; the order changes once, on
    // the drop, and is reported with the rest when the list closes.
    if (reorderable) {
      /** @type {string | null} */
      let dragValue = null;
      /** @type {Element | null} */
      let overRow = null;
      /** @type {'before' | 'after'} */
      let side = 'after';
      const clear = () => {
        listbox.querySelectorAll('.blockr-pick__option--drop-before, .blockr-pick__option--drop-after')
          .forEach((r) => r.classList.remove('blockr-pick__option--drop-before', 'blockr-pick__option--drop-after'));
      };
      const dragEnd = () => {
        clear();
        listbox.querySelectorAll('.blockr-pick__option--dragging')
          .forEach((r) => r.classList.remove('blockr-pick__option--dragging'));
        dragValue = null;
        overRow = null;
      };
      listbox.addEventListener('dragstart', (e) => {
        const row = /** @type {Element} */ (e.target).closest('.blockr-select__option[draggable="true"]');
        if (!row) return;
        dragValue = row.getAttribute('data-value');
        const dt = /** @type {DataTransfer} */ (e.dataTransfer);
        dt.effectAllowed = 'move';
        dt.setData('text/plain', /** @type {string} */ (dragValue));
        row.classList.add('blockr-pick__option--dragging');
      });
      listbox.addEventListener('dragover', (e) => {
        if (dragValue == null) return;
        const row = /** @type {Element} */ (e.target).closest('.blockr-select__option[draggable="true"]');
        if (!row) return;
        e.preventDefault();
        /** @type {DataTransfer} */ (e.dataTransfer).dropEffect = 'move';
        if (row.getAttribute('data-value') === dragValue) { clear(); overRow = null; return; }
        const r = row.getBoundingClientRect();
        const s = e.clientY < r.top + r.height / 2 ? 'before' : 'after';
        if (row !== overRow || s !== side) {
          clear();
          overRow = row;
          side = s;
          row.classList.add(`blockr-pick__option--drop-${s}`);
        }
      });
      listbox.addEventListener('dragend', dragEnd);
      listbox.addEventListener('drop', (e) => {
        e.preventDefault();
        const target = overRow && overRow.getAttribute('data-value');
        const moved = dragValue;
        const drop = side;
        dragEnd();
        if (moved == null || target == null || moved === target) return;
        const sel = st.selected.filter((v) => v !== moved);
        sel.splice(sel.indexOf(target) + (drop === 'after' ? 1 : 0), 0, moved);
        repick(sel, null);
      });
    }

    // --- Reconciling -------------------------------------------------------

    /** @param {any} opts @param {any} sel */
    const reconcile = (opts, sel) => {
      st.options = toOptions(opts);
      const vals = st.options.map(optValue);
      st.selected = toArray(sel != null ? sel : st.selected).filter((v) => vals.indexOf(v) >= 0);
      render();
    };

    render();
    if (headless) open();

    return {
      el: root,
      setOptions: reconcile,
      /** @param {string[] | null} value */
      setValue(value) { reconcile(st.options, value); },
      getValue() { return st.selected.slice(); },
      /** @param {any} opts @param {any} [sel] */
      updateOptions(opts, sel) {
        st.options = toOptions(opts);
        if (sel != null) st.selected = toArray(sel);
        render();
      },
      /** @param {boolean} flag */
      setLoading(flag) { st.loading = !!flag; render(); },
      /** @param {boolean} flag */
      setDisabled(flag) {
        st.disabled = !!flag;
        if (st.disabled) close();
        render();
      },
      /** @param {{ total?: number, truncated?: boolean } | null | undefined} info */
      setSearchInfo(info) {
        st.serverSearch = !!(info && info.truncated);
        st.serverTotal = (info && info.total) || 0;
        render();
      },
      destroy() {
        if (st.destroyed) return;
        if (searchTimer) clearTimeout(searchTimer);
        close('destroy');
        st.destroyed = true;
        Blockr.removeNode(list);
        Blockr.removeNode(root);
      }
    };
  };

  /** The list on its own, hung under an element the caller owns (a word).
   *  Same contract as Blockr.Select.menu() with `mode: 'multi'`.
   *  @param {HTMLElement} anchor @param {any} config */
  const createMenu = (anchor, config) => {
    const host = div('blockr-select-menu-host');
    document.body.appendChild(host);
    /** @type {ReturnType<typeof createPick> | null} */
    let handle = null;
    let done = false;
    const teardown = () => {
      if (done) return;
      done = true;
      setTimeout(() => {
        if (handle) handle.destroy();
        Blockr.removeNode(host);
        const active = document.activeElement;
        if (!active || active === document.body) anchor.focus();
        if (config.onClose) config.onClose();
      }, 0);
    };
    handle = createPick(host, Object.assign({}, config, { onClose: teardown }), anchor);
    return { close: teardown };
  };

  /** @type {any} */ (window).Blockr = window.Blockr || {};
  Blockr.Pick = {
    multi: (/** @type {HTMLElement} */ container, /** @type {any} */ config) => createPick(container, config, null),
    menu: createMenu,
    summary
  };
})();
