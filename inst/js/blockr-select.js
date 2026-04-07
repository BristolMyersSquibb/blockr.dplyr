/**
 * Blockr.Select — lightweight select component (single + multi)
 *
 * Replaces selectize.js in blockr blocks.
 * Depends on blockr-core.js (Blockr namespace, icons, utilities).
 *
 * API:
 *   Blockr.Select.single(container, config) -> { el, setOptions, getValue, destroy }
 *   Blockr.Select.multi(container, config)  -> { el, setOptions, getValue, destroy }
 */
(() => {
  'use strict';

  const createSelect = (container, config, mode) => {
    const id = Blockr.uid('bsel');
    const dropdownId = `${id}-lb`;

    // State
    let options = config.options || [];
    let selected = mode === 'multi'
      ? (config.selected || []).slice()
      : (config.selected != null ? config.selected : (options.length > 0 ? options[0] : ''));
    const placeholder = config.placeholder || '';
    const reorderable = mode === 'multi' && config.reorderable !== false;
    const onChange = config.onChange || null;
    let isOpen = false;
    let searchQuery = '';
    let highlightIdx = -1;
    let destroyed = false;

    // Drag state (multi only)
    let dragValue = null;
    let dragOverTag = null;
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

    let tagsEl = null;
    let valueEl = null;

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
    }
    control.appendChild(searchInput);

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
    root.appendChild(dropdown);
    container.appendChild(root);

    // --- Rendering ---

    const getFiltered = () => {
      const q = searchQuery.toLowerCase();
      const result = [];
      for (let i = 0; i < options.length; i++) {
        const opt = options[i];
        if (mode === 'multi' && selected.indexOf(opt) >= 0) continue;
        if (q && opt.toLowerCase().indexOf(q) < 0) continue;
        result.push(opt);
      }
      return result;
    };

    const renderDropdown = () => {
      const filtered = getFiltered();
      dropdown.innerHTML = '';

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
        const div = document.createElement('div');
        div.className = 'blockr-select__option';
        if (i === highlightIdx) div.className += ' blockr-select__option--highlighted';
        if (mode === 'single' && opt === selected) div.className += ' blockr-select__option--selected';
        div.setAttribute('role', 'option');
        div.setAttribute('id', `${id}-opt-${i}`);
        div.setAttribute('aria-selected', (mode === 'single' && opt === selected) ? 'true' : 'false');
        div.setAttribute('data-value', opt);
        div.textContent = opt;
        dropdown.appendChild(div);
      }

      if (highlightIdx >= 0) {
        searchInput.setAttribute('aria-activedescendant', `${id}-opt-${highlightIdx}`);
      }
    };

    const renderValue = () => {
      if (mode !== 'single') return;
      if (selected) {
        valueEl.textContent = selected;
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
      tagsEl.innerHTML = '';
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

        tagsEl.appendChild(tag);
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

      const rect = root.getBoundingClientRect();
      const spaceBelow = window.innerHeight - rect.bottom - 8;
      const dropMaxH = 240;
      const openAbove = spaceBelow < dropMaxH && rect.top > dropMaxH;

      root.classList.add('blockr-select--open');
      root.classList.toggle('blockr-select--above', openAbove);
      root.setAttribute('aria-expanded', 'true');

      if (mode === 'single') {
        valueEl.style.display = 'none';
        searchInput.style.width = '';
        searchInput.setAttribute('placeholder', selected || placeholder);
      }

      renderDropdown();
      searchInput.focus();
    };

    const close = () => {
      if (!isOpen) return;
      isOpen = false;
      searchQuery = '';
      searchInput.value = '';

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

    const selectOption = (value) => {
      if (mode === 'single') {
        const changed = selected !== value;
        selected = value;
        close();
        render();
        if (changed && onChange) onChange(selected);
      } else {
        if (selected.indexOf(value) < 0) {
          selected.push(value);
          searchQuery = '';
          searchInput.value = '';
          highlightIdx = 0;
          render();
          renderDropdown();
          if (onChange) onChange(selected.slice());
        }
      }
    };

    const removeTag = (value) => {
      const idx = selected.indexOf(value);
      if (idx >= 0) {
        selected.splice(idx, 1);
        render();
        if (isOpen) renderDropdown();
        if (onChange) onChange(selected.slice());
      }
    };

    // --- Event handlers ---

    const onControlClick = (e) => {
      if (e.target.closest('.blockr-select__tag-remove')) return;
      if (mode === 'single') {
        toggle();
      } else {
        if (!isOpen) open();
        searchInput.focus();
      }
    };

    const onDropdownClick = (e) => {
      const optEl = e.target.closest('.blockr-select__option');
      if (optEl) {
        const val = optEl.getAttribute('data-value');
        if (val != null) selectOption(val);
      }
    };

    const onTagRemoveClick = (e) => {
      const btn = e.target.closest('.blockr-select__tag-remove');
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
    };

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
            selectOption(filtered[highlightIdx]);
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

    const onDocumentClick = (e) => {
      if (!root.contains(e.target)) close();
    };

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

    const onDragStart = (e) => {
      const tag = e.target.closest('.blockr-select__tag');
      if (!tag) return;
      dragValue = tag.getAttribute('data-value');
      e.dataTransfer.effectAllowed = 'move';
      e.dataTransfer.setData('text/plain', dragValue);
      tag.classList.add('blockr-select__tag--dragging');
    };

    const onDragOver = (e) => {
      if (dragValue == null) return;
      e.preventDefault();
      e.dataTransfer.dropEffect = 'move';

      const tag = e.target.closest('.blockr-select__tag');
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

    const onDrop = (e) => {
      e.preventDefault();
      if (dragValue == null || !dragOverTag) { onDragEnd(); return; }

      const targetVal = dragOverTag.getAttribute('data-value');
      const fromIdx = selected.indexOf(dragValue);
      let toIdx = selected.indexOf(targetVal);
      if (fromIdx < 0 || toIdx < 0 || fromIdx === toIdx) { onDragEnd(); return; }

      selected.splice(fromIdx, 1);
      toIdx = selected.indexOf(targetVal);
      const insertIdx = dragSide === 'after' ? toIdx + 1 : toIdx;
      selected.splice(insertIdx, 0, dragValue);

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

      setOptions(opts, sel) {
        options = Array.isArray(opts) ? opts : (opts != null ? [opts] : []);
        if (mode === 'single') {
          if (sel != null && options.indexOf(sel) >= 0) {
            selected = sel;
          } else if (options.length > 0) {
            selected = options[0];
          } else {
            selected = '';
          }
        } else {
          if (sel != null) {
            selected = sel.filter(v => options.indexOf(v) >= 0);
          } else {
            selected = selected.filter(v => options.indexOf(v) >= 0);
          }
        }
        render();
        if (isOpen) renderDropdown();
      },

      getValue() {
        return mode === 'single' ? (selected || '') : selected.slice();
      },

      destroy() {
        if (destroyed) return;
        destroyed = true;
        close();
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
    single: (container, config) => createSelect(container, config, 'single'),
    multi: (container, config) => createSelect(container, config, 'multi')
  };
})();
