// @ts-check
/**
 * blockr-ui.js — the design system's shared JS layer: the Blockr namespace,
 * DOM helpers, the icon set, and the small controls every block builds on
 * (the Enter button, the required-empty cue, the checkbox, the segmented
 * control, the gear tray). Blockr.Select (blockr-select.js) builds on it.
 *
 * Load it first. It holds nothing block-specific: the block protocol
 * (Blockr.registerBlock and the restore queue) is in blockr-core.js.
 */
window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({});

Blockr.uid = (() => {
  let counter = 0;
  return (prefix = 'b') => `${prefix}-${++counter}`;
})();

Blockr.escapeHtml = (s) => {
  const div = document.createElement('div');
  div.appendChild(document.createTextNode(s));
  return div.innerHTML;
};

Blockr.removeNode = (node) => {
  node?.parentNode?.removeChild(node);
};

/**
 * Natural (nowrap, unconstrained) width of an element's rendered content.
 *
 * Clones the element's innerHTML into a hidden shrink-wrapped measurer in
 * the document, so class-based styles (e.g. .blockr-select__opt-label)
 * still apply. Row factories use this to size a shared left column to the
 * widest committed value instead of a fixed pixel width — the source
 * element itself is usually ellipsized (flex: 1 + overflow: hidden), so
 * its own scrollWidth can't be trusted for shrinking.
 *
 * @param {Element} el
 * @returns {number}
 */
Blockr.contentWidth = (el) => {
  let m = Blockr._measureEl;
  if (!m || !m.isConnected) {
    m = document.createElement('div');
    m.style.cssText =
      'position:absolute;left:-9999px;top:0;visibility:hidden;' +
      'white-space:nowrap;width:auto;pointer-events:none;';
    document.body.appendChild(m);
    Blockr._measureEl = m;
  }
  const cs = window.getComputedStyle(el);
  m.style.fontFamily = cs.fontFamily;
  m.style.fontSize = cs.fontSize;
  m.style.fontWeight = cs.fontWeight;
  m.innerHTML = el.innerHTML;
  const w = Math.ceil(m.getBoundingClientRect().width);
  m.innerHTML = '';
  return w;
};

/**
 * Document-click registry — one document-level listener for all blocks.
 *
 * Per-instance `document.addEventListener('click', ...)` calls leak: the
 * closure retains the block and its detached DOM forever once the block is
 * removed from the board. Entries here are dropped automatically when their
 * anchor element leaves the document, so removed blocks become collectable.
 *
 * Blockr.onDocClick(anchorEl, cb) -> calls cb(event) for every document
 * click while `anchorEl` is connected. The callback does its own
 * containment checks (e.g. close a popover unless the click hit it).
 */
Blockr._docClick = new Set();
document.addEventListener('click', (e) => {
  for (const entry of Blockr._docClick) {
    if (!entry.el.isConnected) {
      Blockr._docClick.delete(entry);
    } else {
      entry.cb(e);
    }
  }
});
Blockr.onDocClick = (el, cb) => {
  Blockr._docClick.add({ el, cb });
};

Blockr.icons = {
  chevron:
    '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" stroke="currentColor" ' +
    'stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round">' +
    '<polyline points="3 4.5 6 7.5 9 4.5"></polyline></svg>',
  // A tag's x: a thin stroke, like every small icon (design system, "Small
  // icons"); at 1.5 it read bold beside the row's remove button.
  remove:
    '<svg width="10" height="10" viewBox="0 0 10 10" fill="none" stroke="currentColor" ' +
    'stroke-width="1" stroke-linecap="round">' +
    '<line x1="2.5" y1="2.5" x2="7.5" y2="7.5"></line>' +
    '<line x1="7.5" y1="2.5" x2="2.5" y2="7.5"></line></svg>',
  x:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 ' +
    '.708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>',
  plus:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 ' +
    '0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>',
  confirm:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 ' +
    '1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>',
  code:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M10.478 1.647a.5.5 0 1 0-.956-.294l-4 13a.5.5 0 0 0 .956.294z' +
    'M4.854 4.146a.5.5 0 0 1 0 .708L1.707 8l3.147 3.146a.5.5 0 0 1-.708.708l-3.5-3.5a.5.5 0 0 1 ' +
    '0-.708l3.5-3.5a.5.5 0 0 1 .708 0m6.292 0a.5.5 0 0 0 0 .708L14.293 8l-3.147 3.146a.5.5 0 0 0 ' +
    '.708.708l3.5-3.5a.5.5 0 0 0 0-.708l-3.5-3.5a.5.5 0 0 0-.708 0"/></svg>',
  gear:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 ' +
    '1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82' +
    '.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 ' +
    '.872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 ' +
    '2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31' +
    '.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-' +
    '.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-' +
    '1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 ' +
    '2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
};

/**
 * Toggle the canonical required-empty amber cue (blockr-blocks.css
 * .blockr-field--required-empty) on a field wrapper or standalone input.
 * One name keeps call sites greppable for the blockr.ui move.
 * @param {Element} el
 * @param {boolean} empty
 */
Blockr.setRequiredEmpty = (el, empty) => {
  el.classList.toggle('blockr-field--required-empty', !!empty);
};

/**
 * Commit-on-Enter text input (design-system §5.5): typing never submits —
 * a chip arms with "Enter ↵" while the value is dirty, the value commits on
 * Enter, blur or the chip (which then fades to the ✓ icon), and Escape
 * reverts to the last committed value.
 *
 * The input must already sit in its parent: the chip is inserted directly
 * after it. Programmatic value changes (setState restores, mode switches)
 * go through the returned `sync(value)`, which resets the committed
 * baseline so a restored value never shows an armed chip.
 *
 * The chip always reads "Enter ↵" — a bare glyph is not self-evident, and
 * one label everywhere beats saving a few pixels in tight rows.
 *
 * @param {HTMLInputElement} input
 * @param {{ onCommit: (value: string) => void }} opts
 * @returns {{ chip: HTMLButtonElement, commit: () => void,
 *             sync: (value: string) => void }}
 */
Blockr.textCommit = (input, opts) => {
  const chip = document.createElement('button');
  chip.type = 'button';
  chip.className = 'blockr-expr-confirm';
  chip.title = 'Apply (Enter)';
  chip.setAttribute('aria-label', 'Apply (Enter)');
  chip.style.display = 'none';
  let committed = input.value;
  let everCommitted = false;
  const armed = 'Enter <span class="blockr-kbd">↵</span>';
  const syncChip = () => {
    if (input.value !== committed) {
      chip.style.display = '';
      chip.classList.remove('confirmed');
      chip.innerHTML = armed;
    } else if (everCommitted) {
      chip.style.display = '';
      chip.classList.add('confirmed');
      chip.innerHTML = Blockr.icons.confirm;
    } else {
      chip.style.display = 'none';
    }
  };
  const commit = () => {
    if (input.value === committed) return;
    committed = input.value;
    everCommitted = true;
    opts.onCommit(input.value);
    syncChip();
  };
  input.addEventListener('input', syncChip);
  input.addEventListener('keydown', (e) => {
    if (e.key === 'Enter') { e.preventDefault(); commit(); }
    else if (e.key === 'Escape') { input.value = committed; syncChip(); }
  });
  input.addEventListener('blur', commit);
  // Keep focus on the input so the chip click doesn't race blur-commit.
  chip.addEventListener('mousedown', (e) => e.preventDefault());
  chip.addEventListener('click', commit);
  /** @type {Element} */ (input.parentElement).insertBefore(chip, input.nextSibling);
  return {
    chip,
    commit,
    sync: (value) => {
      input.value = value;
      committed = value;
      everCommitted = false;
      syncChip();
    }
  };
};

/* --- Controls ----------------------------------------------------------- */

(function () {
  'use strict';

  var CHECK_SVG =
    '<svg width="10" height="10" viewBox="0 0 16 16" fill="currentColor">' +
    '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 ' +
    '0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>';

  /**
   * Build a design-system checkbox.
   * @param {string} label
   * @param {boolean} checked
   * @param {(checked: boolean) => void} onChange
   * @returns {{ el: HTMLLabelElement, input: HTMLInputElement,
   *             set: (v: boolean) => void, get: () => boolean }}
   */
  function checkbox(label, checked, onChange) {
    var wrap = document.createElement('label');
    wrap.className = 'blockr-checkbox';
    var input = document.createElement('input');
    input.type = 'checkbox';
    input.checked = !!checked;
    var box = document.createElement('span');
    box.className = 'blockr-checkbox__box';
    box.innerHTML = CHECK_SVG;
    var txt = document.createElement('span');
    txt.className = 'blockr-checkbox__label';
    txt.textContent = label;
    input.addEventListener('change', function () { onChange(input.checked); });
    wrap.appendChild(input);
    wrap.appendChild(box);
    wrap.appendChild(txt);
    return {
      el: wrap,
      input: input,
      set: function (v) { input.checked = !!v; },
      get: function () { return input.checked; }
    };
  }

  /**
   * The gear tray (design system, "The gear tray"): the gear toggles the band
   * in flow under the header row. It slides open and closed over 0.22s, so
   * the content below is seen moving; Escape inside it closes it and returns
   * focus to the gear. The gear carries the tooltip "Settings", reports its
   * state in aria-expanded and takes the accent tint while open
   * (.blockr-gear-active).
   * @param {HTMLElement} band
   * @param {HTMLButtonElement} gear
   * @param {{ label?: string }} [opts]
   * @returns {BlockrGearTrayHandle}
   */
  function gearTray(band, gear, opts) {
    var open = false;
    /** @type {Animation | null} */
    var anim = null;
    var still = typeof window.matchMedia === 'function' &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    band.setAttribute('role', 'region');
    band.setAttribute('aria-label', (opts && opts.label) || 'Settings');
    gear.title = 'Settings';
    gear.setAttribute('aria-label', 'Settings');
    gear.setAttribute('aria-expanded', 'false');

    /** @param {boolean} next */
    function set(next) {
      if (next === open) return;
      open = next;
      gear.classList.toggle('blockr-gear-active', open);
      gear.setAttribute('aria-expanded', open ? 'true' : 'false');
      if (anim) { anim.cancel(); anim = null; }
      if (open) band.classList.add('blockr-settings--open');
      if (still || typeof band.animate !== 'function') {
        if (!open) band.classList.remove('blockr-settings--open');
        return;
      }
      // Animate from nothing to the band's natural size (or back). The
      // clip keeps the beak, which sits above the band, visible throughout.
      var cs = getComputedStyle(band);
      var full = {
        height: band.offsetHeight + 'px',
        paddingTop: cs.paddingTop, paddingBottom: cs.paddingBottom,
        marginBottom: cs.marginBottom
      };
      var none = { height: '0px', paddingTop: '0px', paddingBottom: '0px',
                   marginBottom: '0px' };
      band.style.clipPath = 'inset(-12px 0 0 0)';
      anim = band.animate(open ? [none, full] : [full, none],
                          { duration: 220, easing: 'ease' });
      anim.onfinish = function () {
        anim = null;
        band.style.clipPath = '';
        if (!open) band.classList.remove('blockr-settings--open');
      };
    }

    gear.addEventListener('click', function () { set(!open); });
    band.addEventListener('keydown', function (e) {
      if (e.key === 'Escape' && open) {
        e.stopPropagation();
        set(false);
        gear.focus();
      }
    });

    return {
      set: set,
      toggle: function () { set(!open); },
      isOpen: function () { return open; }
    };
  }

  /**
   * A segmented control (design system): a fixed choice of two or three short
   * values, all in view, the pick in the accent tint. 42px in the field grid;
   * `size: 'xs'` is the 26px form for a row or a bar.
   * @param {{ value: string, label: string, title?: string }[]} options
   * @param {string} selected
   * @param {(value: string) => void} onChange
   * @param {{ size?: 'xs', label?: string }} [opts]
   * @returns {BlockrSegmentedHandle}
   */
  function segmented(options, selected, onChange, opts) {
    var wrap = document.createElement('div');
    wrap.className = 'blockr-segmented' +
      (opts && opts.size === 'xs' ? ' blockr-segmented--xs' : '');
    wrap.setAttribute('role', 'radiogroup');
    if (opts && opts.label) wrap.setAttribute('aria-label', opts.label);
    var current = selected;
    /** @type {Record<string, HTMLButtonElement>} */
    var segs = {};
    /** @param {string} v */
    function set(v) {
      current = v;
      Object.keys(segs).forEach(function (k) {
        var on = k === v;
        segs[k].classList.toggle('is-selected', on);
        segs[k].setAttribute('aria-checked', on ? 'true' : 'false');
      });
    }
    options.forEach(function (o) {
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'blockr-segmented__seg';
      b.textContent = o.label;
      if (o.title) b.title = o.title;
      b.setAttribute('role', 'radio');
      b.addEventListener('click', function () {
        if (current === o.value) return;
        set(o.value);
        onChange(o.value);
      });
      segs[o.value] = b;
      wrap.appendChild(b);
    });
    set(current);
    return { el: wrap, set: set, get: function () { return current; } };
  }

  Blockr.checkbox = checkbox;
  Blockr.gearTray = gearTray;
  Blockr.segmented = segmented;
})();
