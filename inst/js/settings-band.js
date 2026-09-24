// @ts-check
/**
 * CANONICAL SOURCE: blockr.viz/inst/js/settings-band.js — keep in sync.
 * Copied verbatim into blockr.dplyr per the design-system convention.
 *
 * settings-band.js — shared factory for the design-system boolean control
 * (see blockr.ui/dev/boolean-controls-proposals.html): a .blockr-checkbox
 * built on a native <input type="checkbox">.
 *
 * Staged in blockr.viz for the settings-band pilot; moves to blockr.ui
 * (blockr-core.js) with the shared layer. Loaded before drilldown-config.js /
 * the block scripts, which use Blockr.checkbox when present.
 */
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

  var ns = /** @type {BlockrNamespace} */ (
    (typeof Blockr !== 'undefined') ? Blockr
      : (window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({})));
  ns.checkbox = checkbox;
  ns.gearTray = gearTray;
  ns.segmented = segmented;
})();
