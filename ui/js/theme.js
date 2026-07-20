/* Two-state theme switch: light / dark.  Loaded synchronously in
 * <head> so the data-theme attribute lands before first paint (no
 * flash of the wrong theme).  CSP forbids inline scripts, so this tiny
 * file is served from /assets/theme.js instead.
 *
 * No stored preference means "follow the OS": base.css already applies
 * the dark palette via @media (prefers-color-scheme: dark) whenever no
 * data-theme attribute is set.  Clicking the toggle stores an explicit
 * choice from then on, overriding the OS going forward. */
(function () {
  'use strict';
  var KEY = 'hypha-theme'; /* 'light' | 'dark' | absent (follow OS) */

  function storedMode() {
    try { return localStorage.getItem(KEY); } catch (e) { return null; }
  }

  function systemPrefersDark() {
    return !!(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);
  }

  function effective() {
    var s = storedMode();
    if (s === 'light' || s === 'dark') return s;
    return systemPrefersDark() ? 'dark' : 'light';
  }

  function apply(mode) {
    if (mode === 'light' || mode === 'dark') {
      document.documentElement.setAttribute('data-theme', mode);
    } else {
      document.documentElement.removeAttribute('data-theme');
    }
  }

  /* Pre-paint: apply whatever is explicitly stored, or clear the
   * attribute entirely so the prefers-color-scheme media query in
   * base.css decides.  A stale 'auto' value from a previous version of
   * this file also falls into this branch — no migration needed. */
  apply(storedMode());

  window.hyphaTheme = {
    toggle: function () {
      var next = effective() === 'dark' ? 'light' : 'dark';
      try { localStorage.setItem(KEY, next); } catch (e) { /* private mode */ }
      apply(next);
      return next;
    },
    effective: effective
  };

  document.addEventListener('DOMContentLoaded', function () {
    var btn = document.getElementById('theme-toggle');
    if (!btn) return;
    var label = function (mode) { return mode === 'dark' ? 'Dark' : 'Light'; };
    btn.textContent = label(effective());
    btn.addEventListener('click', function () {
      btn.textContent = label(window.hyphaTheme.toggle());
    });
  });
})();
