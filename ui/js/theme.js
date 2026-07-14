/* Three-state theme switch: auto → light → dark.  Loaded synchronously
 * in <head> so the data-theme attribute lands before first paint (no
 * flash of the wrong theme).  CSP forbids inline scripts, so this tiny
 * file is served from /assets/theme.js instead. */
(function () {
  'use strict';
  var KEY = 'hypha-theme'; /* 'auto' | 'light' | 'dark' */

  function apply(mode) {
    if (mode === 'light' || mode === 'dark') {
      document.documentElement.setAttribute('data-theme', mode);
    } else {
      document.documentElement.removeAttribute('data-theme');
    }
  }

  var stored = null;
  try { stored = localStorage.getItem(KEY); } catch (e) { /* private mode */ }
  apply(stored || 'auto');

  window.hyphaTheme = {
    cycle: function () {
      var order = ['auto', 'light', 'dark'];
      var cur = 'auto';
      try { cur = localStorage.getItem(KEY) || 'auto'; } catch (e) {}
      var next = order[(order.indexOf(cur) + 1) % order.length];
      try { localStorage.setItem(KEY, next); } catch (e) {}
      apply(next);
      return next;
    },
    current: function () {
      try { return localStorage.getItem(KEY) || 'auto'; } catch (e) { return 'auto'; }
    }
  };
})();
