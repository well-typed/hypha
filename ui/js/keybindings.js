(function () {
  'use strict';

  // The search bar is sticky at the top of every page, so global
  // focus/navigation keybindings are unnecessary. What remains:
  //   - Esc in the search box blurs it.
  //   - The scope toggle restricts search to one package. Only the user
  //     changes it: clicking it (or Enter/Space while it has focus,
  //     which browsers turn into a click) flips it; clearing the query
  //     leaves it alone. Tab is never intercepted: it moves focus.
  //   - A "pkg:<name>" token followed by whitespace is captured as soon
  //     as it is typed or pasted: it leaves the query and becomes the
  //     toggle, switched on. The server still understands a pkg: token
  //     left in the query (one at the very end, a URL, a non-JS client).
  var CAPTURE = /(^|\s)pkg:(\S+)\s/;

  document.addEventListener('keydown', function (ev) {
    var el = document.activeElement;
    var inSearch = !!(el && el.classList && el.classList.contains('search-input'));
    if (inSearch && ev.key === 'Escape') el.blur();
  });

  document.addEventListener('click', function (ev) {
    var btn = ev.target.closest && ev.target.closest('.search-scope');
    if (!btn) return;
    if (scopeOn()) switchOff();
    else setScope(btn.getAttribute('data-scope'));
  });

  document.addEventListener('input', function (ev) {
    var el = ev.target;
    if (!(el.classList && el.classList.contains('search-input'))) return;

    var value = el.value;
    var caret = -1;
    var name  = null;
    var m;
    // A paste can carry several tokens; the last one wins, as it would
    // had they been typed one after the other.
    while ((m = CAPTURE.exec(value)) !== null) {
      caret = m.index + m[1].length;
      name  = m[2];
      value = value.slice(0, caret) + value.slice(m.index + m[0].length);
    }
    if (name === null) return;

    el.value = value;
    el.setSelectionRange(caret, caret);
    setScope(name);
  });

  function scopeToggle() {
    return document.querySelector('.search-scope');
  }

  function scopeOn() {
    var toggle = scopeToggle();
    return !!toggle && toggle.getAttribute('aria-pressed') === 'true';
  }

  // Point the toggle at a package, switched on or off. The label,
  // data-scope and title always describe the package it would scope to.
  function showToggle(toggle, name, on) {
    toggle.setAttribute('data-scope', name);
    toggle.setAttribute('title', 'Search only in ' + name);
    toggle.querySelector('.scope-name').textContent = name;
    toggle.setAttribute('aria-pressed', on ? 'true' : 'false');
    toggle.hidden = false;
  }

  function setScope(name) {
    var toggle = scopeToggle();
    if (!toggle) return;
    showToggle(toggle, name, true);
    scopeChanged(name);
  }

  // Back to what the page offered: its own package, switched off, or
  // nothing at all on a page that is not inside a package.
  function switchOff() {
    var toggle = scopeToggle();
    if (!toggle) return;
    var pageScope = toggle.getAttribute('data-page-scope');
    if (pageScope) {
      showToggle(toggle, pageScope, false);
    } else {
      toggle.setAttribute('aria-pressed', 'false');
      toggle.hidden = true;
    }
    scopeChanged('');
  }

  // Mirror the scope into the hidden input htmx sends as ?pkg=, then
  // tell the search input to re-run its query (its hx-trigger listens
  // for scope-changed) so the results follow immediately.
  function scopeChanged(name) {
    var value = document.querySelector('.search-scope-value');
    var input = document.querySelector('.search-input');
    if (!value || !input) return;
    value.value = name;
    input.focus();
    input.dispatchEvent(new Event('scope-changed'));
  }
})();
