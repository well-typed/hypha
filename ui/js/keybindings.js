(function () {
  'use strict';

  // The search bar is sticky at the top of every page, so global
  // focus/navigation keybindings are unnecessary. What remains:
  //   - Esc in the search box blurs it.
  //   - The scope toggle restricts search to one package. Only the user
  //     changes it: clicking it (or Enter/Space while it has focus,
  //     which browsers turn into a click) flips it; clearing the query
  //     leaves it alone. Tab is never intercepted: it moves focus.
  //   - A "pkg:<name>" token is captured the moment the whitespace
  //     closing it is typed (or pasted) at the caret: it leaves the
  //     query and becomes the toggle, switched on. Only a token ending
  //     at the caret counts, so typing "pkg:" in front of an existing
  //     word does not swallow that word as a package name. The server
  //     still understands a pkg: token left in the query.
  var CAPTURE = /(^|\s)pkg:(\S+)\s$/;

  document.addEventListener('keydown', function (ev) {
    var el = document.activeElement;
    var inSearch = !!(el && el.classList && el.classList.contains('search-input'));
    if (inSearch && ev.key === 'Escape') el.blur();
  });

  // A pointer click hands focus to the search box, ready for typing. A
  // keyboard activation (detail === 0) leaves focus on the toggle, so
  // its new aria-pressed state is announced and can be flipped back.
  document.addEventListener('click', function (ev) {
    var btn = ev.target.closest && ev.target.closest('.search-scope');
    if (!btn) return;
    var refocus = ev.detail > 0;
    if (scopeOn()) switchOff(refocus);
    else setScope(btn.getAttribute('data-scope'), refocus);
  });

  document.addEventListener('input', function (ev) {
    var el = ev.target;
    if (!(el.classList && el.classList.contains('search-input'))) return;

    var caret  = el.selectionStart;
    var before = el.value.slice(0, caret);
    var m = CAPTURE.exec(before);
    if (m === null) return;

    var start = m.index + m[1].length;
    el.value = before.slice(0, start) + el.value.slice(caret);
    el.setSelectionRange(start, start);
    setScope(m[2], true);
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

  function setScope(name, refocus) {
    var toggle = scopeToggle();
    if (!toggle) return;
    showToggle(toggle, name, true);
    scopeChanged(name, refocus);
  }

  // Back to what the page offered: its own package, switched off, or
  // nothing at all on a page that is not inside a package.
  function switchOff(refocus) {
    var toggle = scopeToggle();
    if (!toggle) return;
    var pageScope = toggle.getAttribute('data-page-scope');
    if (pageScope) {
      showToggle(toggle, pageScope, false);
    } else {
      toggle.setAttribute('aria-pressed', 'false');
      toggle.hidden = true;
      // A hidden button cannot keep focus; the search box takes it.
      refocus = true;
    }
    scopeChanged('', refocus);
  }

  // Mirror the scope into the hidden input htmx sends as ?pkg=, then
  // tell the search input to re-run its query (its hx-trigger listens
  // for scope-changed) so the results follow immediately.
  function scopeChanged(name, refocus) {
    var value = document.querySelector('.search-scope-value');
    var input = document.querySelector('.search-input');
    if (!value || !input) return;
    value.value = name;
    if (refocus) input.focus();
    input.dispatchEvent(new Event('scope-changed'));
  }
})();
