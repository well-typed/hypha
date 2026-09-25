(function () {
  'use strict';

  // The search bar is sticky at the top of every page, so global
  // focus/navigation keybindings are unnecessary. What remains:
  //   - Esc in the search box blurs it.
  //   - The scope toggle, a button the server renders on pages inside a
  //     package, restricts search to that package. Clicking it (or
  //     Enter/Space while it has focus, which browsers turn into a
  //     click) flips it. Tab is never intercepted: it moves focus.
  //   - Backspace on an empty query switches an active scope off.
  // Typing a pkg:<name> token into the query scopes from any page; that
  // is parsed server-side and needs nothing here.
  document.addEventListener('keydown', function (ev) {
    var el = document.activeElement;
    var inSearch = !!(el && el.classList && el.classList.contains('search-input'));
    if (!inSearch) return;

    if (ev.key === 'Escape') {
      el.blur();
      return;
    }

    if (ev.key === 'Backspace' && el.value === '' && scopeOn()) {
      setScope(false);
    }
  });

  document.addEventListener('click', function (ev) {
    var btn = ev.target.closest && ev.target.closest('.search-scope');
    if (btn) setScope(!scopeOn());
  });

  function scopeToggle() {
    return document.querySelector('.search-scope');
  }

  function scopeOn() {
    var toggle = scopeToggle();
    return !!toggle && toggle.getAttribute('aria-pressed') === 'true';
  }

  // Mirror the toggle into the hidden input htmx sends as ?pkg=, then
  // tell the search input to re-run its query (its hx-trigger listens
  // for scope-changed) so the results follow the toggle immediately.
  function setScope(on) {
    var toggle = scopeToggle();
    var value  = document.querySelector('.search-scope-value');
    var input  = document.querySelector('.search-input');
    if (!toggle || !value || !input) return;

    toggle.setAttribute('aria-pressed', on ? 'true' : 'false');
    value.value = on ? toggle.getAttribute('data-scope') : '';
    input.focus();
    input.dispatchEvent(new Event('scope-changed'));
  }
})();
