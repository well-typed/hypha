(function () {
  'use strict';

  // The search bar is sticky at the top of every page, so global
  // focus/navigation keybindings are unnecessary. We keep one tiny
  // affordance: Esc blurs the search input when it's focused.
  document.addEventListener('keydown', function (ev) {
    if (ev.key !== 'Escape') return;
    var el = document.activeElement;
    if (el && el.classList && el.classList.contains('search-input')) {
      el.blur();
    }
  });
})();
