(function () {
  'use strict';

  // The search bar is sticky at the top of every page, so global
  // focus/navigation keybindings are unnecessary. We keep three tiny
  // affordances, all scoped to when the search input has focus:
  //   - Esc blurs it.
  //   - Tab adds a "search scope" chip restricting fuzzy search to the
  //     package whose docs are on screen right now (read from the URL,
  //     no server round-trip) — unless there's no package to scope to,
  //     or a chip is already showing, in which case Tab falls through
  //     to normal focus navigation.
  //   - Backspace, when the query is empty, removes an existing chip.
  // Clicking a chip's × also removes it (separate click listener,
  // since it targets the button, not the search input).
  document.addEventListener('keydown', function (ev) {
    var el = document.activeElement;
    var inSearch = !!(el && el.classList && el.classList.contains('search-input'));
    if (!inSearch) return;

    if (ev.key === 'Escape') {
      el.blur();
      return;
    }

    if (ev.key === 'Tab' && !currentScopeChip()) {
      var pkg = packageFromPath();
      if (pkg) {
        ev.preventDefault();
        addScopeChip(pkg);
      }
      return;
    }

    if (ev.key === 'Backspace' && el.value === '' && currentScopeChip()) {
      removeScopeChip();
    }
  });

  document.addEventListener('click', function (ev) {
    var btn = ev.target.closest && ev.target.closest('.search-scope .remove');
    if (btn) removeScopeChip();
  });

  function packageFromPath() {
    var m = /^\/(?:pkg|source)\/([^/]+)/.exec(window.location.pathname);
    return m ? decodeURIComponent(m[1]) : null;
  }

  function currentScopeChip() {
    return document.querySelector('.search-scope');
  }

  function scopeValueInput() {
    return document.querySelector('.search-scope-value');
  }

  function addScopeChip(pkg) {
    var wrap  = document.querySelector('.search-wrap');
    var input = document.querySelector('.search-input');
    if (!wrap || !input) return;

    var chip = document.createElement('span');
    chip.className = 'search-scope';

    var name = document.createElement('span');
    name.textContent = pkg;

    var remove = document.createElement('button');
    remove.type = 'button';
    remove.className = 'remove';
    remove.setAttribute('aria-label', 'Clear package scope');
    remove.textContent = '×';

    chip.appendChild(name);
    chip.appendChild(remove);
    wrap.insertBefore(chip, input);

    var valueInput = scopeValueInput();
    if (valueInput) valueInput.value = pkg;
    input.focus();
  }

  function removeScopeChip() {
    var chip = currentScopeChip();
    if (chip && chip.parentNode) chip.parentNode.removeChild(chip);

    var valueInput = scopeValueInput();
    if (valueInput) valueInput.value = '';

    var input = document.querySelector('.search-input');
    if (input) input.focus();
  }
})();
