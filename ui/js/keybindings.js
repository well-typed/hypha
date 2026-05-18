(function () {
  'use strict';

  function focusSearch(ev) {
    var input = document.querySelector('.search-input');
    if (input && document.activeElement !== input) {
      ev.preventDefault();
      input.focus();
      input.select();
    }
  }

  function moveCursor(delta) {
    var items = Array.from(document.querySelectorAll('.results li'));
    if (items.length === 0) return;
    var i = items.findIndex(function (li) { return li.classList.contains('cursor'); });
    var next = Math.max(0, Math.min(items.length - 1, (i < 0 ? 0 : i + delta)));
    items.forEach(function (li) { li.classList.remove('cursor'); });
    items[next].classList.add('cursor');
    items[next].scrollIntoView({ block: 'nearest' });
  }

  function activateCursor() {
    var li = document.querySelector('.results li.cursor');
    if (li) {
      var a = li.querySelector('a');
      if (a) window.location.href = a.getAttribute('href');
    }
  }

  document.addEventListener('keydown', function (ev) {
    if (ev.key === '/' || ev.key === 's' || (ev.ctrlKey && ev.key === 'k')) {
      focusSearch(ev);
    } else if (ev.key === 'Escape') {
      if (document.activeElement) document.activeElement.blur();
    } else if (!ev.metaKey && !ev.ctrlKey && !ev.altKey) {
      if (document.activeElement && document.activeElement.tagName === 'INPUT') return;
      switch (ev.key) {
        case 'j': case 'ArrowDown': ev.preventDefault(); moveCursor(+1); break;
        case 'k': case 'ArrowUp':   ev.preventDefault(); moveCursor(-1); break;
        case 'Enter':               ev.preventDefault(); activateCursor(); break;
      }
    }
  });
})();
