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

  function toggleHelp() {
    var overlay = document.querySelector('.help-overlay');
    if (!overlay) return;
    overlay.classList.toggle('open');
  }

  function dismissHelp() {
    var overlay = document.querySelector('.help-overlay');
    if (overlay) overlay.classList.remove('open');
  }

  // Lightweight chord buffer: any prefix key sets pending; the next key
  // resolves it. Times out after 1s to avoid leaking state.
  var pending = null;
  var pendingTimer = null;
  function setPending(key) {
    pending = key;
    if (pendingTimer) clearTimeout(pendingTimer);
    pendingTimer = setTimeout(function () { pending = null; }, 1000);
  }
  function clearPending() {
    pending = null;
    if (pendingTimer) { clearTimeout(pendingTimer); pendingTimer = null; }
  }

  document.addEventListener('keydown', function (ev) {
    // '?' is always live (even when input has focus, since '?' is rare there).
    if (ev.key === '?') { ev.preventDefault(); toggleHelp(); return; }

    if (ev.key === 'Escape') {
      dismissHelp();
      if (document.activeElement) document.activeElement.blur();
      return;
    }

    if (ev.key === '/' || ev.key === 's' || (ev.ctrlKey && ev.key === 'k')) {
      focusSearch(ev);
      return;
    }

    if (ev.metaKey || ev.ctrlKey || ev.altKey) return;
    if (document.activeElement && document.activeElement.tagName === 'INPUT') return;

    if (pending === 'g') {
      clearPending();
      ev.preventDefault();
      if (ev.key === 'p') { window.location.href = '/packages'; return; }
      if (ev.key === 'h') { window.location.href = '/'; return; }
      return;
    }

    switch (ev.key) {
      case 'g':                       ev.preventDefault(); setPending('g'); break;
      case 'j': case 'ArrowDown':     ev.preventDefault(); moveCursor(+1); break;
      case 'k': case 'ArrowUp':       ev.preventDefault(); moveCursor(-1); break;
      case 'h': case 'ArrowLeft':     ev.preventDefault(); window.history.back();    break;
      case 'l': case 'ArrowRight':    ev.preventDefault(); window.history.forward(); break;
      case 'Enter':                   ev.preventDefault(); activateCursor(); break;
    }
  });
})();
