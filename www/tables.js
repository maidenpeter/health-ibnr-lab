// Shows a "scroll sideways" hint on any table that is wider than its card, and only then.
document.addEventListener("DOMContentLoaded", function () {
  function mark() {
    document.querySelectorAll('.dataTables_wrapper').forEach(function (w) {
      var body = w.querySelector('.dataTables_scrollBody');
      if (!body) return;
      if (!w.querySelector(':scope > .scroll-hint')) {
        var hint = document.createElement('div');
        hint.className = 'scroll-hint';
        hint.textContent = 'Scroll sideways to see every column.';
        w.insertBefore(hint, w.firstChild);
      }
      w.classList.toggle('is-scrollable', body.scrollWidth > body.clientWidth + 1);
    });
  }
  var timer;
  function soon() { clearTimeout(timer); timer = setTimeout(mark, 250); }
  $(document).on('draw.dt init.dt shown.bs.tab shiny:value', soon);
  $(window).on('resize', soon);
});
