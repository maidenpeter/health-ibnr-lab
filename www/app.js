// Small behaviours for the lab's pages.
document.addEventListener("DOMContentLoaded", function () {
  // Tables: show a "scroll sideways" hint on any table that is wider than its card, and only then.
  function markScrollable() {
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
  function soon() { clearTimeout(timer); timer = setTimeout(markScrollable, 250); }
  $(document).on('draw.dt init.dt shown.bs.tab shiny:value', soon);
  $(window).on('resize', soon);

  // Navbar: on a phone or tablet, close the menu once a page is chosen and scroll to the top of that page.
  $(document).on('click', '.navbar-collapse.show .nav-link', function () {
    var menu = document.querySelector('.navbar-collapse.show');
    if (menu && window.bootstrap) window.bootstrap.Collapse.getOrCreateInstance(menu, { toggle: false }).hide();
    window.scrollTo(0, 0);
  });
});
