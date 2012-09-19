!function ($) {
  $(function(){
    window.prettyPrint && prettyPrint();

    // TODO: enable tooltips
    $('.tooltip-packages').tooltip({
      selector: "a[rel=tooltip]"
    });

    // Disable Enter in the search form (live JS search instead)
    $('.form-search').submit(function(e) {
      e.preventDefault();
    });

  })
}(window.jQuery)
