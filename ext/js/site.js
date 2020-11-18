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
    $('.form-search').show();

    $('a,h2,h3,h4').filter(':not([href])[id]').click(function() {
      location.hash = $( this ).attr('id');
    });

  })
}(window.jQuery)
