$("#menu-toggle").click(function(e) {
        e.preventDefault();
        $("#wrapper").toggleClass("toggled");
});

////////////////////////////////////////////////////
// tasks: google-country-trends, twitter-locations
////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray( $("#task select option:selected").text(),
      ["google-country-trends", "twitter-locations"] );
    if (optionText >= 0) {
      $("#options p").removeClass("no-input");
      $("#output, #count, #date, #duration, #location, #language, #query, #user").addClass("no-input");
    }
 });
});
//
// $(document).ready(function(){
//   $('#task select').change(function() {
//     var selectedText = $("#task select option:selected").text();
//     if(selectedText.includes("google-country-trends" || "twitter-locations")){
//         $("#options p").removeClass("no-input");
//         $("#output, #count, #date, #duration, #location, #language, #query, #user").addClass("no-input");
//       }
//   });
// });

////////////////////////////////////////////////////
// tasks: google-country-trends, twitter-locations
////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray( $("#task select option:selected").text(),
      ["tumblr-blog-info", "wikipedia-page-links", "wikipedia-search", "wikipedia-text", "web-text"] );
    if (optionText >= 0) {
      $("#query").removeClass("no-input");
      $("#option p, #output, #count, #date, #duration, #location, #language, #user").addClass("no-input");
    }
 });
});


// $(document).ready(function(){
//   $('#task select').change(function() {
//     var selectedText = $("#task select option:selected").text();
//       if (selectedText = "tumblr-blog-info" || "wikipedia-page-links" || "wikipedia-search" || "wikipedia-text" || "web-text") {
//         $("#options p, #output, #count, #date, #duration, #location, #language, #user").addClass("no-input");
//         $("#query").removeClass("no-input");
//         $("#options p, #output, #count, #date, #duration, #location, #language, #user").addClass("no-input");
//       }
//   });
// });
//
//
//
//
