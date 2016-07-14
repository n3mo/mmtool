$("#menu-toggle").click(function(e) {
        e.preventDefault();
        $("#wrapper").toggleClass("toggled");
});

////////////////////////////////////////
// removes select choice on reload
////////////////////////////////////////
$(window).unload(function() {
  $("#output, #count, #date, #duration, #location, #language, #query, #user").addClass("no-input");
  $('select option').each(function() {
    if ( $(this).val() > 0 ) {
        $(this).remove();
    }
  });
});

//////////////////////////////////////////
// Sets dynamic input for first page load
//////////////////////////////////////////
$(document).ready(function() {
    if ($("#task select option:selected").val() == 0) {
      $("#output, #count, #date, #duration, #location, #language, #query, #user").addClass("no-input");
    }
});

////////////////////////////////////////////////////
// tasks: google-country-trends, twitter-locations
////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["google-country-trends", "twitter-locations"] );
    if (optionText >= 0) {
      $("#output").removeClass("no-input");
      $("#count, #date, #duration, #location, #language, #query, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: tumblr-blog-info, wikipedia-page-links, wikipedia-search, web-text
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["tumblr-blog-info", "wikipedia-page-links", "wikipedia-search", "web-text"] );
    if (optionText >= 0) {
      $("#query, #output").removeClass("no-input");
      $("#count, #date, #duration, #location, #language, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: tumblr-posts, tumblr-tag
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["tumblr-posts", "tumblr-tag"] );
    if (optionText >= 0) {
      $("#query, #count, #output").removeClass("no-input");
      $("#date, #duration, #location, #language, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: twitter-followers, twitter-friends
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["twitter-followers", "twitter-friends"] );
    if (optionText >= 0) {
      $("#user, #output").removeClass("no-input");
      $("#count, #date, #duration, #location, #language, #query").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: twitter-trends, twitter-trends-nohash
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["twitter-trends", "twitter-trends-nohash"] );
    if (optionText >= 0) {
      $("#location, #output").removeClass("no-input");
      $("#count, #date, #duration, #language, #query, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: wikipedia-views
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["wikipedia-views"] );
    if (optionText >= 0) {
      $("#query, #date, #language, #output").removeClass("no-input");
      $("#count, #duration, #location, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: wikipedia-text
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["wikipedia-text"] );
    if (optionText >= 0) {
      $("#query, #language, #output").removeClass("no-input");
      $("#count, #date, #duration, #location, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: google-trends
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["google-trends"] );
    if (optionText >= 0) {
      $("#date, #output").removeClass("no-input");
      $("#count, #duration, #location, #language, #query, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: twitter-sample
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["twitter-sample"] );
    if (optionText >= 0) {
      $("#count, #duration, #output").removeClass("no-input");
      $("#date, #location, #language, #query, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: twitter-search
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["twitter-search"] );
    if (optionText >= 0) {
      $("#query, #count, #location, #language, #output").removeClass("no-input");
      $("#date, #duration, #user").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: twitter-stream
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["twitter-stream"] );
    if (optionText >= 0) {
      $("#query, #user, #location, #language, #duration, #count, #output").removeClass("no-input");
      $("#date").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// tasks: twitter-user
//////////////////////////////////////////////////////////////////////////////////////////////
$(document).ready(function(){
 $('#task select').change(function() {
    var optionText = jQuery.inArray($("#task select option:selected").text(),
      ["twitter-user"] );
    if (optionText >= 0) {
      $("#user, #count, #output").removeClass("no-input");
      $("#date, #duration, #location, #language, #query").addClass("no-input");
    }
 });
});

///////////////////////////////////////////////////////////////////////////////////////////////
// Results folding
//////////////////////////////////////////////////////////////////////////////////////////////

$('.fold_massmine').click(function() {
    $('#massmine-result').toggle(500)
})


$('.fold_hashtags').click(function() {
    $('#hashtags-result').toggle(500)
})

$('.fold_user_mentions').click(function() {
    $('#user-mentions-result').toggle(500)
})

