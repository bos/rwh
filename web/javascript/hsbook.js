function qid(id) {
  return id.replace(/([.:])/g, "\\$1");
}

function beforeComment(formData, jqForm, options) {
  var form = jqForm[0];
  if (!form.comment.value) {
    $(options.target + " span.comment_error").empty().append(
      "<span class=\"comment_error\">Your comment is empty</span>");
    return false;
  }
  if (!form.name.value) {
    $(options.target + " span.comment_error").empty().append(
      "<span class=\"comment_error\">Please provide a name</span>");
    return false;
  }
  $(options.target + " span.comment_error").empty().after(
    "<img src=\"figs/throbber.gif\" style=\"vertical-align: middle\"/>");
  $(options.target + " input[@name=submit]").attr("disabled", true);
}

function ajaxifyForm(id) {
  var q = qid(id);
  
  $("#form_" + q).ajaxForm({ beforeSubmit: beforeComment,
			     success: function() { ajaxifyForm(id); },
			     target: "#comments_" + q });
}

function toggleComment(id) {
  $("#toggle_" + qid(id)).nextAll().toggle();
  return false;
}

function loadComments(id) {
  $("#comments_" + qid(id)).load(location.protocol + "//" + location.host +
				 "/comments/single/" + id + "/", function() {
    ajaxifyForm(id);
  });
  return false;
}

$(document).ready(function() {
  function loading(id) {
    return " <span id=\"comments_" + id + "\" class=\"comment\">" +
      "<span pid=\"" + id + "\" class=\"commenttoggle\">Loading..." +
      "</span></span>";
  }
  $("div.toc>p").toggle(function() { $(this).next().show("normal"); },
			function() { $(this).next().hide("normal"); });
  $("div.toc>p").hover(function() { $(this).fadeTo("normal", 0.8); },
		       function() { $(this).fadeTo("normal", 0.35); });
  $("p[@id]").each(function() {
    $(this).append(loading($(this).attr("id")));
  });
  $("pre[@id]").each(function() {
    $(this).after(loading($(this).attr("id")));
  });
  var chapid = $("div.preface, div.chapter, div.appendix, div.bibliography").attr("id");
  $("#chapterfeed").attr("href",
			 $("#chapterfeed").attr("href") + chapid + "/");
  $.getJSON(location.protocol + "//" + location.host + "/comments/chapter/" +
	    chapid + "/count/", function(data) {
    $.each(data, function(id, item) {
      var s = item == 1 ? "" : "s";
      $("#comments_" + qid(id) + " span.commenttoggle").replaceWith(
        "<a class='commenttoggle' id='toggle_" + id + "' " +
	"onclick='return loadComments(\"" + id + "\")' " +
	"href='comments: show / hide'>" + item + " comment" + s + "</a>");
    });
    $("span.commenttoggle").each(function() {
      var id = $(this).attr("pid");
      $(this).replaceWith("<a class='commenttoggle' id='toggle_" + id + "' " +
			  "onclick='return loadComments(\"" + id + "\")' " +
			  "href='comment: add'>No comments</a>");
    });
  });
});
