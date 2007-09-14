function beforeComment(formData, jqForm, options) {
  var form = jqForm[0];
  if (!form.comment.value) {
    $("//span.comment_error", jqForm).empty().append(
      "<span class=\"comment_error\">Your comment is empty</span>");
    return false;
  }
  if (!form.name.value) {
    $("//span.comment_error", jqForm).empty().append(
      "<span class=\"comment_error\">Please provide a name</span>");
    return false;
  }
  $("//span.comment_error", jqForm).empty().after("<img src=\"figs/throbber.gif\" style=\"vertical-align: middle\"/>");
  $("//input[@name=submit]", jqForm).attr("disabled", true);
}

function updateComments(responseText, statusText) {
  $(this).children("a.commenttoggle")
    .toggle(function() { $(this).parent().children(".comment").show("normal"); },
	    function() { $(this).parent().children(".comment").hide("normal"); })
    .hover(function() { $(this).fadeTo("normal", 0.8); },
	   function() { $(this).fadeTo("normal", 0.35); });
  $(this).children("form.comment").ajaxForm({
    beforeSubmit: beforeComment, success: updateComments, target: $(this)
  });
}

$(document).ready(function() {
  function loading(id) {
    return " <span class=\"comment\" pid=\"" + id +
      "\"><span class=\"commenttoggle\">Loading...</span></span>";
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
  $("span.comment").each(function() {
    $(this).load(location.protocol + "//" + location.host +
		 "/comments/single/" + $(this).attr("pid") + "/",
		 updateComments);
  });
});
