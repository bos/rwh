function paraHoverIn() {
  $(this).fadeTo("normal", 0.8);
}

function paraHoverOut() {
  $(this).fadeTo("normal", 0.35);
}

function commentOpen() {
  $(this).parent().children(".comment").show("normal");
}

function commentClose() {
  $(this).parent().children(".comment").hide("normal");
}

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
    .toggle(commentOpen, commentClose)
    .hover(paraHoverIn, paraHoverOut);
  $(this).children("form.comment").ajaxForm({
    beforeSubmit: beforeComment, success: updateComments, target: $(this)
  });
}

$(document).ready(function() {
  function loading(id) {
    return " <span class=\"comment\" pid=\"" + id +
      "\"><span class=\"commenttoggle\">Loading...</span></span>";
  }
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
