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

function validateForm(formData, jqForm, options) {
  var form = jqForm[0];
  if (!form.name.value) {
    alert("name!");
    return false;
  }
}

function updateComments(responseText, statusText) {
  $(this).children("span.commenttoggle")
    .toggle(commentOpen, commentClose)
    .hover(paraHoverIn, paraHoverOut);
  $(this).children("form.comment").ajaxForm({
    beforeSubmit: validateForm, success: updateComments, target: $(this)
  });
}

$(document).ready(function() {
  $("p[@id]").append("<span class=\"comment\"><span class=\"commenttoggle\">Loading...</span></span>");
  $("span.comment").each(function() {
    $(this).load("http://localhost:8000/comments/single/" +
		 $(this).parent().attr("id"), updateComments);
  });
});
