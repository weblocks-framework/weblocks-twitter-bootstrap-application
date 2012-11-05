
function showDialog(title, body, cssClass){
  var dialogBody = '<div class="modal-body">' + body + '</div>';
  var titleBar = '<div class="modal-header"><h3>' + title + '</h3><div class="extra-bottom-3"></div></div>';
  var dialogHtml = '<div class="modal fade ' + cssClass + '">' + titleBar + dialogBody + '</div>';
  var dialog = jQuery(dialogHtml);

  //dialog.appendTo('body');
  //jQuery('div.page-wrapper').append(dialog);
  dialog.modal({
    backdrop: true,
    keyboard: false
  });
  // XXX some bug, 2 modal-backdrops showing, don't know how to avoid
  (jQuery('.modal-backdrop:nth(1)').remove());
  jQuery('.modal-backdrop').unbind('click').bind('click', function(){ return false; })
}

function removeDialog(){
  jQuery('.modal').modal('hide').remove();
}
