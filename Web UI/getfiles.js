$.getJSON(Constants.FILE_LIST_URL, function(json) {
	$("#loading").hide();
	for (var i = 0; i < json.length; i++) {
		Constants.BODY.append(Statics.fileUrl(json[i]));
	}
});
