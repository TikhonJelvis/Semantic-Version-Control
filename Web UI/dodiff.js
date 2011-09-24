$.extend({
  getUrlVars: function(){
    var vars = [], hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for(var i = 0; i < hashes.length; i++)
    {
      hash = hashes[i].split('=');
      vars.push(hash[0]);
      vars[hash[0]] = hash[1];
    }
    return vars;
  },
  getUrlVar: function(name){
    return $.getUrlVars()[name];
  }
});
var allVars = $.getUrlVars();
$.getJSON(Constants.FILE_URL + "?" + Constants.PATH + "=" + allVars[Constants.FIRST_FILE] +
	"&" + Constants.VERSION + "=" + allVars[Constants.FIRST_FILE_VERSION], 
	function(json) {
		var parseTree = new ParseTree(json.data);
		var iterator = parseTree.iterator();
		var div = $(Constants.DIV);
		div.addClass(Constants.TITLE_CLASS);
		div.text(json.path + "---" + json.currentVersion);
		Constants.BODY.append(div);
		div = $(Constants.DIV);
		div.addClass(Constants.TITLE_CLASS);
		for (var i = 0; i < json.versions.length; i++) {
			var a = $(Constants.A_HTML);
			var url = window.location.href;
			console.log(url);
			console.log(Constants.FIRST_FILE_VERSION + "=" + allVars[Constants.FIRST_FILE_VERSION]);
			url = url.replace(Constants.FIRST_FILE_VERSION + "=" + allVars[Constants.FIRST_FILE_VERSION],
				Constants.FIRST_FILE_VERSION + "=" + json.versions[i]);
			a.text(json.versions[i]);
			a.attr({"style": "margin-left: 10px", "href" : url });
			div.append(a);
		}
		Constants.BODY.append(div);
		while (iterator.hasNext()) {
			var node = iterator.next().getPrettyNode();
			Constants.BODY.append(node);
		}
	});
$.getJSON(Constants.FILE_URL + "?" + Constants.PATH + "=" + allVars[Constants.SECOND_FILE] +
	"&" + Constants.VERSION + "=" + allVars[Constants.SECOND_FILE_VERSION], 
	function(json) {
		var parseTree = new ParseTree(json.data);
		var iterator = parseTree.iterator();
		var div = $(Constants.DIV);
		div.addClass(Constants.TITLE_CLASS);
		div.text(json.path + "---" + json.currentVersion);
		Constants.BODY.append(div);
		div = $(Constants.DIV);
		div.addClass(Constants.TITLE_CLASS);
		for (var i = 0; i < json.versions.length; i++) {
			var a = $(Constants.A_HTML);
			var url = window.location.href;
			url = url.replace(Constants.SECOND_FILE_VERSION + "=" + allVars[Constants.SECOND_FILE_VERSION],
				Constants.SECOND_FILE_VERSION + "=" + json.versions[i]);
			a.text(json.versions[i]);
			a.attr({"style": "margin-left: 10px", "href" : url });
			div.append(a);
		}
		Constants.BODY.append(div);
		while (iterator.hasNext()) {
			var node = iterator.next().getPrettyNode();
			Constants.BODY.append(node);
		}
	});
