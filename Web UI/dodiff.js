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
$.getJSON(Constants.FILE_URL + allVars[Constants.FIRST_FILE], 
	function(json) {
		var parseTree = new ParseTree(json.data);
		tree1 = [];
		var iterator = parseTree.iterator();
		var holder = $(Constants.DIV);
		holder.addClass(Constants.HOLDER_CLASS);
		var div = $(Constants.DIV);
		div.addClass(Constants.TITLE_CLASS);
		div.text(json.path + "(" + json.currentVersion + ")");
		for (var i = 0; i < json.versions.length; i++) {
			var a = $(Constants.A_HTML);
			var url = window.location.href;
			url = url.replace(Constants.FIRST_FILE_VERSION + "=" + allVars[Constants.FIRST_FILE_VERSION],
				Constants.FIRST_FILE_VERSION + "=" + json.versions[i]);
			a.text(json.versions[i]);
			a.attr({"style": "margin-left: 10px", "href" : url });
			div.append($(Constants.SPAN).append(a));
		}
		div.append($(Constants.SPAN).append(Statics.getButtons(tree1)));
		holder.append(div);
		while (iterator.hasNext()) {
			var node = iterator.next();
			holder.append(node.getPrettyNode());
			tree1.push(node);
		}
		Constants.BODY.append(holder);
		if (!!allVars[Constants.SECOND_FILE]) {
			$.getJSON(Constants.FILE_URL + allVars[Constants.SECOND_FILE], 
				function(json2) {	
					var holder = $(Constants.DIV);
					holder.addClass(Constants.HOLDER_CLASS);
					var div = $(Constants.DIV);
					div.addClass(Constants.TITLE_CLASS);
					div.text(json2.path + "(" + json2.currentVersion + ")");
					for (var i = 0; i < json2.versions.length; i++) {
						var a = $(Constants.A_HTML);
						var url = window.location.href;
						url = url.replace(Constants.SECOND_FILE_VERSION + "=" + allVars[Constants.SECOND_FILE_VERSION],
							Constants.SECOND_FILE_VERSION + "=" + json2.versions[i]);
						a.text(json2.versions[i]);
						a.attr({"style": "margin-left: 10px", "href" : url });
						div.append($(Constants.SPAN).append(a));
					}
					tree2 = [];
					div.append($(Constants.SPAN).append(Statics.getButtons(tree2)));
					holder.append(div);
					var iterator = new ParseTree(json2.data).iterator();
					while (iterator.hasNext()) {
						var node = iterator.next();
						holder.append(node.getPrettyNode());
						tree2.push(node);
					}
					Constants.BODY.append(Statics.getBreakNode());
					Constants.BODY.append(Statics.getBreakNode());
					Constants.BODY.append(Statics.getBreakNode());
					Constants.BODY.append(holder);
					$.getJSON(Constants.FILE_DIFF_URL + allVars[Constants.FIRST_FILE], 
					function(js) {
						var holder = $(Constants.DIV);
						holder.addClass(Constants.HOLDER_CLASS);
						var div = $(Constants.DIV);
						div.addClass(Constants.TITLE_CLASS);
						div.text("Diff: " + json.path + "(" + json.currentVersion + ")---" + json2.path + "(" + json2.currentVersion + ")");
						holder.append(div);
						var tree = new diffTree(js);
						var iterator = tree.parseFrom(parseTree);
						while (iterator.hasNext()) {
							var node = iterator.next().getPrettyNode();
							holder.append(node);
						}
						Constants.BODY.append(Statics.getBreakNode());
						Constants.BODY.append(Statics.getBreakNode());
						Constants.BODY.append(Statics.getBreakNode());
						Constants.BODY.append(holder);
					});
				}
		);
		}
	}
);
