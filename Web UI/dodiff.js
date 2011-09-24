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
		var parseTree = new ParseTree(json);
		tree1 = [];
		var iterator = parseTree.iterator();
		var holder = $(Constants.DIV);
		holder.addClass(Constants.HOLDER_CLASS);
		var div = $(Constants.DIV);
		div.addClass(Constants.TITLE_CLASS);
		div.text(json.id);
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
					div.text(json2.id);
					tree2 = [];
					div.append($(Constants.SPAN).append(Statics.getButtons(tree2)));
					holder.append(div);
					var iterator = new ParseTree(json2).iterator();
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
						div.text("Diff: " + json.id + ")---" + json2.id);
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
