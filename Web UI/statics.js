/*
	An enum of static functions used commonly by all files.  Put any static/global functions in here.
*/
var Statics = {
	fileUrl: function(file) {
		var div = $(Constants.DIV);
		var a = $(Constants.A_HTML);
		a.attr("href", Constants.DIFF_URL + "&" + Constants.FIRST_FILE + "=" +
			escape(file.path) + "&" + Constants.FIRST_FILE_VERSION + "=" + file.versions[file.versions.length - 1]);
		a.text(file.path);
		var span = $(Constants.SPAN);
		span.text("versions: [" + file.versions + "]");
		span.attr({ "style": "margin-left: 10px" });
		div.append(a);
		div.append(span);
		return div;
	},
	getBreakNode: function() {
		return $(Constants.BR);
	},
	getStatementHolderNode: function(id) {
		var div = $(Constants.SPAN);
		div.addClass(Constants.STATEMENT_HOLDER_CLASS);
		if (id) {
			div.attr(Constants.ID_ATTRIBUTE, Constants.STATEMENT_HOLDER_ID_PREFIX + id);
		}
		return div;
	},
	getOpenFuncNode: function(id) {
		var div = $(Constants.SPAN);
		div.text(Constants.OPEN_PAREN);
		div.addClass(Constants.OPEN_PAREN_CLASS);
		div.attr(Constants.ID_ATTRIBUTE, Constants.OPEN_PAREN_ID_PREFIX + id);
		return div;
	},
	getCloseFuncNode: function(id) {
		var div = $(Constants.SPAN);
		div.text(Constants.CLOSE_PAREN);
		div.addClass(Constants.CLOSE_PAREN_CLASS);
		if (id) {
			div.attr(Constants.ID_ATTRIBUTE, Constants.CLOSE_PAREN_ID_PREFIX + id);
		}
		return div;
	},
	getVariableNode: function(text, id, whitespace, closeParen) {
		var fullId = Constants.VARIABLE_ID_PREFIX + id;
		var span = $(Constants.SPAN);
		span.text(text + (whitespace ? Constants.WHITESPACE : ''));
		span.addClass(Constants.VARIABLE_CLASS);
		span.attr(Constants.ID_ATTRIBUTE, fullId);
		span.mouseover((function() {
			$('[' + Constants.ID_ATTRIBUTE + '="' + fullId + '"]').addClass(Constants.HIGHLIGHTED_CLASS);
		}));
		span.mouseout((function() {
			$('[' + Constants.ID_ATTRIBUTE + '="' + fullId + '"]').removeClass(Constants.HIGHLIGHTED_CLASS);
		}));
		var holder = Statics.getStatementHolderNode();
		holder.append(span);
		for (var i = 0; i < closeParen; i++) {
			holder.append(Statics.getCloseFuncNode());
		}
		return holder;
	},
	getKeywordNode: function(text, id, whitespace, closeParen) {
		var span = Statics.getVariableNode(text, id, whitespace, closeParen);
		span.children().addClass(Constants.KEYWORD_CLASS);
		return span;
	},
	getConstantNode: function(text, id, whitespace, closeParen) {
		var span = Statics.getVariableNode(text, id, whitespace, closeParen);
		span.children().addClass(Constants.CONSTANT_CLASS);
		return span;
	},
	parseObject: function(obj) {
		return !!obj ? new Statement(obj.value, obj.type, obj.id, Statics.parseBody(obj.body)) : null;
	},
	parseBody: function(objs) {
		var ret = [];
		if (!!objs && objs.length) {
			for (var i = 0; i < objs.length; i++) {
				ret[i] = Statics.parseObject(objs[i]);
			}
		}
		return ret;
	}
};
