/*
	An enum of static functions used commonly by all files.  Put any static/global functions in here.
*/
var Statics = {
	getStatementHolderNode: function(id) {
		var div = $(Constants.DIV);
		div.addClass(Constants.STATEMENT_HOLDER_CLASS);
		div.attr(Constants.ID_ATTRIBUTE, Constants.STATEMENT_HOLDER_ID_PREFIX + id);
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
		div.attr(Constants.ID_ATTRIBUTE, Constants.CLOSE_PAREN_ID_PREFIX + id);
		return div;
	},
	getVariableNode: function(text, id, whitespace) {
		var fullId = Constants.VARIABLE_ID_PREFIX + id;
		var span = $(Constants.SPAN);
		span.text(text + (whitespace ? Constants.WHITESPACE : ''));
		span.addClass(Constants.VARIABLE_CLASS);
		span.attr(Constants.ID_ATTRIBUTE, fullId);
		span.mouseover((function() {
			//span.addClass(Constants.HIGHLIGHTED_CLASS);
			$('[' + Constants.ID_ATTRIBUTE + '="' + fullId + '"]').addClass(Constants.HIGHLIGHTED_CLASS);
		}));
		span.mouseout((function() {
			//span.removeClass(Constants.HIGHLIGHTED_CLASS);
			$('[' + Constants.ID_ATTRIBUTE + '="' + fullId + '"]').removeClass(Constants.HIGHLIGHTED_CLASS);
		}));
		return span;
	},
	getKeywordNode: function(text, id, whitespace) {
		var span = Statics.getVariableNode(text, id, whitespace);
		span.addClass(Constants.KEYWORD_CLASS);
		return span;
	},
	getConstantNode: function(text, id, whitespace) {
		var span = Statics.getVariableNode(text, id, whitespace);
		span.addClass(Constants.CONSTANT_CLASS);
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
