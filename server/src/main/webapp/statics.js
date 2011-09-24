/*
	An enum of static functions used commonly by all files.  Put any static/global functions in here.
*/
var selected = [];
var Statics = {
	fileUrl: function(file) {
		var div = $(Constants.DIV);
		var a = $(Constants.A_HTML);
		a.attr("href", Constants.DIFF_URL + "&" + Constants.FIRST_FILE + "=" +
			escape(file.id) + "&" + Constants.SECOND_FILE + "=" + file.parents[0]);
		a.attr("target", "newWindow");
		a.text(file.id);
		var span = $(Constants.SPAN);
		//span.text("versions: [" + file.versions + "]");
		span.attr({ "style": "margin-left: 10px" });
		div.append(a);
		//div.append(span);
		var span = $(Constants.SPAN);
		//span.attr({ "style": "margin-left: 10px" });
		var checked = false
		var checkbox = $("<input type='checkbox'></input>");
		checkbox.click(function() {
			checked = !checked;
			if (checked) {
				selected.push(file);
			} else {
				selected.splice(selected.indexOf(file), 1);
			}
		});
		//span.append(checkbox);
		div.append(span);
		return div;
	},
	getSubmitButton: function() {
		var span = $(Constants.SPAN);
		var button = $("<input type='button' value='Diff'></input>");
		button.click(function() {
			if (!selected.length) {
				alert("Please select at least one file");
			} else {
				var file = selected[0];
				var file2 = selected.length > 1 ? selected[1] : selected[0];
				window.open(Constants.DIFF_URL + "&" + Constants.FIRST_FILE + "=" +
					escape(file.id) + "&" + Constants.FIRST_FILE_VERSION + "=" + file.id +
					"&" + Constants.SECOND_FILE + "=" +
					escape(file2.id) + "&" + Constants.SECOND_FILE_VERSION + "=" + file2.id);
			}
		});
		//span.append(button);
		return span;
	},
	getButtons: function(statements) {
		var span = $(Constants.SPAN);
		var button = $("<input type='button' value='plain'></input>");
		button.click(function() {		
			var myWin = window.open();
			$(myWin.document.body).attr("style", "white-space: pre");
			for (var i = 0; i < statements.length; i++) {
				var mspan = $(Constants.SPAN);
				mspan.text(statements[i].getText());
				$(myWin.document.body).append(mspan);
			}
		});
		span.append(button);
		return span;
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
	getWhitespaceNode: function(indent) {
		var text = '';
		for (var i = 0; i < indent; i++) {
			text = " " + text;
		}
		var div = $(Constants.SPAN);
		div.text(text);
		div.addClass(Constants.WHITESPACE_CLASS);
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
	getVariableNode: function(text, id, whitespace, closeParen, statement) {
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
		var deleted = false;
		span.click(function() {
			if (deleted) {
				span.removeClass(Constants.REMOVED_CLASS);
				deleted = false;
			} else {
				span.addClass(Constants.REMOVED_CLASS);	
				deleted = true;
			}
			statement.setDeleted(deleted);
		});
		span.dblclick(function() {
			if (value = prompt("Enter a new value for: " + text, text)) {
				value = value.replace(/ /g, '');
				if (value) {
					statement.setValue(value);
					span.text(value + (whitespace ? Constants.WHITESPACE : ''));
					text = value;
				}
			}
		});
		return holder;
	},
	getKeywordNode: function(text, id, whitespace, closeParen, statement) {
		var span = Statics.getVariableNode(text, id, whitespace, closeParen, statement);
		span.children().addClass(Constants.KEYWORD_CLASS);
		return span;
	},
	getConstantNode: function(text, id, whitespace, closeParen, statement) {
		var span = Statics.getVariableNode(text, id, whitespace, closeParen, statement);
		span.children().addClass(Constants.CONSTANT_CLASS);
		return span;
	},
	getCommentNode: function(text, id, whitespace, closeParen, statement) {
		var span = Statics.getVariableNode(text, id, whitespace, closeParen, statement);
		span.children().addClass(Constants.COMMENT_CLASS);
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
