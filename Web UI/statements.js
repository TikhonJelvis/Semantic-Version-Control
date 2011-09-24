/*
	Recursive data structure holding a single statement that can print itself out.
*/
function Statement(value, type, id, body) {
	var deleted = false;
	var isInserted = false;
	var inserted = [];
	this.deleteStatement = function() {
		deleted = true;
	}
	this.setValue = function(val) {
		value = val;
	}
	this.setDeleted = function(val) {
		deleted = val;
	}
	this.insertStatement = function() {
		isInserted = true;
	}
	this.isDeleted = function() {
		return deleted;
	}
	this.isInserted = function() {
		return isInserted;
	}
	this.isFunction = function() {
		return type == Constants.FUNCTION;
	}
	this.isKeyword = function() {
		return type == Constants.KEYWORD;
	}
	this.isConstant = function() {
		return type == Constants.NUMBER || type == Constants.STRING;
	}
	this.isComment = function() {
		return type == Constants.COMMENT;
	}
	this.get = function(pos) {
		return body[pos];
	}
	this.insert = function(statement, pos) {
		if (!inserted[pos]) {
			inserted[pos] = [];
		}
		inserted[pos].push(statement);
	}
	this.finalize = function() {
		for(var j = inserted.length - 1; j >= 0; j--) {
			if (!inserted[j]) {
				continue;
			}
			for (var i = inserted[j].length - 1; i >= 0; i--) {
				body.splice(j, 0, inserted[j][i]);
			}
		}
		var iterator = this.iterator();
		while(iterator.hasNext()) {
			iterator.next().finalize();
		}
	}
	this.iterator = function() {
		return new function() {
			var index = 0;
			this.hasNext = function() {
				return !!body[index];
			};
			this.hasNextNonComment = function() {
				for(var i = index; i < body.length; i++) {
					if (!body[i].isComment()) {
						return true;
					}
				}
				return false;
			};
			this.next = function() {
				return body[index++];
			};
			this.peek = function() {
				return body[index];
			};
			this.reset = function() {
				index = 0;
			};
		}
	}
	this.getPrettyNode = function(optWhitespace, optDeleted, optInserted, wasKeyword, closeParen, indent) {
		closeParen = closeParen ? closeParen : 0;
		indent = indent ? indent : 0;
		var div;
		var deleted = optDeleted || this.isDeleted();
		var inserted = optInserted || this.isInserted();
		if (this.isFunction()) {
			div = Statics.getStatementHolderNode(id);
			if (!wasKeyword) {
				div.append(Statics.getBreakNode());
				div.append(Statics.getWhitespaceNode(indent));
			} else {
				div.removeClass(Constants.STATEMENT_HOLDER_CLASS);
				if (deleted) {
					span.addClass(Constants.REMOVED_CLASS);
				}
				if (inserted) {
					span.addClass(Constants.INSERTED_CLASS);
				}
			}
			var span = Statics.getOpenFuncNode(id);
			div.append(span);
			var iterator = this.iterator();
			var wasFunction = false;
			var waskeyword = false;
			while (iterator.hasNext()) {
				var next = iterator.next();
				span = next.getPrettyNode(iterator.hasNext() ? !iterator.peek().isFunction() : false,
					deleted, inserted, wasKeyword, iterator.hasNextNonComment() ? 0 : ++closeParen, indent + 4);
				if (deleted) {
					span.addClass(Constants.REMOVED_CLASS);
				}
				if (inserted) {
					span.addClass(Constants.INSERTED_CLASS);
				}
				if (wasFunction && !next.isFunction() && !next.isKeyword()) {
					div.append(Statics.getBreakNode());
					div.append(Statics.getWhitespaceNode(indent));
				} else if (!next.isFunction()) {
					span.removeClass(Constants.STATEMENT_HOLDER_CLASS);
				}
				wasFunction = next.isFunction();
				wasComment = next.isComment();
				wasKeyword = next.isKeyword();
				div.append(span);
			}
		} else if (this.isKeyword()) {
			div = Statics.getKeywordNode(value, id, !!optWhitespace, closeParen, this);
		} else if (this.isConstant()) {
			div = Statics.getConstantNode(value, id, !!optWhitespace, closeParen, this);
		} else if (this.isComment()) {
			div = Statics.getCommentNode(value, id, !!optWhitespace, 0, this);
		} else {
			div = Statics.getVariableNode(value, id, !!optWhitespace, closeParen, this);
		}
		return div;
	}
	this.getText = function() {
		if (this.isFunction()) {
			var text = Constants.OPEN_PAREN;
			var iterator = this.iterator();
			var good = false;
			while (iterator.hasNext()) {
				var t = iterator.next().getText();
				if (!t.length) continue;
				text += t;
				if (iterator.hasNextNonComment()) {
					text += Constants.WHITESPACE;
				} else {
					text += Constants.CLOSE_PAREN;
				}
				good = true;
			}
			return (good && !this.isDeleted()) ? text : '';
		} else {
			return this.isDeleted() ? '' : this.isComment() ? (value + "\n") : value;
		}
	}
}
