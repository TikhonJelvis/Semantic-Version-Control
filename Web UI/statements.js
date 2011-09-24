/*
	Recursive data structure holding a single statement that can print itself out or query itself.
	// TODO: allow querying of object
*/
function Statement(value, type, id, body) {
	var deleted = false;
	var isInserted = false;
	var inserted = [];
	this.deleteStatement = function() {
		deleted = true;
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
	this.getPrettyNode = function(optWhitespace, optDeleted, optInserted, wasKeyword, closeParen) {
		closeParen = closeParen ? closeParen : 0;
		var div;
		var deleted = optDeleted || this.isDeleted();
		var inserted = optInserted || this.isInserted();
		if (this.isFunction()) {
			div = Statics.getStatementHolderNode(id);
			if (!wasKeyword) {
				div.append(Statics.getBreakNode());
			}
			var span = Statics.getOpenFuncNode(id);
			if (deleted) {
				span.addClass(Constants.REMOVED_CLASS);
			}
			if (inserted) {
				span.addClass(Constants.INSERTED_CLASS);
			}
			div.append(span);
			var iterator = this.iterator();
			var wasFunction = false;
			var waskeyword = false;
			while (iterator.hasNext()) {
				var next = iterator.next();
				span = next.getPrettyNode(iterator.hasNext() ? !iterator.peek().isFunction() : false,
					deleted, inserted, wasKeyword, iterator.hasNext() ? 0 : ++closeParen);
				if (deleted) {
					span.addClass(Constants.REMOVED_CLASS);
				}
				if (inserted) {
					span.addClass(Constants.INSERTED_CLASS);
				}
				if (wasFunction && !next.isFunction() && !next.isKeyword()) {
					div.append(Statics.getBreakNode());
				} else if (!next.isFunction()) {
					span.removeClass(Constants.STATEMENT_HOLDER_CLASS);
				}
				wasFunction = next.isFunction();
				wasKeyword = next.isKeyword();
				div.append(span);
			}
		} else if (this.isKeyword()) {
			div = Statics.getKeywordNode(value, id, !!optWhitespace, closeParen);
		} else if (this.isConstant()) {
			div = Statics.getConstantNode(value, id, !!optWhitespace, closeParen);
		} else {
			div = Statics.getVariableNode(value, id, !!optWhitespace, closeParen);
		}
		if (deleted) {
			div.addClass(Constants.REMOVED_CLASS);
		}
		if (inserted) {
			div.addClass(Constants.INSERTED_CLASS);
		}
		return div;
	}
	this.getText = function() {
		if (this.isFunction()) {
			var text = Constants.OPEN_PAREN;
			var iterator = this.iterator();
			while (iterator.hasNext()) {
				text += iterator.next().getText()
				if (iterator.hasNext()) {
					text += Constants.WHITESPACE;
				}
			}
			return text + Constants.CLOSE_PAREN;
		} else {
			return value;
		}
	}
}
