/*
	Recursive data structure holding a single statement that can print itself out or query itself.
	// TODO: allow querying of object
*/
function Statement(value, type, id, body) {
	var deleted = false;
	var inserted = false;
	var inserted = [];
	this.deleteStatement = function() {
		deleted = true;
	}
	this.insertStatement = function() {
		inserted = true;
	}
	this.isDeleted = function() {
		return deleted;
	}
	this.isInserted = function() {
		return inserted;
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
	this.insert = function(statement) {
		inserted.push(statement);
	}
	this.finalize = function() {
		for (var i = 0; i < inserted.length; i++) {
			body.push(inserted);
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
			this.reset = function() {
				index = 0;
			};
		}
	}
	this.getPrettyNode = function(optWhitespace, optDeleted, optInserted) {
		var div;
		var deleted = optDeleted || this.isDeleted();
		var inserted = optInserted || this.isInserted();
		if (this.isFunction()) {
			div = Statics.getStatementHolderNode(id);
			var span = Statics.getOpenFuncNode(id);
			if (deleted) {
				span.addClass(Constants.REMOVED_CLASS);
			}
			if (inserted) {
				span.addClass(Constants.INSERTED_CLASS);
			}
			div.append(span);
			var iterator = this.iterator();
			while (iterator.hasNext()) {
				var span = iterator.next().getPrettyNode(iterator.hasNext(), deleted, inserted);
				if (deleted) {
					span.addClass(Constants.REMOVED_CLASS);
				}
				if (inserted) {
					span.addClass(Constants.INSERTED_CLASS);
				}
				div.append(span);
			}
			span = Statics.getCloseFuncNode(id);
			if (deleted) {
				span.addClass(Constants.REMOVED_CLASS);
			}
			if (inserted) {
				span.addClass(Constants.INSERTED_CLASS);
			}
			div.append(span);
		} else if (this.isKeyword()) {
			div = Statics.getKeywordNode(value, id, !!optWhitespace);
		} else if (this.isConstant()) {
			div = Statics.getConstantNode(value, id, !!optWhitespace);
		} else {
			div = Statics.getVariableNode(value, id, !!optWhitespace);
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
