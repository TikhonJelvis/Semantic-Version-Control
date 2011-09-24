function diffTree(tree) {
	this.parseFrom = function(parseTree) {
		var iterator = parseTree.iterator();
		var statements = [];
		var inserted = [];
		while(iterator.hasNext()) {
			statements.push(iterator.next());
		}
		iterator = this.iterator();
		while(iterator.hasNext()) {
			var diff = iterator.next();
			if (diff.operation == Constants.ADD) {
				var parser = new ParseTree(diff.body).iterator();
				var indices = diff.location;
				if (indices.length > 1) {
					var current = statements[indices[0]];
					for(var i = 1; i < indices.length - 1; i++) {
						current = current.get(indices[i]);
					}
					while (parser.hasNext()) {
						var statement = parser.next();
						statement.insertStatement();
						current.insert(statement, indices[i]);
					}
				} else {
					inserted[indices[0]] = parser;
				}
			} else if (diff.operation == Constants.REMOVE) {
				var indices = diff.location;
				var current = statements[indices[0]];
				for(var i = 1; i < indices.length; i++) {
					current = current.get(indices[i]);
				}
				current.deleteStatement();
			} else if (diff.operation == Constants.MODIFICATION) {
				var statement = Statics.parseObject(diff.body);
				console.log(statement.getText());
				statement.insertStatement();
				var indices = diff.location;
				if (indices.length > 1) {
					var current = statements[indices[0]];
					for(var i = 1; i < indices.length - 1; i++) {
						current = current.get(indices[i]);
					}
					current.replace(indices[i], statement);
				} else {
					statements[indices[0]] = statement;
				}
			}
		}
		for(var j = inserted.length; j >= 0; j--) {
			if (!inserted[j]) {
				continue;
			}
			var backwards = [];
			var iterator = inserted[j].iterator();
			while(iterator.hasNext()) {
				backwards.push(iterator.next());
			}
			for (var i = backwards.length - 1; i >= 0; i--) {
				backwards[i].insertStatement();
				statements.splice(j, 0, backwards[i]);
			}
		}
		for(var i = 0; i < statements.length; i++) {
			statements[i].finalize();
		}
		return new function() {
			var index = 0;
			this.hasNext = function() {
				return !!statements[index];
			};
			this.next = function() {
				return statements[index++];
			};
			this.reset = function() {
				index = 0;
			};
		};
	};
	this.iterator = function() {
		return new function() {
			var index = 0;
			this.reset = function() {
				index = 0;
			};
			this.hasNext = function() {
				return !!tree[index];
			};
			this.next = function() {
				return tree[index++];
			};
		}
	};
}
