function diffTree(tree) {
	this.parseFrom = function(parseTree) {
		var iterator = parseTree.iterator();
		var statements = [];
		while(iterator.hasNext()) {
			statements.push(iterator.next());
		}
		iterator = this.iterator();
		while(iterator.hasNext()) {
			var diff = iterator.next();
			if (diff.operation == Constants.ADD) {
				var parser = new ParseTree(diff.body).iterator();
				var indices = diff.indices;
				var current = statements[indices[0]];
				for(var i = 1; i < indices.length; i++) {
					current = current.get(indices[i]);
				}
				while (parser.hasNext()) {
					var statement = parser.next();
					statement.insertStatement();
					current.insert(statement);
				}
			} else if (diff.operation == Constants.REMOVE) {
				var indices = diff.indices;
				var current = statements[indices[0]];
				for(var i = 1; i < indices.length; i++) {
					current = current.get(indices[i]);
				}
				current.deleteStatement();
			}
		}
		iterator = parseTree.iterator();
		while(iterator.hasNext()) {
			iterator.next().finalize();
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
