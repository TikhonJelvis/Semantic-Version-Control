/*
	Iterator over an array of JSON Objects each representing a statement. The next function returns a parsed Statement object.
*/
function ParseTree(tree) {
	this.iterator = function() {
		return new function() {
			var index = 0;
			this.hasNext = function() {
				return !!tree.body[index];
			};
			this.next = function() {
				return Statics.parseObject(tree.body[index++]);
			};
			this.reset = function() {
				index = 0;
			};
		}
	}
}
