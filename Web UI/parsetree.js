/*
	Iterator over an array of JSON Objects each representing a statement. The next function returns a parsed Statement object.
*/
function ParseTree(tree) {
	this.iterator = function() {
		return new function() {
			var index = 0;
			this.hasNext = function() {
				return !!tree[index];
			};
			this.next = function() {
				return Statics.parseObject(tree[index++]);
			};
			this.reset = function() {
				index = 0;
			};
		}
	}
}
