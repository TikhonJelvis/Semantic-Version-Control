var json =[{"value" : "(define x 1.0)",
"type" : "list",
"id" : 5,
"body" : [{"value" : "define",
"type" : "keyword",
"id" : 2,
"body" : []}
, {"value" : "x",
"type" : "variable",
"id" : 3,
"body" : []}
, {"value" : "1.0",
"type" : "number",
"id" : 4,
"body" : []}
]}
, {"value" : "(define x 2.0)",
"type" : "list",
"id" : 9,
"body" : [{"value" : "define",
"type" : "keyword",
"id" : 7,
"body" : []}
, {"value" : "x",
"type" : "variable",
"id" : 3,
"body" : []}
, {"value" : "2.0",
"type" : "number",
"id" : 8,
"body" : []}
]}
, {"value" : "(lambda (x) x)",
"type" : "list",
"id" : 13,
"body" : [{"value" : "lambda",
"type" : "keyword",
"id" : 12,
"body" : []}
, {"value" : "",
"type" : "list",
"id" : 13,
"body" : [{"value" : "x",
"type" : "variable",
"id" : 12,
"body" : []}
]}
, {"value" : "x",
"type" : "variable",
"id" : 12,
"body" : []}
]}
, {"value" : "(lambda (y) (+ x y))",
"type" : "list",
"id" : 20,
"body" : [{"value" : "lambda",
"type" : "keyword",
"id" : 16,
"body" : []}
, {"value" : "",
"type" : "list",
"id" : 17,
"body" : [{"value" : "y",
"type" : "variable",
"id" : 16,
"body" : []}
]}
, {"value" : "(+ x y)",
"type" : "list",
"id" : 19,
"body" : [{"value" : "+",
"type" : "variable",
"id" : 18,
"body" : []}
, {"value" : "x",
"type" : "variable",
"id" : 3,
"body" : []}
, {"value" : "y",
"type" : "variable",
"id" : 16,
"body" : []}
]}
]}
, {"value" : "(lambda (a) (lambda (b) (+ a b x)))",
"type" : "list",
"id" : 31,
"body" : [{"value" : "lambda",
"type" : "keyword",
"id" : 23,
"body" : []}
, {"value" : "",
"type" : "list",
"id" : 24,
"body" : [{"value" : "a",
"type" : "variable",
"id" : 23,
"body" : []}
]}
, {"value" : "(lambda (b) (+ a b x))",
"type" : "list",
"id" : 30,
"body" : [{"value" : "lambda",
"type" : "keyword",
"id" : 26,
"body" : []}
, {"value" : "",
"type" : "list",
"id" : 27,
"body" : [{"value" : "b",
"type" : "variable",
"id" : 26,
"body" : []}
]}
, {"value" : "(+ a b x)",
"type" : "list",
"id" : 29,
"body" : [{"value" : "+",
"type" : "variable",
"id" : 28,
"body" : []}
, {"value" : "a",
"type" : "variable",
"id" : 23,
"body" : []}
, {"value" : "b",
"type" : "variable",
"id" : 26,
"body" : []}
, {"value" : "x",
"type" : "variable",
"id" : 3,
"body" : []}
]}
]}
]}
];
var parseTree = new ParseTree(json);
var iterator = parseTree.iterator();
while (iterator.hasNext()) {
	var node = iterator.next().getPrettyNode();
	Constants.BODY.append(node);
}
