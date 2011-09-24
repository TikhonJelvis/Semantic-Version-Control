
var parseTree = new ParseTree([
	{
			value: "(define num 5)",
			type: "function",
			id: 1,
			body: [
				{
					value: "define",
					type: "keyword",
					id: 2
				},
				{
					value: "num",
					type: "variable",
					id: 3
				},
				{
					value: "5",
					type: "number",
					id: 4
				}
			]
	},
	{
			value: "(do (some stuff (with)) depth)",
			type: "function",
			id: 5,
			body: [
				{
					value: "do",
					type: "variable",
					id: 6	
				},
				{
					value: "(some stuff (num))",
					type: "function",
					id: 7,
					body: [
						{
							value: "some",
							type: "variable",
							id: 8
						},
						{
							value: "stuff",
							type: "variable",
							id: 9
						},
						{
							value: "(num)",
							type: "function",
							id: 10,
							body: [
								{
									value: "num",
									type: "variable",
									id: 3
								}
							]
						}
					]
				},
				{
					value: "depth",
					type: "variable",
					id: 12
				}
			]
	}
	]);
var diffTree = new diffTree([
		{
			operation: 'add',
			indices: [
				1, 1, 2
			],
			body: [
					{
						value: "hello",
						type: "variable",
						id: 13
					},
					{
						value: "world",
						type: "variable",
						id: 14
					},
			]
		},
		{
			operation: 'insert',
			indices: [
				1
			],
			body: [	
					{
						value: "(top-level insert)",
						type: "function",
						id: 15,
						body: [		
							{
								value: "top-level",
								type: "variable",
								id: 16
							},
							{
								value: "insert",
								type: "variable",
								id: 17
							}
						]
					},
					{
						value: "(you-can insert (multiple (statements)))",
						type: "function",
						id: 18,
						body: [	
							{
								value: "you-can",
								type: "variable",
								id: 19
							},
							{
								value: "insert",
								type: "variable",
								id: 17
							},	
							{
								value: "(multiple (statements))",
								type: "function",
								id: 21,
								body: [
									{
										value: "multiple",
										type: "variable",
										id: 22
									},
									{
										value: "(statements)",
										type: "function",
										id: 23,
										body: [
											{
												value: "statements",
												type: "variable",
												id: 24
											}
										]
									}
								]
							}
						]
					}
			]
		},
		{
			operation: 'remove',
			indices: [
				1, 1, 2
			]
		}
	]);
var iterator = parseTree.iterator();
while (iterator.hasNext()) {
	var node = iterator.next().getPrettyNode();
	Constants.BODY.append(node);
}
Constants.BODY.append($("<br/>"));
Constants.BODY.append($("<br/>"));
Constants.BODY.append($("<br/>"));
iterator = diffTree.parseFrom(parseTree);
while (iterator.hasNext()) {
	var node = iterator.next().getPrettyNode();
	Constants.BODY.append(node);
}
