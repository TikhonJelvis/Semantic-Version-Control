{
	"path": "hello.world",
	"versions": [
		"1", "2"
	],
	"currentVersion": "2",
	"data": [
	{
			"value": "(define num 5)",
			"type": "list",
			"id": "1",
			"body": [
				{
					"value": "define",
					"type": "keyword",
					"id": "2"
				},
				{
					"value": "num",
					"type": "variable",
					"id": "3"
				},
				{
					"value": "5",
					"type": "number",
					"id": "4"
				}
			]
	},
	{
			"value": "(do (some stuff (with)) depth)",
			"type": "list",
			"id": 5,
			"body": [
				{
					"value": "do",
					"type": "variable",
					"id": "6"	
				},
				{
					"value": "(some stuff (num))",
					"type": "list",
					"id": "7",
					"body": [
						{
							"value": "some",
							"type": "variable",
							"id": "8"
						},
						{
							"value": "stuff",
							"type": "variable",
							"id": "9"
						},
						{
							"value": "(num)",
							"type": "list",
							"id": "10",
							"body": [
								{
									"value": "num",
									"type": "variable",
									"id": "3"
								}
							]
						}
					]
				},
				{
					"value": "depth",
					"type": "variable",
					"id": "12"
				}
			]
	}
	]
}