[
		{
			"operation": "add",
			"indices": [
				"1", "1", "2"
			],
			"body": [
					{
						"value": "hello",
						"type": "variable",
						"id": "13"
					},
					{
						"value": "world",
						"type": "variable",
						"id": "14"
					}
			]
		},
		{
			"operation": "insert",
			"indices": [
				"1"
			],
			"body": [	
					{
						"value": "(top-level insert)",
						"type": "list",
						"id": "15",
						"body": [		
							{
								"value": "top-level",
								"type": "variable",
								"id": "16"
							},
							{
								"value": "insert",
								"type": "variable",
								"id": "17"
							}
						]
					},
					{
						"value": "(you-can insert (multiple (statements)))",
						"type": "list",
						"id": "18",
						"body": [	
							{
								"value": "you-can",
								"type": "variable",
								"id": "19"
							},
							{
								"value": "insert",
								"type": "variable",
								"id": "17"
							},	
							{
								"value": "(multiple (statements))",
								"type": "list",
								"id": "21",
								"body": [
									{
										"value": "multiple",
										"type": "variable",
										"id": 22
									},
									{
										"value": "(statements)",
										"type": "list",
										"id": "23",
										"body": [
											{
												"value": "statements",
												"type": "variable",
												"id": "24"
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
			"operation": "remove",
			"indices": [
				"1", "1", "2"
			]
		}
	]