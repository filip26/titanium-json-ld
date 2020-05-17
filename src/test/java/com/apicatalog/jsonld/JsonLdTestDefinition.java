package com.apicatalog.jsonld;

import javax.json.JsonObject;

public class JsonLdTestDefinition {

	public String id;
	public String name;
	public String input;
	public String expect;
	public String expectErrorCode;
	public JsonLdTestOptions options;
	
	public JsonLdTestDefinition expectErrorCode(String errorCode) {
		
		if (errorCode == null || errorCode.isBlank()) {
			return this;
		}
		
		expectErrorCode = errorCode.strip().toUpperCase().replace(" ", "_"); 
		
		return this;
	}
	
	public static final JsonLdTestDefinition of(JsonObject o) {
		final JsonLdTestDefinition testDefinition = new JsonLdTestDefinition();
		testDefinition.id = o.getString("@id");
		testDefinition.name = o.getString("name");
		testDefinition.input = o.getString("input");
		testDefinition.expect = o.getString("expect", null);
		testDefinition.expectErrorCode(o.getString("expectErrorCode", null));

		if (o.containsKey("option")) {
			testDefinition.options = JsonLdTestOptions.of(o.get("option").asJsonObject());
		}
		
		return testDefinition;
	}	
}
