package com.apicatalog.jsonld;

import javax.json.JsonObject;

import com.apicatalog.jsonld.grammar.Version;

public class JsonLdTestDefinition {

	public String id;
	public String name;
	public String input;
	public String expect;
	public String expectErrorCode;
	public Version specVersion;
	public JsonLdTestOptions options;
	
	public JsonLdTestDefinition expectErrorCode(String errorCode) {
		
		if (errorCode == null || errorCode.isBlank()) {
			return this;
		}
		
		expectErrorCode = errorCode.strip().toUpperCase().replace(" ", "_").replace("-", "_").replaceAll("\\_\\@", "_KEYWORD_" ); 
		
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
			
			if (o.getJsonObject("option").containsKey("specVersion")) {
				testDefinition.specVersion = Version.of(o.getJsonObject("option").getString("specVersion"));
			}
			
			testDefinition.options = JsonLdTestOptions.of(o.get("option").asJsonObject());
		}
		
		return testDefinition;
	}	
}
