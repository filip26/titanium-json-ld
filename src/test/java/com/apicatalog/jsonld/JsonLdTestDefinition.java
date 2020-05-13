package com.apicatalog.jsonld;

public class JsonLdTestDefinition {

	public String id;
	public String name;
	public String input;
	public String expect;
	public String expectErrorCode;
	
	public JsonLdTestDefinition expectErrorCode(String errorCode) {
		
		if (errorCode == null || errorCode.isBlank()) {
			return this;
		}
		
		expectErrorCode = errorCode.strip().toUpperCase().replace(" ", "_"); 
		
		return this;
	}
	
}
