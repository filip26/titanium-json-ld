package com.apicatalog.jsonld.document;

import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLdError;

public interface Document {
	
	JsonValue asJsonValue() throws JsonLdError;
	
}
