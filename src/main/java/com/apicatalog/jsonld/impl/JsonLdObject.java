package com.apicatalog.jsonld.impl;

import com.apicatalog.jsonld.JsonLdRecord;
import com.apicatalog.jsonld.model.JsonObject;

public class JsonLdObject implements JsonLdRecord {

	private final JsonObject jsonObject;
	
	public JsonLdObject(JsonObject jsonObject) {
		this.jsonObject = jsonObject;
	}
	
	
}
