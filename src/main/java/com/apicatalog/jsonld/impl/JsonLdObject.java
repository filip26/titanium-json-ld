package com.apicatalog.jsonld.impl;

import com.apicatalog.json.JsonObject;
import com.apicatalog.jsonld.JsonLdRecord;

public class JsonLdObject implements JsonLdRecord {

	private final JsonObject jsonObject;
	
	public JsonLdObject(JsonObject jsonObject) {
		this.jsonObject = jsonObject;
	}
	
	
}
