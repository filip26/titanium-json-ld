package com.apicatalog.gson;

import com.apicatalog.json.JsonObject;

public class JsonObjectWrapper implements JsonObject {

	private final com.google.gson.JsonObject jsonObject;
	
	public JsonObjectWrapper(final com.google.gson.JsonObject jsonObject) {
		this.jsonObject = jsonObject;
	}
	
}
