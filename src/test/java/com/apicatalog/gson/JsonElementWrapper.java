package com.apicatalog.gson;

import com.apicatalog.json.JsonArray;
import com.apicatalog.json.JsonElement;
import com.apicatalog.json.JsonObject;

public class JsonElementWrapper implements JsonElement {

	private final com.google.gson.JsonElement element;
	
	public JsonElementWrapper(final com.google.gson.JsonElement element) {
		this.element = element;
	}

	@Override
	public boolean isJsonArray() {
		return element.isJsonArray();
	}

	@Override
	public boolean isJsonObject() {
		return element.isJsonObject();
	}

	@Override
	public JsonObject getAsJsonObject() {
		return new JsonObjectWrapper(element.getAsJsonObject());
	}

	@Override
	public JsonArray getAsJsonArray() {
		return new JsonArrayWrapper(element.getAsJsonArray());
	}

}
