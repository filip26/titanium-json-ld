package com.apicatalog.json;

public interface JsonElement {

	boolean isJsonArray();

	boolean isJsonObject();

	JsonObject getAsJsonObject();

	JsonArray getAsJsonArray();

}
