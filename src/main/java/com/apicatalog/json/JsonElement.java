package com.apicatalog.json;

import com.apicatalog.jsonld.model.JsonObject;

public interface JsonElement {

	boolean isJsonArray();

	boolean isJsonObject();

	JsonObject getAsJsonObject();

}
