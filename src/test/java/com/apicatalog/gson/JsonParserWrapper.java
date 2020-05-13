package com.apicatalog.gson;

import com.apicatalog.json.JsonElement;
import com.apicatalog.json.JsonParser;

public final class JsonParserWrapper implements JsonParser {

	public final JsonElement parse(String json) {		
		 return new JsonElementWrapper(com.google.gson.JsonParser.parseString(json));
	}
	
}
