package com.apicatalog.jsonld.input;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdInput;

public class JsonStructureInput implements JsonLdInput {

	private final JsonStructure jsonStructure;

	public JsonStructureInput(JsonStructure jsonStructure) {
		this.jsonStructure = jsonStructure;
	}

	public JsonStructure getJsonStructure() {
		return jsonStructure;
	}

}
