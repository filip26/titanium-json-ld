package com.apicatalog.jsonld.input;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.JsonLdInput;

public class JsonStructureInput implements JsonLdInput {

	private final JsonStructure jsonStructure;
//TODO	private final Type type;

	public JsonStructureInput(JsonStructure jsonStructure) {
		this.jsonStructure = jsonStructure;
	}

	@Override
	public Type getType() {
		// TODO Auto-generated method stub
		return Type.RECORD;
	}

	public JsonStructure getJsonStructure() {
		return jsonStructure;
	}

}
