package com.apicatalog.jsonld.document;

import java.io.Reader;

import javax.json.Json;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;
import javax.json.stream.JsonParser;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;

public class DocumentReader implements Document {

	private final Reader reader;
	
	public DocumentReader(final Reader reader) {
		this.reader = reader;
	}
	
	@Override
	public JsonValue asJsonValue() throws JsonLdError {

	   try (final JsonParser parser = Json.createParser(reader)) {
		
		   if (!parser.hasNext()) {
			   throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		   }
		   
		   parser.next();
		   
		   JsonValue root = parser.getValue();
			
			if (ValueType.ARRAY.equals(root.getValueType())) {
				return root.asJsonArray();
			}
			
			if (ValueType.OBJECT.equals(root.getValueType())) {
				return root.asJsonObject();			
			}
			
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
	   }
	}
	
}
