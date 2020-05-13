package com.apicatalog.jsonld.document;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;
import javax.json.stream.JsonParser;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdRecord;

public class JsonDocument implements Document {

	private final String json;
	
	public JsonDocument(final String json) {
		this.json = json;
	}
	
	@Override
	public Collection<JsonLdRecord> parse() throws JsonLdError {

	   try (final JsonParser parser = Json.createParser(new StringReader(json))) {
		
		   if (!parser.hasNext()) {
			   throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		   }
		   
		   parser.next();
		   
		   JsonValue root = parser.getValue();
			
			if (ValueType.ARRAY.equals(root.getValueType())) {
				
				final JsonArray jsonArray = root.asJsonArray();
				
				final Collection<JsonLdRecord> records = new ArrayList<>(jsonArray.size());
				
				for (JsonValue item : jsonArray) {
					
					if (!ValueType.OBJECT.equals(item.getValueType())) {
						throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
					}
					
					records.add(JsonLdRecord.of(item.asJsonObject()));
				}
				
				return records;
			}
			
			if (ValueType.OBJECT.equals(root.getValueType())) {
				return Arrays.asList(JsonLdRecord.of(root.asJsonObject()));			
			}
			
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
	   }
	}
	
}
