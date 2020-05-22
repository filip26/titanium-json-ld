package com.apicatalog.jsonld.document;

import java.io.Reader;

import javax.json.Json;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.utils.JsonUtils;

public class DocumentReader implements Document {

	private final Reader reader;

	public DocumentReader(final Reader reader) {
		this.reader = reader;
	}

	@Override
	public JsonStructure asJsonStructure() throws JsonLdError {

		try (final JsonParser parser = Json.createParser(reader)) {

			if (!parser.hasNext()) {
				throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
			}

			parser.next();

			JsonValue root = parser.getValue();

			if (JsonUtils.isArray(root)) {
				return root.asJsonArray();
			}

			if (JsonUtils.isObject(root)) {
				return root.asJsonObject();
			}

			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		}
	}

}
