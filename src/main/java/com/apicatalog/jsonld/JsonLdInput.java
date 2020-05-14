package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.impl.RemoteInput;

/**
 * The {@link JsonLdInput} interface is used to refer to an 
 * input value that that may be a {@link JsonObject}, 
 * a sequence of {@link JsonObject}, {@link URI},
 * which can be dereferenced to retrieve a valid JSON document, or 
 * an already dereferenced {@link RemoteDocument}.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#webidl-295859679">JsonLdInput IDL</a>
 *
 */
public interface JsonLdInput {

	JsonValue asJsonValue(final JsonLdOptions options) throws JsonLdError;
	
	public static JsonLdInput of(JsonObject...objects) {
		return null;
	}

	public static JsonLdInput of(URI documentUri) {
		return new RemoteInput(documentUri);
	}

	public static JsonLdInput of(RemoteDocument remoteDocument) {
		return null;
	}

}
