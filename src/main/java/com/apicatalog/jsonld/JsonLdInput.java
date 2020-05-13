package com.apicatalog.jsonld;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.json.JsonParser;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.impl.RemoteInput;

/**
 * The {@link JsonLdInput} interface is used to refer to an 
 * input value that that may be a {@link JsonLdRecord}, 
 * a sequence of {@link JsonLdRecord}, a string representing an IRI,
 * which can be dereferenced to retrieve a valid JSON document, or 
 * an already dereferenced {@link com.apicatalog.jsonld.document.RemoteDocument RemoteDocument}.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#webidl-295859679">JsonLdInput IDL</a>
 *
 */
public interface JsonLdInput {

	Collection<JsonLdRecord> getRecords(final JsonLdOptions options, final JsonParser jsonParser) throws JsonLdError;
	
	public static JsonLdInput of(JsonLdRecord...records) {
		return null;
	}

	public static JsonLdInput of(URI documentIri) {
		return new RemoteInput(documentIri);
	}

	public static JsonLdInput of(RemoteDocument remoteDocument) {
		return null;
	}

}
