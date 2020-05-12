package com.apicatalog.jsonld;

import java.net.URI;

import com.apicatalog.jsonld.remote.RemoteDocument;

/**
 * The {@link JsonLdInput} interface is used to refer to an 
 * input value that that may be a {@link JsonLdRecord}, 
 * a sequence of {@link JsonLdRecord}, {@link URI} 
 * which can be dereferenced to retrieve a valid JSON document, or 
 * an already dereferenced {@link com.apicatalog.jsonld.remote.RemoteDocument RemoteDocument}.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#webidl-295859679">JsonLdInput IDL</a>
 *
 */
public interface JsonLdInput {

	public static JsonLdInput of(JsonLdRecord...records) {
		return null;
	}

	public static JsonLdInput of(URI document) {
		return null;
	}

	public static JsonLdInput of(RemoteDocument remoteDocument) {
		return null;
	}

}
