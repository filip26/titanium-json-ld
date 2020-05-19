package com.apicatalog.jsonld.processor;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.expansion.Expansion;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

/**
 * 
 * @see <a href="">Expansion</a>
 *
 */
public class ExpansionProcessor {

	ExpansionProcessor() {
	}
	
	public static JsonArray expand(JsonStructure input, JsonLdOptions options) {
		// TODO Auto-generated method stub
		return null;
	}

	public static final JsonArray expand(final URI input, final JsonLdOptions options) throws JsonLdError {
		
		if (options.getDocumentLoader() == null) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		}
		
		try {
			 
			final RemoteDocument remoteDocument = 
					options
						.getDocumentLoader()
							.loadDocument(
									input.toURL(), 
									new LoadDocumentOptions()
										.setExtractAllScripts(options.isExtractAllScripts())
									);
			
			return expand(remoteDocument, options);
			
		} catch (MalformedURLException e) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
		}
	}

	
	public static final JsonArray expand(RemoteDocument input, final JsonLdOptions options) throws JsonLdError {

		final JsonStructure jsonStructure = input.getDocument().asJsonStructure();
		
		// 5. Initialize a new empty active context. The base IRI and 
		//    original base URL of the active context is set to the documentUrl 
		//    from remote document, if available; otherwise to the base option from options.
		//    If set, the base option from options overrides the base IRI.

		URI baseUri = options.getBaseURI();
		
		if (baseUri == null) {
			try {
				baseUri = input.getDocumentUrl().toURI();
			 
			} catch (URISyntaxException e) {
				throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
			}
		}

		ActiveContext activeContext = null;
		
		try {
			
			activeContext = new ActiveContext(baseUri, input.getDocumentUrl().toURI(), options.getProcessingMode());
			
		} catch (URISyntaxException e1) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		}
		
		// 6. If the expandContext option in options is set, update the active context 
		//    using the Context Processing algorithm, passing the expandContext as 
		//    local context and the original base URL from active context as base URL. 
		//    If expandContext is a map having an @context entry, pass that entry's value instead for local context.
		//TODO
		
		URI baseUrl = null;
		
		if (input.getDocumentUrl() != null) {
			try {
				baseUrl = input.getDocumentUrl().toURI();
			} catch (URISyntaxException e) {
				throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
			}
		}
		
		if (baseUrl == null) {
			baseUrl = options.getBaseURI();
		}
	
		JsonValue expanded = 
				Expansion
					.with(activeContext,jsonStructure, null, baseUrl)
					.documentLoader(options.getDocumentLoader())
					.frameExpansion(options.isFrameExpansion())
					.ordered(options.isOrdered())
					.compute();

		// 8.1
		if (ValueType.OBJECT.equals(expanded.getValueType())) {
			
			final JsonObject object = expanded.asJsonObject();
			
			if (object.size() == 1 && object.containsKey(Keywords.GRAPH)) {
				expanded = object.get(Keywords.GRAPH); 
			}
		}
		
		// 8.2
		if (ValueType.NULL.equals(expanded.getValueType())) {
			return JsonValue.EMPTY_JSON_ARRAY;
		}
		
		// 8.3
		return Json.createArrayBuilder().add(expanded).build();
	}
}
