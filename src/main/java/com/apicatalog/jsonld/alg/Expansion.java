package com.apicatalog.jsonld.alg;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.impl.ActiveContext;
import com.apicatalog.jsonld.impl.Keyword;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>
 *
 */
public class Expansion {

	public static JsonArray expand(JsonStructure input, JsonLdOptions options) {
		// TODO Auto-generated method stub
		return null;
	}

	public static final JsonArray expand(final URI input, final JsonLdOptions options) throws JsonLdError {
		
		if (options.getDocumentLoader().isEmpty()) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		}
		
		try {
						 
			final RemoteDocument remoteDocument = 
					options
						.getDocumentLoader()
						.get()
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

		final ActiveContext activeContext = new ActiveContext(baseUri, input.getDocumentUrl());
		
		// 6. If the expandContext option in options is set, update the active context 
		//    using the Context Processing algorithm, passing the expandContext as 
		//    local context and the original base URL from active context as base URL. 
		//    If expandContext is a map having an @context entry, pass that entry's value instead for local context.
		//TODO
		
		URL baseUrl = input.getDocumentUrl();
		
		if (baseUrl == null) {
			try {
				
				baseUrl = options.getBaseURI().toURL();
				
			} catch (MalformedURLException e) {
				throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
			}
		}
	
		JsonValue expanded = 
				expand(
					activeContext, 
					jsonStructure, 
					null, 
					baseUrl,
					options.isFrameExpansion(),
					options.isOrdered()
					);
			
		// 8.1
		if (ValueType.OBJECT.equals(expanded.getValueType())) {
			
			final JsonObject object = expanded.asJsonObject();
			
			if (object.size() == 1 && object.containsKey(Keyword.GRAPH.name())) {
				expanded = object.get(Keyword.GRAPH.name()); 
			}
		}
		
		// 8.2
		if (ValueType.NULL.equals(expanded.getValueType())) {
			return JsonValue.EMPTY_JSON_ARRAY;
		}
		
		// 8.3
		return Json.createArrayBuilder().add(expanded).build();
	}
	
	static final JsonValue expand(
					final ActiveContext activeContext, 
					final JsonValue element, 
					final String activeProperty, 
					final URL baseUrl,
					final boolean frameExpansion,
					final boolean ordered
					) {

		final ValueType elementType = element.getValueType();
		
		// 1. If element is null, return null.
		if (ValueType.NULL.equals(elementType)) {
			return element;
		}
		
		// 2. If active property is @default, initialize the frameExpansion flag to false.
		if ("@default".equals(activeProperty)) {
//			frameExpansion = false;
		}
		
		// 3. If active property has a term definition in active context with a local context, 
		//    initialize property-scoped context to that local context.
		//TODO
		
		// 4. If element is a scalar
		if (ValueType.STRING.equals(elementType)
			|| ValueType.NUMBER.equals(elementType)
			|| ValueType.TRUE.equals(elementType)
			|| ValueType.FALSE.equals(elementType)
			) {

			// 4.1. If active property is null or @graph, drop the free-floating scalar by returning null.
			if (activeProperty == null || "@graph".equals(activeProperty)) {
				return JsonValue.NULL;
			}
			
			/* 
			 * 4.2. If property-scoped context is defined, set active context 
			 *		to the result of the Context Processing algorithm, passing 
			 *		active context, property-scoped context as local context, 
			 *		and base URL from the term definition for active property in active context.
			 */
			//TODO 
			
			/*
			 * 4.3. Return the result of the Value Expansion algorithm, passing 
			 * 		the active context, active property, and element as value. 
			 */
		//	return ValueExpansion.expand(activeContext, element, activeProperty);
		}
		
		// 5. If element is an array,
		if (ValueType.ARRAY.equals(elementType)) {
			
			final JsonArray inputArray = element.asJsonArray();
			
			final JsonArrayBuilder builder = Json.createArrayBuilder();
			
			for (JsonValue item : inputArray) {
				
				JsonValue expanded = expand(activeContext, item, activeProperty, baseUrl, frameExpansion, ordered);
				
				if (ValueType.ARRAY.equals(expanded.getValueType())) {
					for (JsonValue expandedItem : expanded.asJsonArray()) {
						if (ValueType.NULL.equals(expandedItem.getValueType())) {
							continue;
						}
						builder.add(expandedItem);
					}
				} else if (!ValueType.NULL.equals(expanded.getValueType())) {
					builder.add(expanded);
				}
			}
			return builder.build();
		}
		
		// 6. Otherwise element is a map
		
		JsonValue result = element;
		
		// 19.
		if (activeProperty == null || Keyword.GRAPH.name().equals(activeProperty)) {
			
			if (ValueType.OBJECT.equals(elementType)) {
				
				JsonObject object = element.asJsonObject();
			
				// 19.1. If result is a map which is empty, or contains only the entries @value or @list, set result to null
				if (object.isEmpty()) {
					return JsonValue.NULL;
				}
				//TODO
				
				// 19.2. Otherwise, if result is a map whose only entry is @id, set result to null. When the frameExpansion flag is set, a map containing only the @id entry is retained.
				if (object.size() == 1 && object.containsKey(Keyword.ID.name()) && !frameExpansion) {
					return JsonValue.NULL;
				}
				
			}
		}
		
		return element;
	}	
}
