package com.apicatalog.jsonld.expansion;

import java.net.URI;

import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.utils.JsonUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>
 *
 */
public final class Expansion {

	// mandatory
	private ActiveContext activeContext; 
	private JsonValue element;
	private String activeProperty; 
	private URI baseUrl;
	
	// optional
	private boolean frameExpansion;
	private boolean ordered;
	private boolean fromMap;
	private LoadDocumentCallback documentLoader;
	
	private Expansion(final ActiveContext activeContext, final JsonValue element, final String activeProperty, final URI baseUrl) {
		this.activeContext = activeContext;
		this.element = element;
		this.activeProperty = activeProperty;
		this.baseUrl = baseUrl;
		
		// default values
		this.frameExpansion = false;
		this.ordered = false;
		this.fromMap = false;
	}
	
	public static final Expansion with(final ActiveContext activeContext, final JsonValue element, final String activeProperty, final URI baseUrl) {
		return new Expansion(activeContext, element, activeProperty, baseUrl);
	}

	public Expansion frameExpansion(boolean value) {
		this.frameExpansion = value;
		return this;
	}
	
	public Expansion ordered(boolean value) {
		this.ordered = value;
		return this;
	}
	
	public Expansion fromMap(boolean value) {
		this.fromMap = value;
		return this;
	}
	
	public Expansion documentLoader(LoadDocumentCallback documentLoader) {
		this.documentLoader = documentLoader;
		return this;
	}
	
	public JsonValue compute() throws JsonLdError {

		// 1. If element is null, return null
		if (JsonUtils.isNull(element)) {
			return JsonValue.NULL;
		}
		
		// 2. If active property is @default, initialize the frameExpansion flag to false.
		if (Keywords.DEFAULT.equals(activeProperty)) {
			frameExpansion = false;
		}
		
		// 3. If active property has a term definition in active context with a local context, 
		//    initialize property-scoped context to that local context.
		JsonValue propertyContext = null;	//JsonValue.NULL;
		
		if (activeContext.containsTerm(activeProperty)) {	
			propertyContext = activeContext.getTerm(activeProperty).getLocalContext();			
		}

		// 4. If element is a scalar
		if (JsonUtils.isScalar(element)) {
			
			return ScalarExpansionBuilder
						.with(activeContext, propertyContext, element, activeProperty)
						.documentLoader(documentLoader)
						.compute();
		}
		
		// 5. If element is an array,
		if (JsonUtils.isArray(element)) {
			
			return ArrayExpansion
						.with(activeContext, element.asJsonArray(), activeProperty, baseUrl)
						.documentLoader(documentLoader)
						.compute();
		}

		// 6. Otherwise element is a map
		return MapExpansion
					.with(activeContext, propertyContext, element.asJsonObject(), activeProperty, baseUrl)
					.documentLoader(documentLoader)
					.frameExpansion(frameExpansion)
					.ordered(ordered)
					.fromMap(fromMap)
					.compute();
	}
}