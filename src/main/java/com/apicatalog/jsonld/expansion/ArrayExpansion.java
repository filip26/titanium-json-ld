package com.apicatalog.jsonld.expansion;

import java.net.URI;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>
 *
 */
public final class ArrayExpansion {

	// mandatory
	private ActiveContext activeContext; 
	private JsonArray element;
	private String activeProperty; 
	private URI baseUrl;
	
	// optional
	private boolean frameExpansion;
	private boolean ordered;
	private boolean fromMap;
	
	private ArrayExpansion(final ActiveContext activeContext, final JsonArray element, final String activeProperty, final URI baseUrl) {
		this.activeContext = activeContext;
		this.element = element;
		this.activeProperty = activeProperty;
		this.baseUrl = baseUrl;
		
		// default values
		this.frameExpansion = false;
		this.ordered = false;
		this.fromMap = false;
	}
	
	public static final ArrayExpansion with(final ActiveContext activeContext, final JsonArray element, final String activeProperty, final URI baseUrl) {
		return new ArrayExpansion(activeContext, element, activeProperty, baseUrl);
	}

	public ArrayExpansion frameExpansion(boolean value) {
		this.frameExpansion = value;
		return this;
	}
	
	public ArrayExpansion ordered(boolean value) {
		this.ordered = value;
		return this;
	}
	
	public ArrayExpansion fromMap(boolean value) {
		this.fromMap = value;
		return this;
	}
	
	public JsonValue compute() throws JsonLdError {

		final JsonArrayBuilder builder = Json.createArrayBuilder();
		
		for (final JsonValue item : element) {
			
			JsonValue expanded = Expansion
										.with(activeContext, item, activeProperty, baseUrl)
										.frameExpansion(frameExpansion)
										.ordered(ordered)
										.compute();
			
			if (ValueType.ARRAY.equals(expanded.getValueType())) {

				// append array
				for (JsonValue expandedItem : expanded.asJsonArray()) {
					
					if (ValueType.NULL.equals(expandedItem.getValueType())) {
						continue;
					}
					
					builder.add(expandedItem);
				}
				
			// append non-null element
			} else if (!ValueType.NULL.equals(expanded.getValueType())) {
				builder.add(expanded);
			}
		}

		return builder.build();
	}	
}
