package com.apicatalog.jsonld.expansion;

import java.net.URL;
import java.util.Map.Entry;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.ContextProcessor;
import com.apicatalog.jsonld.grammar.Keywords;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>
 *
 */
public final class MapExpansion {

	// mandatory
	private ActiveContext activeContext; 
	private JsonObject element;
	private String activeProperty; 
	private URL baseUrl;
	
	// optional
	private boolean frameExpansion;
	private boolean ordered;
	private boolean fromMap;
	
	private MapExpansion(final ActiveContext activeContext, final JsonObject element, final String activeProperty, final URL baseUrl) {
		this.activeContext = activeContext;
		this.element = element;
		this.activeProperty = activeProperty;
		this.baseUrl = baseUrl;
		
		// default values
		this.frameExpansion = false;
		this.ordered = false;
		this.fromMap = false;
	}
	
	public static final MapExpansion with(final ActiveContext activeContext, final JsonObject element, final String activeProperty, final URL baseUrl) {
		return new MapExpansion(activeContext, element, activeProperty, baseUrl);
	}

	public MapExpansion frameExpansion(boolean value) {
		this.frameExpansion = value;
		return this;
	}
	
	public MapExpansion ordered(boolean value) {
		this.ordered = value;
		return this;
	}
	
	public MapExpansion fromMap(boolean value) {
		this.fromMap = value;
		return this;
	}
	
	public JsonValue compute() throws JsonLdError {

		// 9.
		if (element.containsKey(Keywords.CONTEXT)) {
			activeContext = ContextProcessor.with(activeContext,element.get(Keywords.CONTEXT), baseUrl).compute();
		}
		
		// 12.
		JsonObjectBuilder resultBuilder = Json.createObjectBuilder();
		JsonObjectBuilder nestBuilder = Json.createObjectBuilder();
		
		// 13.
		//TODO ordered
		for (Entry<String, JsonValue> entry : element.entrySet()) {
			
			// 13.1.
			if (Keywords.CONTEXT.equals(entry.getKey())) {
				continue;
			}
			
			
		}
		
		JsonObject result = resultBuilder.build();
		
		if (ValueType.OBJECT.equals(result.getValueType())) {

			final JsonObject object = result.asJsonObject();
			
			// 18.
			if (object.size() == 1 && object.containsKey(Keywords.LANGUAGE)) {
				return JsonValue.NULL;
			}
			
			// 19.
			if (activeProperty == null || Keywords.GRAPH.equals(activeProperty)) {

				// 19.1. If result is a map which is empty, or contains only the entries @value or @list, set result to null
				if (object.isEmpty()) {
					return JsonValue.NULL;
				}
				
				if (object.size() == 1
						&& (object.containsKey(Keywords.VALUE)
								|| object.containsKey(Keywords.LIST)
								)
						) {
					return JsonValue.NULL;
				}

				if (object.size() == 2
						&& object.containsKey(Keywords.VALUE)
						&& object.containsKey(Keywords.LIST)
						) {
					return JsonValue.NULL;
				}

				// 19.2. if result is a map whose only entry is @id, set result to null. When the frameExpansion flag is set, a map containing only the @id entry is retained.
				if (object.size() == 1 && object.containsKey(Keywords.ID) && !frameExpansion) {
					return JsonValue.NULL;
				}
			}
		}
		
		return result;
	}	
}
