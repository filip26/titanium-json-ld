package com.apicatalog.jsonld.expansion;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.ContextProcessor;
import com.apicatalog.jsonld.context.TermDefinition;
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
	private URI baseUrl;
	
	// optional
	private boolean frameExpansion;
	private boolean ordered;
	private boolean fromMap;
	
	private MapExpansion(final ActiveContext activeContext, final JsonObject element, final String activeProperty, final URI baseUrl) {
		this.activeContext = activeContext;
		this.element = element;
		this.activeProperty = activeProperty;
		this.baseUrl = baseUrl;
		
		// default values
		this.frameExpansion = false;
		this.ordered = false;
		this.fromMap = false;
	}
	
	public static final MapExpansion with(final ActiveContext activeContext, final JsonObject element, final String activeProperty, final URI baseUrl) {
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
			activeContext = ContextProcessor.with(activeContext, element.get(Keywords.CONTEXT), baseUrl).compute();
		}
		
		// 10.
		//TODO
		
		// 11.
		//TODO
		
		// 12.
		JsonObjectBuilder resultBuilder = Json.createObjectBuilder();
		JsonObjectBuilder nestBuilder = Json.createObjectBuilder();
		
		// 13.
		List<String> keys = new ArrayList<>(element.keySet());
		
		if (ordered) {
			Collections.sort(keys);
		}
		
		for (String key : keys) {
			
			// 13.1.
			if (Keywords.CONTEXT.equals(key)) {
				continue;
			}

			// 13.2.
			Optional<String> expandedProperty = 
								UriExpansion
									.with(activeContext, key)
									.documentRelative(false)
									.vocab(true)
									.compute();
			

			// 13.3.
			if (expandedProperty.isEmpty() 
					|| !expandedProperty.get().contains(":") 
					&& !Keywords.contains(expandedProperty.get())
					) {
				continue;
			}
			
			TermDefinition keyTermDefinition = activeContext.getTerm(key);


			// 13.4.
			//TODO
			
			// 13.5.
			//TODO
			

			JsonValue value = element.get(key);
			
			// 13.9.
			JsonValue expandedValue =  Expansion
											.with(activeContext, value, key, baseUrl)
											.frameExpansion(frameExpansion)
											.ordered(ordered)
											.compute();
					
			
			// 13.10
			if (ValueType.NULL.equals(expandedValue.getValueType())) {
				continue;
			}

			//TODO
			
			// 13.14
			addValue(resultBuilder, expandedProperty.get(), expandedValue, true);			
		}
		
		// 14.
		//TODO

		JsonObject result = resultBuilder.build();
		
		// 15.
		if (result.containsKey(Keywords.VALUE)) {
			
			// 15.1.
			//TODO
			
			// 15.2.
			//TODO
			

			JsonValue value = result.get(Keywords.VALUE);

			// 15.3.
			if (ValueType.NULL.equals(value.getValueType())
				|| (ValueType.ARRAY.equals(value.getValueType()) && value.asJsonArray().isEmpty())
					) {
				return JsonValue.NULL;
				
			}
			
			// 15.4
			//TODO
				
			// 15.5			
			//TODO
			
		}
		// 16-17
		// TODO

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
	
	public static void addValue(JsonObjectBuilder builder, String key, JsonValue value, boolean asArray) {
		
//		System.out.println("<<< " + key + ", " + key + ", " +value);

		
		// 1. If as array is true and the value of key in object does not exist or is not an array, 
		//    set it to a new array containing any original value.
		if (asArray) {
			//TODO
			
		}
		
		// 2. If value is an array, then for each element v in value, use add value recursively to add v to key in entry.
		
		//TODO
		
		// 3.
		//TODO 
		
		// 3.1
		builder.add(key, value);
		
	}
	
}
