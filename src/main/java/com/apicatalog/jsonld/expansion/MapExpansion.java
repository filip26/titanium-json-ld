package com.apicatalog.jsonld.expansion;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
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
		
		// 7.
		
		// 8.
		
		// 9.
		if (element.containsKey(Keywords.CONTEXT)) {
			activeContext = ContextProcessor.with(activeContext, element.get(Keywords.CONTEXT), baseUrl).compute();
		}
		
		// 10.
		ActiveContext typeScoppedContext = activeContext;
		//TODO
		
		List<String> keys = new ArrayList<>(element.keySet());
		
		if (ordered) {
			Collections.sort(keys);
		}
		
		// 11.
		for (String key : keys) {
			//TODO
		}
		
		
		// 12.
		final Map<String, JsonValue> result = new LinkedHashMap<>(); 
		final Map<String, JsonValue> nest = new LinkedHashMap<>();
		String inputType = null; //TODO
		
//	   input_type = Array(input[type_key]).last
//      input_type = context.expand_iri(input_type, vocab: true, as_string: true, base: @options[:base]) if input_type
		
		// 13.
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
			
			JsonValue value = element.get(key);
			
//			TermDefinition keyTermDefinition = activeContext.getTerm(key);


			// 13.4. If expanded property is a keyword:
			if (Keywords.contains(expandedProperty.get())) {
				
				JsonValue expandedValue = JsonValue.NULL;
				
				// 13.4.2
				if (result.containsKey(expandedProperty.get()) && Keywords.isNot(expandedProperty.get(), Keywords.INCLUDED, Keywords.TYPE)) {
					throw new JsonLdError(JsonLdErrorCode.COLLIDING_KEYWORDS);
				}
				
				// 13.4.3
				if (Keywords.ID.equals(expandedProperty.get())) {
					
					// 13.4.3.1
					if (!ValueType.STRING.equals(value.getValueType())) {
						//TODO frameExpansion
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ID_VALUE);
						
					// 13.4.3.2
					} else {
					
						String stringValue = ((JsonString)value).getString();
						
						expandedValue = Json.createValue(UriExpansion
											.with(activeContext, stringValue)
											.documentRelative(true)
											.vocab(false)
											.compute().get());	//FIXME !!!
						//TODO frameExpansion
					}	
				}
				
				// 13.4.4
				if (Keywords.TYPE.equals(expandedProperty.get())) {
					// 13.4.4.1
					//TODO
					
					// 13.4.4.2
					if (ValueType.OBJECT.equals(value.getValueType()) && value.asJsonObject().isEmpty()) {
						expandedValue = value;
						
					// 13.4.4.3
					} else if (false) {
						//TODO
						
					// 13.4.4.4
					} else {
						
						if (ValueType.STRING.equals(value.getValueType())) {
							expandedValue = Json.createValue(UriExpansion
												.with(typeScoppedContext, ((JsonString)value).getString())
												.vocab(true)	//TODO ?!
												.documentRelative(true)
												.compute().get());	//FIXME !!!
							

						} else {
							//TODO
						}
					}

					// 13.4.4.5
					//TODO
				}
				
				//TODO
				
				
				// 13.4.7
				if (Keywords.VALUE.equals(expandedProperty.get())) {
					
					// 13.4.7.1
					if (Keywords.JSON.equals(inputType)) {
						expandedValue = value;
						
					} else {
					
						//TODO
						// 13.4.7.3
						expandedValue = value;
					}
					
				}

				// 13.4.8
				if (Keywords.LANGUAGE.equals(expandedProperty.get())) {
					
					// 13.4.8.1
					if (!ValueType.STRING.equals(value.getValueType())) {
						//TODO frameExpansion
						throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
					}
					
					// 13.4.8.2
					expandedValue = value;
					//TODO validation, warning, frameExpansion

					
				}
				
				//TODO

				
				// 13.4.16
				if (!ValueType.NULL.equals(expandedValue.getValueType())
//FIXME?!						&& Keywords.VALUE.equals(expandedProperty.get())
						&& !Keywords.JSON.equals(inputType)
						) {
					
					result.put(expandedProperty.get(), expandedValue);
				}
				
				// 13.4.17
				continue;
			}

			
			// 13.5.
			//TODO
			
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
			addValue(result, expandedProperty.get(), expandedValue, true);			
		}
		
		// 14.
		//TODO

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

		// 16. Otherwise, if result contains the entry @type and its associated value is not an array, 
		//	   set it to an array containing only the associated value.
		} else if (result.containsKey(Keywords.TYPE)) {
			
			final JsonValue value = result.get(Keywords.TYPE);
			
			if (!ValueType.ARRAY.equals(value.getValueType())) {
				result.put(Keywords.TYPE, Json.createArrayBuilder().add(value).build());
			}
		}
				
		// 17.
		// TODO

		// 18.
		if (result.size() == 1 && result.containsKey(Keywords.LANGUAGE)) {
			return JsonValue.NULL;
		}
			
		// 19.
		if (activeProperty == null || Keywords.GRAPH.equals(activeProperty)) {

			// 19.1. If result is a map which is empty, or contains only the entries @value or @list, set result to null
			if (result.isEmpty()) {
				return JsonValue.NULL;
			}
				
			if (result.size() == 1
					&& (result.containsKey(Keywords.VALUE)
							|| result.containsKey(Keywords.LIST)
							)
					) {
				return JsonValue.NULL;
			}

			if (result.size() == 2
					&& result.containsKey(Keywords.VALUE)
					&& result.containsKey(Keywords.LIST)
					) {
				return JsonValue.NULL;
			}

			// 19.2. if result is a map whose only entry is @id, set result to null. When the frameExpansion flag is set, a map containing only the @id entry is retained.
			if (result.size() == 1 && result.containsKey(Keywords.ID) && !frameExpansion) {
				return JsonValue.NULL;
			}
		}

		final JsonObjectBuilder resultBuilder = Json.createObjectBuilder();

		result.entrySet()
				.stream()
//				.sorted(Map.Entry.<String, JsonValue>comparingByKey())
				.forEach(e -> resultBuilder.add(e.getKey(), e.getValue()));
		
		return resultBuilder.build();
	}	
	
	public static void addValue(Map<String, JsonValue> object, String key, JsonValue value, boolean asArray) {
		
		JsonArrayBuilder array = null;
		
		// 1. If as array is true and the value of key in object does not exist or is not an array, 
		//    set it to a new array containing any original value.
		if (asArray) {
			
			array = Json.createArrayBuilder();
			
			if (object.containsKey(key)) {
				array.add(object.get(key));
			}
		}
		
		// 2. If value is an array, then for each element v in value, use add value recursively to add v to key in entry.
		if (ValueType.ARRAY.equals(value.getValueType())) {
			
			for (JsonValue v : value.asJsonArray()) {
				addValue(object, key, v, false);
			}

		// 3.
		} else {
			// 3.1
			if (array == null && !object.containsKey(key)) {
				object.put(key, value);
				
			// 3.2
			} else {
				
				// 3.2.1
				if (array == null) {
					array  = Json.createArrayBuilder();
					if (object.containsKey(key)) {
						array.add(object.get(key));
					}
				}
		
				// 3.2.2
				object.put(key, array.add(value).build());
			}	
		}
	}
}
