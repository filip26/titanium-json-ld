package com.apicatalog.jsonld.expansion;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
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
		Map<String, JsonValue> result = new LinkedHashMap<>(); 
		Map<String, JsonValue> nest = new LinkedHashMap<>();
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
					
						Optional<String> expandedStringValue = UriExpansion
												.with(activeContext, ((JsonString)value).getString())
												.documentRelative(true)
												.vocab(false)
												.compute(); 

						if (expandedStringValue.isPresent()) {
							expandedValue = Json.createValue(expandedStringValue.get());
						}
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
							
							Optional<String> expandedStringValue = 
										UriExpansion
												.with(typeScoppedContext, ((JsonString)value).getString())
												.vocab(true)	//TODO ?!
												.documentRelative(true)
												.compute(); 
							
							if (expandedStringValue.isPresent()) {
								expandedValue = Json.createValue(expandedStringValue.get());
							}
							

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
				
				// 13.4.11
				if (Keywords.LIST.equals(expandedProperty.get())) {
					
					// 13.4.11.1
					if (activeProperty == null || Keywords.GRAPH.equals(activeProperty)) {
						continue;
					}

					// 13.4.11.1
					expandedValue = Expansion
										.with(activeContext, value, activeProperty, baseUrl)
										.frameExpansion(frameExpansion)
										.ordered(ordered)
										.compute();
					
					if (!ValueType.ARRAY.equals(expandedValue.getValueType())) {
						expandedValue = Json.createArrayBuilder().add(expandedValue).build();
					}
				}

				// 13.4.12
				if (Keywords.SET.equals(expandedProperty.get())) {
					
					expandedValue = Expansion
							.with(activeContext, value, activeProperty, baseUrl)
							.frameExpansion(frameExpansion)
							.ordered(ordered)
							.compute();
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
			TermDefinition keyTermDefinition = activeContext.getTerm(key);

			Collection<String> containerMapping = null;
			
			if (keyTermDefinition != null) {
				containerMapping = keyTermDefinition.getContainerMapping();
			}
			if (containerMapping == null) {
				containerMapping = Collections.emptyList();
			}
									

			// 13.6.
			//TODO

			// 13.7.1
			JsonValue expandedValue = Json.createArrayBuilder().build();
			
			// 13.9.
			expandedValue =  Expansion
											.with(activeContext, value, key, baseUrl)
											.frameExpansion(frameExpansion)
											.ordered(ordered)
											.compute();
					
			
			// 13.10.
			if (ValueType.NULL.equals(expandedValue.getValueType())) {
				continue;
			}
			
			// 13.11.
			if (containerMapping.contains(Keywords.LIST) && !isListObject(expandedValue)) {
				expandedValue = toListObject(expandedValue);
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
			
		// 17.
		} else if (result.containsKey(Keywords.LIST) || result.containsKey(Keywords.SET)) {
			
			// 17.1.
			//TODO
			
			// 17.2.
			if (result.containsKey(Keywords.SET)) {
				return result.get(Keywords.SET);
//				//TODO
			}
			
		}

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
		
		if (result.isEmpty()) {
			return JsonValue.NULL;
		}

		final JsonObjectBuilder resultBuilder = Json.createObjectBuilder();

		result.entrySet()
				.stream()
//				.sorted(Map.Entry.<String, JsonValue>comparingByKey())
				.forEach(e -> resultBuilder.add(e.getKey(), e.getValue()));
		
		return resultBuilder.build();
	}	
	
	//TODO don't use this algorithm, easy reduce complexity 
	public static void addValue(Map<String, JsonValue> object, String key, JsonValue value, boolean asArray) {
		
		// 1. If as array is true and the value of key in object does not exist or is not an array, 
		//    set it to a new array containing any original value.
		if (asArray) {
			
			if (!object.containsKey(key)) {
				object.put(key,  Json.createArrayBuilder().build());
				
			} else {
				
				JsonValue original = object.get(key);
				
				if (!ValueType.ARRAY.equals(original.getValueType())){
					object.put(key,  Json.createArrayBuilder().add(original).build());
				}
			}
		}
		
		// 2. If value is an array, then for each element v in value, use add value recursively to add v to key in entry.
		if (ValueType.ARRAY.equals(value.getValueType())) {
			
			for (JsonValue v : value.asJsonArray()) {
				addValue(object, key, v, asArray);
			}

		// 3.
		} else {
			// 3.1
			if (!object.containsKey(key)) {
				object.put(key, value);
				
			// 3.2
			} else {

				JsonValue original = object.get(key);
				
				// 3.2.1
				if (!ValueType.ARRAY.equals(original.getValueType())){
					object.put(key,  Json.createArrayBuilder().add(original).build());

				// 3.2.2
				} else {
					
					JsonArrayBuilder array = Json.createArrayBuilder();
					
					original.asJsonArray().stream().forEach(array::add);
					
					object.put(key, array.add(value).build());
				}
			}	
		}
	}
	
	/**
	 * list object
     * A list object is a map that has a @list key. It may also have an @index key, but no other entries. 
     * See the Lists and Sets section of JSON-LD 1.1 for a normative description.
     * 
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-list-object">List Object</a>
     * 
	 * @param value
	 * @return
	 */
	static final boolean isListObject(JsonValue value) {
		
		return ValueType.OBJECT.equals(value.getValueType()) && value.asJsonObject().containsKey(Keywords.LIST);
		//TODO check @index and no other entries
		
	}
	
	/*
	 *  convert expanded value to a list object by 
	 *  first setting it to an array containing only expanded value if it is not already an array, 
	 *  and then by setting it to a map containing the key-value pair @list-expanded value.
	 */
	static final JsonObject toListObject(JsonValue value) {
		if (ValueType.ARRAY.equals(value.getValueType())) {
			return Json.createObjectBuilder().add(Keywords.LIST, value).build();
		}
		
		return Json.createObjectBuilder().add(Keywords.LIST, Json.createArrayBuilder().add(value)).build();
		
	}
}
