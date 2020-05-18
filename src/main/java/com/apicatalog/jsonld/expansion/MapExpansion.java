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
import javax.json.JsonArray;
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
import com.apicatalog.jsonld.grammar.ListObject;
import com.apicatalog.jsonld.utils.JsonUtils;
import com.apicatalog.jsonld.utils.UriUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>
 *
 */
public final class MapExpansion {

	// mandatory
	private ActiveContext activeContext;
	private JsonValue propertyContext;
	private JsonObject element;
	private String activeProperty; 
	private URI baseUrl;

	// optional
	private boolean frameExpansion;
	private boolean ordered;
	private boolean fromMap;
	
	private MapExpansion(final ActiveContext activeContext, final JsonValue propertyContext, final JsonObject element, final String activeProperty, final URI baseUrl) {
		this.activeContext = activeContext;
		this.propertyContext = propertyContext;
		this.element = element;
		this.activeProperty = activeProperty;
		this.baseUrl = baseUrl;
		
		// default values
		this.frameExpansion = false;
		this.ordered = false;
		this.fromMap = false;
	}
	
	public static final MapExpansion with(final ActiveContext activeContext, final JsonValue propertyContext, final JsonObject element, final String activeProperty, final URI baseUrl) {
		return new MapExpansion(activeContext, propertyContext, element, activeProperty, baseUrl);
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
		if (activeContext.hasPreviousContext()) {
			//TODO
		}
		
		// 8.
		if (propertyContext != null) {
			
			TermDefinition activePropertyDefinition = activeContext.getTerm(activeProperty);
			
			activeContext = ContextProcessor
								.with(activeContext, propertyContext, activePropertyDefinition.getBaseUrl())
								.overrideProtected(true)
								.compute()
								;
		}		
		
		// 9.
		if (element.containsKey(Keywords.CONTEXT)) {
			activeContext = ContextProcessor.with(activeContext, element.get(Keywords.CONTEXT), baseUrl).compute();
		}
		
		// 10.
		ActiveContext typeContext = activeContext;
		
		List<String> keys = new ArrayList<>(element.keySet());		
		Collections.sort(keys);
		
		// 11.
		for (String key : keys) {
			
			String expandedKey = UriExpansion
									.with(activeContext, key)
									.vocab(true)
									.compute()
									.orElse(null) //FIXME
									;
			
			if (!Keywords.TYPE.equals(expandedKey)) {
				continue;
			}
			
			JsonValue value = element.get(key);
			
			// 11.1
			if (!ValueType.ARRAY.equals(value.getValueType())) {
				value = Json.createArrayBuilder().add(value).build();
			}
			
			// 11.2
			JsonArray valueArray = value.asJsonArray();
			//TODO oder lex
			
			for (JsonValue term : valueArray) {
				
				if (ValueType.STRING.equals(term.getValueType()) 
						&& typeContext.containsTerm(((JsonString)term).getString())) {
					
					TermDefinition termDefinition = typeContext.getTerm(((JsonString)term).getString());
					//TODO ?! TermDefinition activeTermDefinition = activeContext.getTerm(((JsonString)term).getString());
					
					if (termDefinition.hasLocalContext()) {		
						activeContext = ContextProcessor
											.with(typeContext, termDefinition.getLocalContext(), termDefinition.getBaseUrl())
											.propagate(false)
											.compute();
					}
				}
			}		
		}
		
		
		// 12.
		Map<String, JsonValue> result = new LinkedHashMap<>(); 
		Map<String, JsonValue> nest = new LinkedHashMap<>();
		String inputType = null; //TODO
		
//	   input_type = Array(input[type_key]).last
//      input_type = context.expand_iri(input_type, vocab: true, as_string: true, base: @options[:base]) if input_type
		
		// 13.
		if (!ordered) {
			keys = new ArrayList<>(element.keySet());
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
			
			JsonValue value = element.get(key);
			
			// 13.4. If expanded property is a keyword:
			if (Keywords.contains(expandedProperty.get())) {
				
				JsonValue expandedValue = JsonValue.NULL;
				
				// 13.4.1
				if (Keywords.REVERSE.equals(activeProperty)) {
					throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_MAP);
				}
				
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
												.with(typeContext, ((JsonString)value).getString())
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
					if (result.containsKey(Keywords.TYPE)) {
						
						JsonValue type = result.get(Keywords.TYPE);
						
						if (ValueType.ARRAY.equals(type.getValueType())) {
							
							expandedValue = Json.createArrayBuilder(type.asJsonArray()).add(expandedValue).build();
						} else {
							expandedValue = Json.createArrayBuilder().add(value).add(expandedValue).build();
						}
						
					}
				}
				
				// 13.4.5
				if (Keywords.GRAPH.equals(expandedProperty.get())) {
					
					expandedValue = Expansion
										.with(typeContext, value, Keywords.GRAPH, baseUrl)
										.frameExpansion(frameExpansion)
										.ordered(ordered)
										.compute()
										;
				}
				
				// 13.4.6
				if (Keywords.INCLUDED.equals(expandedProperty.get())) {
					
				}
				
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

			JsonValue expandedValue = JsonValue.NULL;

			// 13.6.
			if (false) {
				//TODO
				
			// 13.7			
			} else if (containerMapping.contains(Keywords.LANGUAGE)
					&& ValueType.OBJECT.equals(value.getValueType())
					) {
				// 13.7.1
				expandedValue = Json.createArrayBuilder().build();
				
			// 13.8.
			} else if (
					(containerMapping.contains(Keywords.INDEX)
					|| containerMapping.contains(Keywords.TYPE)
					|| containerMapping.contains(Keywords.ID))
					&& ValueType.OBJECT.equals(value.getValueType())
					) {

				// 13.8.1
				expandedValue = Json.createArrayBuilder().build();
				
			// 13.9.
			} else {
				expandedValue =  Expansion
						.with(activeContext, value, key, baseUrl)
						.frameExpansion(frameExpansion)
						.ordered(ordered)
						.compute();
			}
			
			// 13.10.
			if (ValueType.NULL.equals(expandedValue.getValueType())) {
				continue;
			}
			
			// 13.11.
			if (containerMapping.contains(Keywords.LIST) && !ListObject.isListObject(expandedValue)) {
				expandedValue = ListObject.toListObject(expandedValue);
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
			if (!Keywords.allIsOneOf(
					result.keySet(), 
					Keywords.DIRECTION,
					Keywords.INDEX,
					Keywords.LANGUAGE,
					Keywords.TYPE,
					Keywords.VALUE
					)) {
				
				throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
			}
			if ((result.keySet().contains(Keywords.DIRECTION)
					|| result.keySet().contains(Keywords.LANGUAGE))
				&& result.keySet().contains(Keywords.TYPE)
				) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
			}

			// 15.2.
			JsonValue type = result.get(Keywords.TYPE);
			
			if (JsonUtils.contains(Keywords.JSON, type)) {

			} else {
				
				JsonValue value = result.get(Keywords.VALUE);
				
				// 15.3.
				if (value == null || ValueType.NULL.equals(value.getValueType())
						|| (ValueType.ARRAY.equals(value.getValueType()) && value.asJsonArray().isEmpty())
						) {
					return JsonValue.NULL;
					

				// 15.4
				} else if (!ValueType.STRING.equals(value.getValueType()) && result.containsKey(Keywords.LANGUAGE))  {
					throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_VALUE);

				// 15.5			
				} else if (result.containsKey(Keywords.TYPE)
						&& !ValueType.STRING.equals(type.getValueType())
						&& UriUtils.isNotURI(((JsonString)type).getString())
						){
					throw new JsonLdError(JsonLdErrorCode.INVALID_TYPED_VALUE);
				}
			}

			
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
			if (result.size() > 2 || result.size() == 2 && !result.containsKey(Keywords.INDEX)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_SET_OR_LIST_OBJECT);
			}
			
			// 17.2.
			if (result.containsKey(Keywords.SET)) {
				JsonValue set = result.get(Keywords.SET);
				
				if (!ValueType.OBJECT.equals(set.getValueType())) {
					return set;
				}			

				result = set.asJsonObject();
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
					object.put(key, Json.createArrayBuilder(original.asJsonArray()).add(value).build());
				}
			}	
		}
	}
}
