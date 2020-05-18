package com.apicatalog.jsonld.expansion;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.json.Json;
import javax.json.JsonArray;
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
import com.apicatalog.jsonld.grammar.DefaultObject;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.ListObject;
import com.apicatalog.jsonld.grammar.ValueObject;
import com.apicatalog.jsonld.grammar.Version;
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
		if (activeContext.hasPreviousContext() 
				&& !fromMap 
				&& !element.containsKey(Keywords.VALUE)
				&& (element.size() != 1
					|| !element.containsKey(Keywords.ID)
				)
				) {		
			activeContext = activeContext.getPreviousContext();
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
					//TODO ?!
					TermDefinition activeTermDefinition = activeContext.getTerm(((JsonString)term).getString());
					
					if (termDefinition.hasLocalContext() && activeTermDefinition != null) {		
						activeContext = ContextProcessor
											.with(typeContext, termDefinition.getLocalContext(), activeTermDefinition.getBaseUrl())
											.propagate(false)
											.compute();
					}
				}
			}		
		}
		
		// 12.
		Map<String, JsonValue> result = new LinkedHashMap<>(); 
		Map<String, JsonValue> nest = new LinkedHashMap<>();
		String inputType = null;
		
		// Initialize input type to expansion of the last value of the first entry in element 
		// expanding to @type (if any), ordering entries lexicographically by key. Both the key and
		// value of the matched entry are IRI expanded.	
		if (element.containsKey(Keywords.TYPE)) { //TODO ?!?!
			
			JsonValue t = element.get(Keywords.TYPE);
			
			if (JsonUtils.isArray(t)) {
				t = JsonUtils.last(t.asJsonArray());
			}

			if (JsonUtils.isString(t)) {
				inputType = UriExpansion.with(typeContext, ((JsonString)t).getString())
											.vocab(true)
											.compute();
			}
		}		
		
		// 13.
		if (!ordered) {
			keys = new ArrayList<>(element.keySet());
		}
		
		Map<String, JsonValue> reverseMap = null;

		for (final String key : keys) {
			
			// 13.1.
			if (Keywords.CONTEXT.equals(key)) {
				continue;
			}

			// 13.2.
			String expandedProperty = 
								UriExpansion
									.with(activeContext, key)
									.documentRelative(false)
									.vocab(true)
									.compute();
			
			// 13.3.
			if (expandedProperty == null 
					|| (!expandedProperty.contains(":") 
					&& !Keywords.contains(expandedProperty))
					) {
				continue;
			}
			
			JsonValue value = element.get(key);
			
			// 13.4. If expanded property is a keyword:
			if (Keywords.contains(expandedProperty)) {
				
				JsonValue expandedValue = JsonValue.NULL;
				
				// 13.4.1
				if (Keywords.REVERSE.equals(activeProperty)) {
					throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_MAP);
				}
				
				// 13.4.2
				if (result.containsKey(expandedProperty) && Keywords.isNot(expandedProperty, Keywords.INCLUDED, Keywords.TYPE)) {
					throw new JsonLdError(JsonLdErrorCode.COLLIDING_KEYWORDS);
				}
				
				// 13.4.3
				if (Keywords.ID.equals(expandedProperty)) {
					
					// 13.4.3.1
					if (!ValueType.STRING.equals(value.getValueType())) {
						//TODO frameExpansion
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ID_VALUE);
						
					// 13.4.3.2
					} else {

						String expandedStringValue = UriExpansion
												.with(activeContext, ((JsonString)value).getString())
												.documentRelative(true)
												.vocab(false)
												.compute(); 

						if (expandedStringValue != null) {
							expandedValue = Json.createValue(expandedStringValue);
						}
						//TODO frameExpansion
					}	
				}
				
				// 13.4.4
				if (Keywords.TYPE.equals(expandedProperty)) {
					// 13.4.4.1
					if (!frameExpansion 
							&& !ValueType.STRING.equals(value.getValueType()) 
							&& !ValueType.ARRAY.equals(value.getValueType())) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_VALUE);
					}
					//TODO
					
					// 13.4.4.2
					if (ValueType.OBJECT.equals(value.getValueType()) && value.asJsonObject().isEmpty()) {
						expandedValue = value;
						
					// 13.4.4.3
					} else if (DefaultObject.isDefaultObject(value)) {

						//TODO
						
					// 13.4.4.4
					} else {
						
						if (ValueType.STRING.equals(value.getValueType())) {
							
							String expandedStringValue = 
										UriExpansion
												.with(typeContext, ((JsonString)value).getString())
												.vocab(true)
												.documentRelative(true)
												.compute(); 
							
							if (expandedStringValue != null) {
								expandedValue = Json.createValue(expandedStringValue);
							}

						} else if (ValueType.ARRAY.equals(value.getValueType())) {

							JsonArrayBuilder array = Json.createArrayBuilder();
							
							for (JsonValue item : value.asJsonArray()) {
								
								if (ValueType.STRING.equals(item.getValueType())) {
									
									String expandedStringValue = 
												UriExpansion
														.with(typeContext, ((JsonString)item).getString())
														.vocab(true)
														.documentRelative(true)
														.compute(); 
									
									if (expandedStringValue != null) {
										array.add(Json.createValue(expandedStringValue));
									}
								}
							}
							expandedValue = array.build();
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
				if (Keywords.GRAPH.equals(expandedProperty)) {
					
					expandedValue = Expansion
										.with(typeContext, value, Keywords.GRAPH, baseUrl)
										.frameExpansion(frameExpansion)
										.ordered(ordered)
										.compute()
										;
				}
				
				// 13.4.6
				if (Keywords.INCLUDED.equals(expandedProperty)) {
					
					// 13.4.6.1
					if (activeContext.inMode(Version.V1_0)) {
						continue;
					}
					
					// 13.4.6.2					
					expandedValue = Expansion
										.with(activeContext, value, null, baseUrl)
										.frameExpansion(frameExpansion)
										.ordered(ordered)
										.compute();
					
					if (JsonUtils.isNotNull(expandedValue)) {
					
						if (JsonUtils.isNotArray(expandedValue)) {
							expandedValue = Json.createArrayBuilder().add(expandedValue).build();
						}
						
						// 13.4.6.3
						//TODO

						// 13.4.6.4
						if (result.containsKey(Keywords.INCLUDED)) {
							expandedValue = Json.createArrayBuilder(result.get(Keywords.INCLUDED).asJsonArray()).add(expandedValue).build();
						}
					}
				}
				
				// 13.4.7
				if (Keywords.VALUE.equals(expandedProperty)) {
					
					// 13.4.7.1
					if (Keywords.JSON.equals(inputType)) {
						
						if (activeContext.inMode(Version.V1_0)) {
							throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT_VALUE);
						}
						
						expandedValue = value;
						
					// 13.4.7.2
					} else if (!frameExpansion && JsonUtils.isNotNull(value) && JsonUtils.isNotScalar(value)
							//TODO frameexpansion
							) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT_VALUE);

					// 13.4.7.3
					} else {
						expandedValue = value;
					}
					
					// 13.4.7.4
					if (JsonUtils.isNull(expandedValue)) {
						result.put(Keywords.VALUE, JsonValue.NULL);
						continue;
					}
				}

				// 13.4.8
				if (Keywords.LANGUAGE.equals(expandedProperty)) {
					
					// 13.4.8.1
					if (!ValueType.STRING.equals(value.getValueType())) {
						//TODO frameExpansion
						throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
					}

					// 13.4.8.2
					expandedValue = value;
					//TODO validation, warning, frameExpansion					
				}
				
				// 13.4.9
				if (Keywords.DIRECTION.equals(expandedProperty)) {
					// 13.4.9.1
					if (activeContext.inMode(Version.V1_0)) {
						continue;
					}
					
					// 13.4.9.2
					if (JsonUtils.isNotString(value) 
							&& !((JsonString)value).getString().equals("ltr")
							&& !((JsonString)value).getString().equals("rtl")
							) {
						//TODO frameexpansion
						throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
					}
					
					expandedValue = value;
					//TODO
				}
				
				// 13.4.10
				if (Keywords.INDEX.equals(expandedProperty)) {

					// 13.4.10.1
					if (JsonUtils.isNotString(value)) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INDEX_VALUE);
					}
					
					// 13.4.10.2
					expandedValue = value;
				}
				
				// 13.4.11
				if (Keywords.LIST.equals(expandedProperty)) {
					
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
				if (Keywords.SET.equals(expandedProperty)) {
					
					expandedValue = Expansion
							.with(activeContext, value, activeProperty, baseUrl)
							.frameExpansion(frameExpansion)
							.ordered(ordered)
							.compute();
				}
				
				// 13.4.13
				if (Keywords.REVERSE.equals(expandedProperty)) {

					// 13.4.13.1.
					if (JsonUtils.isNotObject(value)) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_REVERSE_VALUE);
					}
					
					// 13.4.13.2.
					expandedValue = Expansion
										.with(activeContext, value, Keywords.REVERSE, baseUrl)
										.frameExpansion(frameExpansion)
										.ordered(ordered)
										.compute();


					if (JsonUtils.isObject(expandedValue)) { 

						// 13.4.13.3.
						if (expandedValue.asJsonObject().containsKey(Keywords.REVERSE)) {
							for (Entry<String, JsonValue> entry : expandedValue.asJsonObject().entrySet()) {
								// 13.4.13.3.1.
								addValue(result, entry.getKey(), entry.getValue(), true);
							}
						}
					
						// 13.4.13.4.
						if (expandedValue.asJsonObject().size() > 1
								|| !expandedValue.asJsonObject().containsKey(Keywords.REVERSE)
								) {

							// 13.4.13.4.1
							if (result.containsKey(Keywords.REVERSE)) {
								reverseMap = new LinkedHashMap<>(result.get(Keywords.REVERSE).asJsonObject());
							}
							
							if (reverseMap == null) {
								reverseMap = new LinkedHashMap<>();
							}
							
							// 13.4.13.4.2
							for (Entry<String, JsonValue> entry : expandedValue.asJsonObject().entrySet()) {

								if (Keywords.REVERSE.equals(entry.getKey())) {
									continue;
								}
								
								// 13.4.13.4.2.1
								if (JsonUtils.isArray(entry.getValue())) {
									
									for (JsonValue item : entry.getValue().asJsonArray()) {
										
										// 13.4.13.4.2.1.1
										if (ListObject.isListObject(item) || ValueObject.isValueObject(item)) {
											throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
										}
									
										// 13.4.13.4.2.1.1
										addValue(reverseMap, entry.getKey(), item, true);
									}
								}
							}
						}						
					}
		
					// 13.4.13.5.
					continue;
				}

				// 13.4.14
				if (Keywords.NEST.equals(expandedProperty)) {
					if (!nest.containsKey(key)) {
						nest.put(key, Json.createArrayBuilder().build());
					}
					continue;
				}

				// 13.4.15
				if (frameExpansion) {
					//TODO					
				}

				// 13.4.16
				if (!ValueType.NULL.equals(expandedValue.getValueType())
//FIXME?!						&& Keywords.VALUE.equals(expandedProperty.get())
						&& !Keywords.JSON.equals(inputType)
						) {
					
					result.put(expandedProperty, expandedValue);
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

			JsonValue expandedValue;

			// 13.6.
			if (keyTermDefinition != null && Keywords.JSON.equals(keyTermDefinition.getTypeMapping())) {
				
				expandedValue = Json.createObjectBuilder()
									.add(Keywords.VALUE, value)
									.add(Keywords.TYPE, Json.createValue(Keywords.JSON))
									.build();
				
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

			// 13.12.
			if (containerMapping.contains(Keywords.GRAPH) 
					&& !containerMapping.contains(Keywords.ID)
					&& !containerMapping.contains(Keywords.INDEX)
					) {
				
				if (JsonUtils.isNotArray(expandedValue)) {
					expandedValue = Json.createArrayBuilder().add(expandedValue).build();
				}
				
				JsonArrayBuilder array = Json.createArrayBuilder();

				for (JsonValue ev : expandedValue.asJsonArray()) {
					array.add(Json.createObjectBuilder().add(Keywords.GRAPH, Json.createArrayBuilder().add(ev)));
				}
				
				expandedValue = array.build();				
			}

			// 13.13.
			if (keyTermDefinition != null && keyTermDefinition.isReverseProperty()) {

				// 13.13.1.
				if (!result.containsKey(Keywords.REVERSE)) {
					result.put(Keywords.REVERSE, Json.createObjectBuilder().build());
				}
				
				// 13.13.2.
				//TODO

				// 13.13.3.
				if (JsonUtils.isNotArray(expandedValue)) {
					expandedValue = Json.createArrayBuilder().add(expandedValue).build();
				}
				
				// 13.13.4.
				for (JsonValue item : expandedValue.asJsonArray()) {
					
					if (ListObject.isListObject(item) || ValueObject.isValueObject(item)) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
					}

					//TODO
					
				}
				
			// 13.14				
			} else {
				addValue(result, expandedProperty, expandedValue, true);
			}
		}
		
		// 14.
		List<String> nestKeys = new ArrayList<>(nest.keySet());
		
		if (ordered) {
			Collections.sort(nestKeys);
		}
		
		for (String nestKey : nestKeys) {
			//TODO
		}

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
						&& (!ValueType.STRING.equals(type.getValueType())
						|| UriUtils.isNotURI(((JsonString)type).getString()))
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
		
		final JsonObjectBuilder resultBuilder = Json.createObjectBuilder();

		result.entrySet()
				.stream()
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
