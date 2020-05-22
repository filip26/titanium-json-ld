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
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.DefaultObject;
import com.apicatalog.jsonld.grammar.DirectionType;
import com.apicatalog.jsonld.grammar.GraphObject;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.ListObject;
import com.apicatalog.jsonld.grammar.NodeObject;
import com.apicatalog.jsonld.grammar.ValueObject;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.utils.JsonUtils;

/**
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
final class MapExpansion1314 {

	// mandatory
	private final ActiveContext activeContext;
	private final JsonObject element;
	private final String activeProperty;
	private final URI baseUrl;

	private ActiveContext typeContext;
	private Map<String, JsonValue> result;
	private String inputType;
	private Map<String, JsonValue> nest;

	// optional
	private boolean frameExpansion;
	private boolean ordered;
	private LoadDocumentCallback documentLoader;

	private MapExpansion1314(final ActiveContext activeContext, final JsonObject element,
			final String activeProperty, final URI baseUrl) {
		this.activeContext = activeContext;
		this.element = element;
		this.activeProperty = activeProperty;
		this.baseUrl = baseUrl;

		// default values
		this.frameExpansion = false;
		this.ordered = false;
	}

	public static final MapExpansion1314 with(final ActiveContext activeContext, final JsonObject element,
			final String activeProperty, final URI baseUrl) {
		return new MapExpansion1314(activeContext, element, activeProperty, baseUrl);
	}

	public MapExpansion1314 frameExpansion(boolean value) {
		this.frameExpansion = value;
		return this;
	}

	public MapExpansion1314 ordered(boolean value) {
		this.ordered = value;
		return this;
	}

	public MapExpansion1314 nest(Map<String, JsonValue> nest) {
		this.nest = nest;
		return this;
	}

	public MapExpansion1314 typeContext(ActiveContext typeContext) {
		this.typeContext = typeContext;
		return this;
	}

	public MapExpansion1314 result(Map<String, JsonValue> result) {
		this.result = result;
		return this;
	}

	public MapExpansion1314 inputType(String inputType) {
		this.inputType = inputType;
		return this;
	}

	public MapExpansion1314 documentLoader(LoadDocumentCallback documentLoader) {
		this.documentLoader = documentLoader;
		return this;
	}

	public void compute() throws JsonLdError {

		// 13.
		List<String> keys = new ArrayList<>(element.keySet());

		if (ordered) {
			Collections.sort(keys);
		}

		for (final String key : keys) {

			// 13.1.
			if (Keywords.CONTEXT.equals(key)) {
				continue;
			}

			// 13.2.
			String expandedProperty = UriExpansionBuilder.with(activeContext, key).documentRelative(false).vocab(true)
					.build();

			// 13.3.
			if (expandedProperty == null || (!expandedProperty.contains(":") && !Keywords.contains(expandedProperty))) {
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
				if (result.containsKey(expandedProperty)
						&& Keywords.isNot(expandedProperty, Keywords.INCLUDED, Keywords.TYPE)) {
					throw new JsonLdError(JsonLdErrorCode.COLLIDING_KEYWORDS);
				}

				// 13.4.3
				if (Keywords.ID.equals(expandedProperty)) {

					// 13.4.3.1
					if (JsonUtils.isNotString(value)) {
						// TODO frameExpansion
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ID_VALUE);

						// 13.4.3.2
					} else {

						String expandedStringValue = UriExpansionBuilder
								.with(activeContext, ((JsonString) value).getString()).documentRelative(true)
								.vocab(false).build();

						if (expandedStringValue != null) {
							expandedValue = Json.createValue(expandedStringValue);
						}
						// TODO frameExpansion
					}
				}

				// 13.4.4
				if (Keywords.TYPE.equals(expandedProperty)) {

					// 13.4.4.1
					if (((!frameExpansion && JsonUtils.isNotString(value) && JsonUtils.isNotArray(value)))
							|| (frameExpansion && !JsonUtils.isEmptyObject(value) && JsonUtils.isNotString(value)
									&& JsonUtils.isNotArray(value) && !DefaultObject.isDefaultObject(value)
							/* TODO default object */
							)) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_VALUE);

					}

					// 13.4.4.2
					if (JsonUtils.isEmptyObject(value)) {
						expandedValue = value;

						// 13.4.4.3
					} else if (DefaultObject.isDefaultObject(value)) {

						value = DefaultObject.getValue(value);

						expandedValue = Json.createObjectBuilder()
								.add(Keywords.DEFAULT,
										UriExpansionBuilder.with(typeContext, ((JsonString) value).getString())
												.vocab(true).documentRelative(true).build())
								.build();

					// 13.4.4.4
					} else {

						if (JsonUtils.isString(value)) {

							String expandedStringValue = UriExpansionBuilder
									.with(typeContext, ((JsonString) value).getString()).vocab(true)
									.documentRelative(true).build();

							if (expandedStringValue != null) {
								expandedValue = Json.createValue(expandedStringValue);
							}

						} else if (JsonUtils.isArray(value)) {

							JsonArrayBuilder array = Json.createArrayBuilder();

							for (JsonValue item : value.asJsonArray()) {

								if (JsonUtils.isString(item)) {

									String expandedStringValue = UriExpansionBuilder
											.with(typeContext, ((JsonString) item).getString()).vocab(true)
											.documentRelative(true).build();

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

						JsonValue typeValue = result.get(Keywords.TYPE);

						if (JsonUtils.isArray(typeValue)) {
							expandedValue = Json.createArrayBuilder(typeValue.asJsonArray()).add(expandedValue).build();

						} else {
							expandedValue = Json.createArrayBuilder().add(typeValue).add(expandedValue).build();
						}
					}
				}

				// 13.4.5
				if (Keywords.GRAPH.equals(expandedProperty)) {

					expandedValue = Expansion.with(typeContext, value, Keywords.GRAPH, baseUrl)
							.documentLoader(documentLoader).frameExpansion(frameExpansion).ordered(ordered).compute();

					if (JsonUtils.isObject(expandedValue)) {
						expandedValue = Json.createArrayBuilder().add(expandedValue).build();
					}
				}

				// 13.4.6
				if (Keywords.INCLUDED.equals(expandedProperty)) {

					// 13.4.6.1
					if (activeContext.inMode(Version.V1_0)) {
						continue;
					}

					// 13.4.6.2
					expandedValue = Expansion.with(activeContext, value, null, baseUrl).documentLoader(documentLoader)
							.frameExpansion(frameExpansion).ordered(ordered).compute();

					if (JsonUtils.isNotNull(expandedValue)) {

						if (JsonUtils.isNotArray(expandedValue)) {
							expandedValue = Json.createArrayBuilder().add(expandedValue).build();
						}

						// 13.4.6.3
						if (!expandedValue.asJsonArray().stream().allMatch(NodeObject::isNodeObject)) {
							throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
						}

						// 13.4.6.4
						if (result.containsKey(Keywords.INCLUDED)) {

							JsonArrayBuilder includes = Json
									.createArrayBuilder(result.get(Keywords.INCLUDED).asJsonArray());

							expandedValue.asJsonArray().stream().forEach(includes::add);

							expandedValue = includes.build();
						}
					} else {
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
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
					// TODO frameexpansion
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
					if (JsonUtils.isNotString(value)) {
						// TODO frameExpansion
						throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
					}

					// 13.4.8.2
					expandedValue = value;
					// TODO validation, warning, frameExpansion
				}

				// 13.4.9.
				if (Keywords.DIRECTION.equals(expandedProperty)) {
					// 13.4.9.1.
					if (activeContext.inMode(Version.V1_0)) {
						continue;
					}

					// 13.4.9.2.
					if (JsonUtils.isNotString(value) && !((JsonString) value).getString().equals("ltr")
							&& !((JsonString) value).getString().equals("rtl")) {
						// TODO frameexpansion
						throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
					}

					// 13.4.9.3.
					expandedValue = value;
					// TODO frameexpansion
				}

				// 13.4.10.
				if (Keywords.INDEX.equals(expandedProperty)) {

					// 13.4.10.1.
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
					expandedValue = Expansion.with(activeContext, value, activeProperty, baseUrl)
							.documentLoader(documentLoader).frameExpansion(frameExpansion).ordered(ordered).compute();

					if (JsonUtils.isNotArray(expandedValue)) {
						expandedValue = Json.createArrayBuilder().add(expandedValue).build();
					}
				}

				// 13.4.12
				if (Keywords.SET.equals(expandedProperty)) {

					expandedValue = Expansion
										.with(activeContext, value, activeProperty, baseUrl)
										.documentLoader(documentLoader)
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
										.documentLoader(documentLoader)
										.frameExpansion(frameExpansion)
										.ordered(ordered)
										.compute();

					if (JsonUtils.isObject(expandedValue)) {

						// 13.4.13.3.
						if (expandedValue.asJsonObject().containsKey(Keywords.REVERSE)) {

							for (Entry<String, JsonValue> entry : expandedValue.asJsonObject().entrySet()) {
								// 13.4.13.3.1.
								MapExpansion.addValue(result, entry.getKey(), entry.getValue(), true);
							}
						}

						// 13.4.13.4.
						if (expandedValue.asJsonObject().size() > 1
								|| !expandedValue.asJsonObject().containsKey(Keywords.REVERSE)) {

							Map<String, JsonValue> reverseMap = null;
							
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
										MapExpansion.addValue(reverseMap, entry.getKey(), item, true);
									}
								}
							}
							result.put(Keywords.REVERSE, JsonUtils.toObject(reverseMap));
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
				if (frameExpansion && (Keywords.DEFAULT.equals(expandedProperty) || "@embed".equals(expandedProperty)
						|| "@explicit".equals(expandedProperty) || "@omitDefault".equals(expandedProperty)
						|| "@requireAll)".equals(expandedProperty))) {

					System.out.println("TODO " + expandedProperty + ", " + expandedValue);
				}

				// 13.4.16
				if (JsonUtils.isNotNull(expandedValue)
						|| (Keywords.VALUE.equals(expandedProperty) && Keywords.JSON.equals(inputType))) {

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

				expandedValue = Json.createObjectBuilder().add(Keywords.VALUE, value)
						.add(Keywords.TYPE, Json.createValue(Keywords.JSON)).build();

				// 13.7.
			} else if (containerMapping.contains(Keywords.LANGUAGE) && JsonUtils.isObject(value)) {

				// 13.7.1.
				expandedValue = Json.createArrayBuilder().build();

				// 13.7.2.
				DirectionType direction = activeContext.getDefaultBaseDirection();

				// 13.7.3.
				if (keyTermDefinition != null && keyTermDefinition.getDirectionMapping() != null) {
					direction = keyTermDefinition.getDirectionMapping();
				}

				// 13.7.4.
				List<String> langCodes = new ArrayList<>(value.asJsonObject().keySet());

				if (ordered) {
					Collections.sort(langCodes);
				}
				Collections.sort(langCodes); // FIXME ?!

				for (String langCode : langCodes) {

					JsonValue langValue = value.asJsonObject().get(langCode);

					// 13.7.4.1.
					if (JsonUtils.isNotArray(langValue)) {
						langValue = Json.createArrayBuilder().add(langValue).build();
					}

					// 13.7.4.2.
					for (JsonValue item : langValue.asJsonArray()) {

						// 13.7.4.2.1.
						if (JsonUtils.isNull(item)) {
							continue;
						}

						// 13.7.4.2.2.
						if (JsonUtils.isNotString(item)) {
							throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_MAP_VALUE);
						}

						// 13.7.4.2.3.
						JsonObjectBuilder langMap = Json.createObjectBuilder().add(Keywords.VALUE, item);

						if (Keywords.NONE.equals(((JsonString) item).getString())) {
							// TODO warning
						}

						// 13.7.4.2.4.
						if (!Keywords.NONE.equals(langCode)) {

							String expandedLangCode = activeContext.expandUri(langCode).vocab(true).build();

							if (!Keywords.NONE.equals(expandedLangCode)) {
								langMap.add(Keywords.LANGUAGE, Json.createValue(langCode));
							}
						}

						// 13.7.4.2.5.
						if (direction != null && !DirectionType.NULL.equals(direction)) {
							langMap.add(Keywords.DIRECTION, Json.createValue(direction.name().toLowerCase()));
						}

						// 13.7.4.2.6.
						expandedValue = Json.createArrayBuilder(expandedValue.asJsonArray()).add(langMap).build();
					}
				}

				// 13.8.
			} else if ((containerMapping.contains(Keywords.INDEX) || containerMapping.contains(Keywords.TYPE)
					|| containerMapping.contains(Keywords.ID)) && JsonUtils.isObject(value)) {

				// 13.8.1.
				expandedValue = Json.createArrayBuilder().build();

				// 13.8.2.
				String indexKey = null;

				if (keyTermDefinition != null) {
					indexKey = keyTermDefinition.getIndexMapping();
				}

				if (indexKey == null) {
					indexKey = Keywords.INDEX;
				}

				// 13.8.3.
				List<String> indicies = new ArrayList<>(value.asJsonObject().keySet());

				if (ordered) {
					Collections.sort(indicies);
				}

				for (String index : indicies) {

					JsonValue indexValue = value.asJsonObject().get(index);

					// 13.8.3.1.
					ActiveContext mapContext = activeContext;

					if (activeContext.hasPreviousContext()
							&& (containerMapping.contains(Keywords.ID) || containerMapping.contains(Keywords.TYPE))) {

						mapContext = activeContext.getPreviousContext();
					}

					// 13.8.3.2.
					TermDefinition indexTermDefinition = activeContext.getTerm(index);

					if (containerMapping.contains(Keywords.TYPE) && indexTermDefinition != null
							&& indexTermDefinition.hasLocalContext()) {

						mapContext = mapContext.create(indexValue, indexTermDefinition.getBaseUrl()).build();
					}

					// 13.8.3.3.
					if (mapContext == null) {
						mapContext = activeContext;
					}

					// 13.8.3.4.
					String expandedIndex = activeContext.expandUri(index).vocab(true).build();

					// 13.8.3.5.
					if (JsonUtils.isNotArray(indexValue)) {
						indexValue = Json.createArrayBuilder().add(indexValue).build();
					}

					// 13.8.3.6.
					indexValue = Expansion.with(mapContext, indexValue, key, baseUrl).fromMap(true)
							.frameExpansion(frameExpansion).ordered(ordered).compute();

					// 13.8.3.7.
					for (JsonValue item : indexValue.asJsonArray()) {

						// 13.8.3.7.1.
						if (containerMapping.contains(Keywords.GRAPH) && !GraphObject.isGraphObject(item)) {
							item = GraphObject.toGraphObject(item);
						}

						// 13.8.3.7.2.
						if (containerMapping.contains(Keywords.INDEX) && !Keywords.INDEX.equals(indexKey)
								&& !Keywords.NONE.equals(expandedIndex)) {

							// 13.8.3.7.2.1.
							JsonValue reExpandedIndex = activeContext.expandValue(Json.createValue(index), indexKey)
									.build();

							// 13.8.3.7.2.2.
							String expandedIndexKey = activeContext.expandUri(indexKey).vocab(true).build();

							// 13.8.3.7.2.3.
							JsonArrayBuilder indexPropertyValues = Json.createArrayBuilder().add(reExpandedIndex);

							JsonValue existingValues = item.asJsonObject().get(expandedIndexKey);

							if (JsonUtils.isNotNull(existingValues)) {
								indexPropertyValues.add(existingValues);
							}

							// 13.8.3.7.2.4.
							item = Json.createObjectBuilder(item.asJsonObject())
									.add(expandedIndexKey, indexPropertyValues).build();

							// 13.8.3.7.2.5.
							if (ValueObject.isValueObject(item) && item.asJsonObject().size() > 1) {
								throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
							}

							// 13.8.3.7.3.
						} else if (containerMapping.contains(Keywords.INDEX)
								&& !item.asJsonObject().containsKey(Keywords.INDEX)
								&& !Keywords.NONE.equals(expandedIndex)) {

							item = Json.createObjectBuilder(item.asJsonObject()).add(Keywords.INDEX, index).build();

							// 13.8.3.7.4.
						} else if (containerMapping.contains(Keywords.ID)
								&& !item.asJsonObject().containsKey(Keywords.ID)
								&& !Keywords.NONE.equals(expandedIndex)) {

							expandedIndex = activeContext.expandUri(index).vocab(false).documentRelative(true).build();

							item = Json.createObjectBuilder(item.asJsonObject()).add(Keywords.ID, expandedIndex)
									.build();

							// 13.8.3.7.5.
						} else if (containerMapping.contains(Keywords.TYPE) && !Keywords.NONE.equals(expandedIndex)) {

							JsonArrayBuilder types = Json.createArrayBuilder().add(expandedIndex);

							JsonValue existingType = item.asJsonObject().get(Keywords.TYPE);
							if (JsonUtils.isNotNull(existingType)) {

								if (JsonUtils.isArray(existingType)) {

									existingType.asJsonArray().stream().forEach(types::add);

								} else {
									types.add(existingType);
								}
							}

							item = Json.createObjectBuilder(item.asJsonObject()).add(Keywords.TYPE, types).build();

						}
						// 13.8.3.7.6.
						expandedValue = Json.createArrayBuilder(expandedValue.asJsonArray()).add(item).build();
					}
				}

				// 13.9.
			} else {

				expandedValue = Expansion.with(activeContext, value, key, baseUrl).documentLoader(documentLoader)
						.frameExpansion(frameExpansion).ordered(ordered).compute();

			}

			// 13.10.
			if (JsonUtils.isNull(expandedValue)) {
				continue;
			}

			// 13.11.
			if (containerMapping.contains(Keywords.LIST) && !ListObject.isListObject(expandedValue)) {
				expandedValue = ListObject.toListObject(expandedValue);
			}

			// 13.12.
			if (containerMapping.contains(Keywords.GRAPH) && !containerMapping.contains(Keywords.ID)
					&& !containerMapping.contains(Keywords.INDEX)) {

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

				Map<String, JsonValue> reverseMap = null;


				if (reverseMap == null) {
					reverseMap = new LinkedHashMap<>();
				}

				// 13.13.1.

				// 13.13.2.

				// 13.13.3.
				if (JsonUtils.isNotArray(expandedValue)) {
					expandedValue = Json.createArrayBuilder().add(expandedValue).build();
				}

				// 13.13.4.
				for (JsonValue item : expandedValue.asJsonArray()) {

					// 13.13.4.1.
					if (ListObject.isListObject(item) || ValueObject.isValueObject(item)) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
					}

					// 13.13.4.2.
					if (!reverseMap.containsKey(expandedProperty)) {
						reverseMap.put(expandedProperty, Json.createArrayBuilder().build());
					}

					// 13.13.4.3.
					MapExpansion.addValue(reverseMap, expandedProperty, item, true);
				}

				result.put(Keywords.REVERSE, JsonUtils.toObject(reverseMap));

			// 13.14
			} else {
				MapExpansion.addValue(result, expandedProperty, expandedValue, true);
			}
		}

		// 14.
		List<String> nestedKeys = new ArrayList<>(nest.keySet());

		if (ordered) {
			Collections.sort(nestedKeys);
		}

		for (String nestedKey : nestedKeys) {

			// 14.1.
			JsonValue nestedValues = element.get(nestedKey);

			if (JsonUtils.isNotArray(nestedValues)) {
				nestedValues = Json.createArrayBuilder().add(nestedValues).build();
			}

			// 14.2.
			for (JsonValue nestValue : nestedValues.asJsonArray()) {

				// 14.2.1
				if (JsonUtils.isNotObject(nestValue)) {
					throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
				}

				for (String nestedValueKey : nestValue.asJsonObject().keySet()) {

					String expandedNestedValueKey = UriExpansionBuilder.with(typeContext, nestedValueKey).vocab(true)
							.build();

					if (Keywords.VALUE.equals(expandedNestedValueKey)) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
					}
				}

				// 14.2.2
				MapExpansion1314.with(activeContext, nestValue.asJsonObject(), activeProperty, baseUrl)
						.documentLoader(documentLoader).inputType(inputType).result(result).typeContext(typeContext)
						.nest(new LinkedHashMap<>()).frameExpansion(frameExpansion).ordered(ordered).compute();
			}
		}

	}
}
