package com.apicatalog.jsonld.expansion;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.utils.JsonUtils;
import com.apicatalog.jsonld.utils.UriUtils;

/**
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
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

	private MapExpansion(final ActiveContext activeContext, final JsonValue propertyContext, final JsonObject element,
			final String activeProperty, final URI baseUrl) {
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

	public static final MapExpansion with(final ActiveContext activeContext, final JsonValue propertyContext,
			final JsonObject element, final String activeProperty, final URI baseUrl) {
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

		// 7. If active context has a previous context, the active context is not
		// propagated.
		// If from map is undefined or false, and element does not contain an entry
		// expanding to @value,
		// and element does not consist of a single entry expanding to @id (where
		// entries are IRI expanded),
		// set active context to previous context from active context,
		// as the scope of a term-scoped context does not apply when processing new node
		// objects.
		if (activeContext.hasPreviousContext() && !fromMap) {

			List<String> keys = new ArrayList<>(element.keySet());
			Collections.sort(keys);

			boolean revert = true;

			for (String key : keys) {

				String expandedKey = 
							activeContext
								.expandUri(key)
								.vocab(true)
								.build();

				if (Keywords.VALUE.equals(expandedKey) || (Keywords.ID.equals(expandedKey) && (element.size() == 1))) {
					revert = false;
					break;
				}
			}

			if (revert) {
				activeContext = activeContext.getPreviousContext();
			}
		}

		// 8.
		if (propertyContext != null) {
//			TermDefinition activePropertyDefinition = activeContext.getTerm(activeProperty);

			activeContext = activeContext
								.create(propertyContext, baseUrl/* TODO activePropertyDefinition.getBaseUrl() */)
								.overrideProtected(true)
								.build();
		}

		// 9.
		if (element.containsKey(Keywords.CONTEXT)) {

			activeContext = activeContext
								.create(element.get(Keywords.CONTEXT), baseUrl)
								.build();
		}

		// 10.
		ActiveContext typeContext = activeContext;

		List<String> keys = new ArrayList<>(element.keySet());

		String typeKey = null;

		// 11.
		for (String key : keys) {

			String expandedKey = 
						activeContext
							.expandUri(key)
							.vocab(true)
							.build();

			if (!Keywords.TYPE.equals(expandedKey)) {
				continue;

			} else if (typeKey == null) {
				typeKey = key;
			}

			JsonValue value = element.get(key);

			// 11.1
			if (JsonUtils.isNotArray(value)) {
				value = Json.createArrayBuilder().add(value).build();
			}

			// 11.2
			List<String> terms = value.asJsonArray().stream().filter(JsonUtils::isString).map(JsonString.class::cast)
					.map(JsonString::getString).sorted().collect(Collectors.toList());

			for (String term : terms) {

				if (typeContext.containsTerm(term)) {

					TermDefinition termDefinition = typeContext.getTerm(term);

					TermDefinition activeTermDefinition = activeContext.getTerm(term);

					if (termDefinition.hasLocalContext() && activeTermDefinition != null) {
						activeContext = 
								activeContext
									.create(termDefinition.getLocalContext(), activeTermDefinition.getBaseUrl())
									.propagate(false)
									.build();
					}
				}
			}
		}

		// 12.
		Map<String, JsonValue> result = new LinkedHashMap<>();

		String inputType = null;

		// Initialize input type to expansion of the last value of the first entry in
		// element
		// expanding to @type (if any), ordering entries lexicographically by key. Both
		// the key and
		// value of the matched entry are IRI expanded.
		if (typeKey != null) {

			JsonValue t = element.get(typeKey);

			String lastValue = null;

			if (JsonUtils.isArray(t)) {

				List<String> sortedValues = new ArrayList<>(t.asJsonArray().stream().filter(JsonUtils::isString)
						.map(JsonString.class::cast).map(JsonString::getString).sorted().collect(Collectors.toList()));

				if (!sortedValues.isEmpty()) {
					lastValue = sortedValues.get(sortedValues.size() - 1);
				}
			}

			if (JsonUtils.isString(t)) {
				lastValue = ((JsonString) t).getString();
			}

			if (lastValue != null) {

				inputType = activeContext
								.expandUri(lastValue)
								.vocab(true)
								.build();
			}
		}

		MapExpansion1314
					.with(activeContext, element, activeProperty, baseUrl)
					.inputType(inputType)
					.result(result)
					.typeContext(typeContext)
					.nest(new LinkedHashMap<>())
					.frameExpansion(frameExpansion)
					.ordered(ordered)
					.compute();

		// 15.
		if (result.containsKey(Keywords.VALUE)) {

			// 15.1.
			if (!Keywords.allIsOneOf(result.keySet(), Keywords.DIRECTION, Keywords.INDEX, Keywords.LANGUAGE,
					Keywords.TYPE, Keywords.VALUE)) {

				throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
			}
			if ((result.keySet().contains(Keywords.DIRECTION) || result.keySet().contains(Keywords.LANGUAGE))
					&& result.keySet().contains(Keywords.TYPE)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
			}

			// 15.2.
			JsonValue type = result.get(Keywords.TYPE);

			JsonValue value = result.get(Keywords.VALUE);

			if (JsonUtils.contains(Keywords.JSON, type)) {

				// 15.3.
			} else if (JsonUtils.isNull(value) || (JsonUtils.isArray(value) && value.asJsonArray().isEmpty())) {
				return JsonValue.NULL;

				// 15.4
			} else if (JsonUtils.isNotString(value) && result.containsKey(Keywords.LANGUAGE)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_VALUE);

				// 15.5
			} else if (result.containsKey(Keywords.TYPE)
					&& (JsonUtils.isNotString(type) || UriUtils.isNotURI(((JsonString) type).getString()))) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TYPED_VALUE);
			}

			// 16. Otherwise, if result contains the entry @type and its associated value is
			// not an array,
			// set it to an array containing only the associated value.
		} else if (result.containsKey(Keywords.TYPE)) {

			final JsonValue value = result.get(Keywords.TYPE);

			if (JsonUtils.isNotArray(value) && JsonUtils.isNotNull(value)) {
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

				if (JsonUtils.isNotObject(set)) {
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

			// 19.1. If result is a map which is empty, or contains only the entries @value
			// or @list, set result to null
			if (result.isEmpty()) {
				return JsonValue.NULL;
			}

			if (result.size() == 1 && (result.containsKey(Keywords.VALUE) || result.containsKey(Keywords.LIST))) {
				return JsonValue.NULL;
			}

			if (result.size() == 2 && result.containsKey(Keywords.VALUE) && result.containsKey(Keywords.LIST)) {
				return JsonValue.NULL;
			}

			// 19.2. if result is a map whose only entry is @id, set result to null. When
			// the frameExpansion flag is set, a map containing only the @id entry is
			// retained.
			if (result.size() == 1 && result.containsKey(Keywords.ID) && !frameExpansion) {
				return JsonValue.NULL;
			}

		}

		return JsonUtils.toObject(result);
	}

	// TODO don't use this algorithm, easy reduce complexity
	public static void addValue(Map<String, JsonValue> object, String key, JsonValue value, boolean asArray) {
		
		// 1. If as array is true and the value of key in object does not exist or is
		// not an array,
		// set it to a new array containing any original value.
		if (asArray) {

			if (!object.containsKey(key)) {
				object.put(key, Json.createArrayBuilder().build());

			} else {

				JsonValue original = object.get(key);

				if (JsonUtils.isNotArray(original)) {
					object.put(key, Json.createArrayBuilder().add(original).build());
				}
			}
		}

		// 2. If value is an array, then for each element v in value, use add value
		// recursively to add v to key in entry.
		if (JsonUtils.isArray(value)) {

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
				if (JsonUtils.isNotArray(original)) {
					object.put(key, Json.createArrayBuilder().add(original).build());

				// 3.2.2
				} else {
					object.put(key, Json.createArrayBuilder(original.asJsonArray()).add(value).build());
				}
			}
		}
	}
}
