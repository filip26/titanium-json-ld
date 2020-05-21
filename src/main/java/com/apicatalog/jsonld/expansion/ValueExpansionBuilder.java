package com.apicatalog.jsonld.expansion;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.DirectionType;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.utils.JsonUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value Expansion Algorithm</a>
 *
 */
public final class ValueExpansionBuilder {

	// mandatory
	private ActiveContext activeContext;
	private String activeProperty;
	private JsonValue value;
	
	public ValueExpansionBuilder(final ActiveContext activeContext, final JsonValue value, final String activeProperty) {
		this.activeContext = activeContext;
		this.value = value;
		this.activeProperty = activeProperty;
	}
	
	public static final ValueExpansionBuilder with(final ActiveContext activeContext, final JsonValue element, final String activeProperty) {
		return new ValueExpansionBuilder(activeContext, element, activeProperty);
	}
	
	public JsonValue build() throws JsonLdError {

		final TermDefinition definition = activeContext.getTerm(activeProperty);

		final String typeMapping = (definition != null) ? definition.getTypeMapping() : null;

		if (typeMapping != null) {

			// 1.
			if (Keywords.ID.equals(typeMapping) && JsonUtils.isString(value)) {
				
				String expandedValue = activeContext
											.expandUri(((JsonString)value).getString())
											.documentRelative(true)
											.vocab(false)
											.build();
				
				return Json.createObjectBuilder().add(Keywords.ID, expandedValue).build();
			}
			
			// 2.
			if (Keywords.VOCAB.equals(typeMapping) && JsonUtils.isString(value)) {
				
				String expandedValue = activeContext
											.expandUri(((JsonString)value).getString())
											.documentRelative(true)
											.vocab(true)
											.build();
				
				return Json.createObjectBuilder().add(Keywords.ID, expandedValue).build();
			}
		}
		
		// 3.
		JsonObject result = Json.createObjectBuilder().add(Keywords.VALUE, value).build();
		
		// 4.
		if (typeMapping != null
			&& !Keywords.ID.equals(typeMapping)
			&& !Keywords.VOCAB.equals(typeMapping)
			&& !Keywords.NONE.equals(typeMapping)
				) {
			
			result = Json.createObjectBuilder(result).add(Keywords.TYPE, Json.createValue(typeMapping)).build();
			
		// 5.
		} else if (JsonUtils.isString(value)) {
			
			// 5.1.
			JsonValue language = null;
						
			if (definition != null && definition.getLanguageMapping() != null) {
				language = definition.getLanguageMapping();
				
			} else if (activeContext.getDefaultLanguage() != null) {
				language = Json.createValue(activeContext.getDefaultLanguage());				
			}
					
			// 5.2.
			DirectionType direction = null;

			if (definition != null) {
				direction = definition.getDirectionMapping();
			}
			
			if (direction == null) {
				direction = activeContext.getDefaultBaseDirection();
			}
			
			// 5.3.
			if (JsonUtils.isNotNull(language)) {
				result = Json.createObjectBuilder(result).add(Keywords.LANGUAGE, language).build();	
			}

			// 5.4.
			if (direction != null && !DirectionType.NULL.equals(direction)) {
				result = Json.createObjectBuilder(result).add(Keywords.DIRECTION, Json.createValue(direction.name().toLowerCase())).build();	
			}
		}

		// 6.
		return result;
	}
}
