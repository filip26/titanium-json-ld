package com.apicatalog.jsonld.expansion;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.Keywords;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value Expansion Algorithm</a>
 *
 */
public final class ValueExpansion {

	// required
	private ActiveContext activeContext;
	private String activeProperty;
	private JsonValue value;
	
	public ValueExpansion(final ActiveContext activeContext, final JsonValue value, final String activeProperty) {
		this.activeContext = activeContext;
		this.value = value;
		this.activeProperty = activeProperty;
	}
	
	public static final ValueExpansion with(final ActiveContext activeContext, final JsonValue element, final String activeProperty) {
		return new ValueExpansion(activeContext, element, activeProperty);
	}
	
	public JsonValue compute() throws JsonLdError {

		final TermDefinition definition = activeContext.getTerm(activeProperty);

		final String typeMapping = (definition != null) ? definition.getTypeMapping() : null;

		// 1.
		if (Keywords.ID.equals(typeMapping)
				&& ValueType.STRING.equals(value.getValueType())
				) {
			
			String expandedValue = UriExpansion
										.with(activeContext, ((JsonString)value).getString())
										.documentRelative(true)
										.vocab(false)
										.compute()
										.orElse(null);	//FIXME
			
			return Json.createObjectBuilder().add(Keywords.ID, expandedValue).build();
		}
		
		// 2.
		if (Keywords.VOCAB.equals(typeMapping)
				&& ValueType.STRING.equals(value.getValueType())
				) {
			
			String expandedValue = UriExpansion
										.with(activeContext, ((JsonString)value).getString())
										.documentRelative(true)
										.vocab(true)
										.compute()
										.orElse(null);	//FIXME
			
			return Json.createObjectBuilder().add(Keywords.ID, expandedValue).build();
		}
		
		// 3.
		JsonObject result = Json.createObjectBuilder().add(Keywords.VALUE, value).build();
		
		// 4.
		
		//TODO
		
		// 5.
		if (ValueType.STRING.equals(value.getValueType())) {
			// 5.1.
			String language = null;
			if (activeContext.containsTerm(Keywords.LANGUAGE)) {
				language = activeContext.getTerm(Keywords.LANGUAGE).getLanguageMapping();
			}
			if (language == null) {
				language = activeContext.getDefaultLanguage();
			}
								

			//TODO
			//System.out.println(">> " + value + ", " + result + ", " + language);
			

		}

		// 6.
		return result;
	}
	
}
