package com.apicatalog.jsonld.expansion;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
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
		
		// 1.
		//TODO
		
		// 2.
		//TODO
		
		// 3.
		JsonObject result = Json.createObjectBuilder().add(Keywords.VALUE, value).build();
		
		// 4.
		//TODO
		
		// 5.
		if (ValueType.STRING.equals(value.getValueType())) {
			//TODO
		}

		// 6.
		return result;
	}
	
}
