package com.apicatalog.jsonld.alg;

import java.net.URL;

import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdContext;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>
 *
 */
public class Expansion {

	public static final JsonValue expand(JsonLdContext context, JsonValue element, String property, URL baseUrl) {

		final ValueType elementType = element.getValueType();
		
		// 1. If element is null, return null.
		if (ValueType.NULL.equals(elementType)) {
			return element;
		}
		
		// 2. If active property is @default, initialize the frameExpansion flag to false.
		//TODO
		
		// 3. If active property has a term definition in active context with a local context, 
		//    initialize property-scoped context to that local context.
		//TODO
		
		// 4. If element is a scalar
		if (ValueType.STRING.equals(elementType)
			|| ValueType.NUMBER.equals(elementType)
			|| ValueType.TRUE.equals(elementType)
			|| ValueType.FALSE.equals(elementType)
			) {

			// 4.1. If active property is null or @graph, drop the free-floating scalar by returning null.
			//TODO
			
			/* 
			 * 4.2. If property-scoped context is defined, set active context 
			 *		to the result of the Context Processing algorithm, passing 
			 *		active context, property-scoped context as local context, 
			 *		and base URL from the term definition for active property in active context.
			 */
			//TODO 
			
			/*
			 * 4.3. Return the result of the Value Expansion algorithm, passing 
			 * 		the active context, active property, and element as value. 
			 */
			return ValueExpansion.expand(context, element, property);
		}
		
		// 5. If element is an array,
		if (ValueType.ARRAY.equals(elementType)) {
			//TODO
		}
		
		// 6. Otherwise element is a map
		
		
		return element;
	}
	
}
