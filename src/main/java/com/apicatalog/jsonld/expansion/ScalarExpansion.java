package com.apicatalog.jsonld.expansion;

import java.net.URL;

import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.ContextProcessor;

/**
 * Steps 4.1 - 4.3
 *  
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>
 *
 */
public final class ScalarExpansion {

	// mandatory
	private ActiveContext activeContext;
	private JsonValue propertyContext;
	private JsonValue element;
	private String activeProperty; 
	private URL baseUrl;
	
	private ScalarExpansion(final ActiveContext activeContext, final JsonValue propertyContext, final JsonValue element, final String activeProperty, final URL baseUrl) {
		this.activeContext = activeContext;
		this.propertyContext = propertyContext;
		this.element = element;
		this.activeProperty = activeProperty;
		this.baseUrl = baseUrl;		
	}
	
	public static final ScalarExpansion with(final ActiveContext activeContext, final JsonValue propertyContext, final JsonValue element, final String activeProperty, final URL baseUrl) {
		return new ScalarExpansion(activeContext, propertyContext, element, activeProperty, baseUrl);
	}
	
	public JsonValue compute() throws JsonLdError {

		// 4.1. If active property is null or @graph, drop the free-floating scalar by returning null.
		if (activeProperty == null || "@graph".equals(activeProperty)) {
			return JsonValue.NULL;
		}
		
		/* 
		 * 4.2. If property-scoped context is defined, set active context 
		 *		to the result of the Context Processing algorithm, passing 
		 *		active context, property-scoped context as local context, 
		 *		and base URL from the term definition for active property in active context.
		 */
		if (propertyContext != null) {	
			activeContext = ContextProcessor.with(activeContext, propertyContext, baseUrl).compute();		
		}
		
		/*
		 * 4.3. Return the result of the Value Expansion algorithm, passing 
		 * 		the active context, active property, and element as value. 
		 */
		return ValueExpansion.with(activeContext, element, activeProperty).compute();		
	}
}
