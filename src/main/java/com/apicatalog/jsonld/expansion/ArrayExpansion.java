package com.apicatalog.jsonld.expansion;

import java.net.URI;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.ListObject;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.utils.JsonUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>
 *
 */
public final class ArrayExpansion {

	// mandatory
	private ActiveContext activeContext; 
	private JsonArray element;
	private String activeProperty; 
	private URI baseUrl;
	
	// optional
	private boolean frameExpansion;
	private boolean ordered;
	private boolean fromMap;
	private LoadDocumentCallback documentLoader;
	
	private ArrayExpansion(final ActiveContext activeContext, final JsonArray element, final String activeProperty, final URI baseUrl) {
		this.activeContext = activeContext;
		this.element = element;
		this.activeProperty = activeProperty;
		this.baseUrl = baseUrl;
		
		// default values
		this.frameExpansion = false;
		this.ordered = false;
		this.fromMap = false;
	}
	
	public static final ArrayExpansion with(final ActiveContext activeContext, final JsonArray element, final String activeProperty, final URI baseUrl) {
		return new ArrayExpansion(activeContext, element, activeProperty, baseUrl);
	}

	public ArrayExpansion frameExpansion(boolean value) {
		this.frameExpansion = value;
		return this;
	}
	
	public ArrayExpansion ordered(boolean value) {
		this.ordered = value;
		return this;
	}
	
	public ArrayExpansion fromMap(boolean value) {
		this.fromMap = value;
		return this;
	}
	
	public ArrayExpansion documentLoader(LoadDocumentCallback documentLoader) {
		this.documentLoader = documentLoader;
		return this;
	}

	public JsonValue compute() throws JsonLdError {

		// 5.1
		final JsonArrayBuilder result = Json.createArrayBuilder();

		// 5.2.
		for (final JsonValue item : element) {

			// 5.2.1
			JsonValue expanded = Expansion
										.with(activeContext, item, activeProperty, baseUrl)
										.documentLoader(documentLoader)
										.frameExpansion(frameExpansion)
										.ordered(ordered)
										.fromMap(fromMap)
										.compute();
			
			TermDefinition definition = activeContext.getTerm(activeProperty);
						
			// 5.2.2
			if (definition != null 
					&& definition.getContainerMapping() != null 
					&& definition.getContainerMapping().contains(Keywords.LIST)
					&& JsonUtils.isArray(expanded)
					) {
				
				expanded = ListObject.toListObject(expanded);	
			}
			
			// 5.2.3
			if (JsonUtils.isArray(expanded)) {

				// append array
				for (JsonValue expandedItem : expanded.asJsonArray()) {
					
					if (JsonUtils.isNull(expandedItem)) {
						continue;
					}

					result.add(expandedItem);
				}
				
			// append non-null element
			} else if (JsonUtils.isNotNull(expanded)) {
				result.add(expanded);
			}
		}

		// 5.3
		return result.build();
	}	
}
