package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.grammar.Keywords;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#create-term-definition">Create Term Definition</a>
 *
 */
public final class TermDefinitionCreator {

	// mandatory
	ActiveContext activeContext;
	JsonObject localContext;
	String term;
	Map<String, Boolean> defined;
	
	// optional
	URI baseUrl;
	boolean protectedFlag;
	boolean overrideProtectedFlag;
	Collection remoteContexts;
	boolean validateScopedContext;

	private TermDefinitionCreator(ActiveContext activeContext, JsonObject localContext, String term, Map<String, Boolean> defined) {
		this.activeContext = activeContext;
		this.localContext = localContext;
		this.term = term;
		this.defined = defined;
		
		// default values
		this.baseUrl = null;
		this.protectedFlag = false;
		this.overrideProtectedFlag = false;
		this.remoteContexts = new ArrayList<>();
		this.validateScopedContext = true;
	}

	public static final TermDefinitionCreator with(	ActiveContext activeContext, JsonObject localContext, String term, Map<String, Boolean> defined) {
		return new TermDefinitionCreator(activeContext, localContext, term, defined);
	}
	
	public TermDefinitionCreator baseUrl(URI baseUrl) {
		this.baseUrl = baseUrl;
		return this;
	}
	
	public TermDefinitionCreator protectedFlag(boolean protectedFlag) {
		this.protectedFlag = protectedFlag;
		return this;
	}
	
	public TermDefinitionCreator overrideProtectedFlag(boolean overrideProtectedFlag) {
		this.overrideProtectedFlag = overrideProtectedFlag;
		return this;
	}
	
	public TermDefinitionCreator remoteContexts(Collection remoteContexts) {
		this.remoteContexts = remoteContexts;
		return this;
	}

	public TermDefinitionCreator validateScopedContext(boolean validateScopedContext) {
		this.validateScopedContext = validateScopedContext;
		return this;
	}

	public void create() throws JsonLdError {
		
		if (term.isBlank()) {
			throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
		}
		
		// 1.
		if (defined.containsKey(term)) {
			
			if (Boolean.TRUE.equals(defined.get(term))) {
				return;
			}
			
			throw new JsonLdError(JsonLdErrorCode.CYCLIC_IRI_MAPPING);
		}
		
		// 2.
		defined.put(term, Boolean.FALSE);
		
		// 3.
		JsonValue value = localContext.get(term);
		
		// 4.
		//TODO
		
		// 5.
		if (Keywords.contains(term)) {
			throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
		}
		if (Keywords.hasForm(term)) {
			//TODO warning
			return;
		}
		
		// 6.
		TermDefinition previousDefinition = activeContext.removeTerm(term);

		JsonObject valueObject = null;
		boolean simpleTerm = true;

		if (ValueType.NULL.equals(value.getValueType())) {
			// 7.
			valueObject = Json.createObjectBuilder().add(Keywords.ID, JsonValue.NULL).build();
			
		} else if (ValueType.STRING.equals(value.getValueType())) {
			// 8.
			valueObject = Json.createObjectBuilder().add(Keywords.ID, value).build();
			simpleTerm = true;
			
		} else if (ValueType.OBJECT.equals(value.getValueType())) {
			// 9.
			valueObject = value.asJsonObject();
			simpleTerm = false;
			
		} else {
			throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
		}
		
		// 10.
		TermDefinition definition = new TermDefinition(false, protectedFlag, false);
		
		// 11.
		if (valueObject.containsKey(Keywords.PROTECTED)) {
			//TODO
		}
		
		// 12.
		if (valueObject.containsKey(Keywords.TYPE)) {
			//TODO
		}
		
		// 13.
		if (valueObject.containsKey(Keywords.REVERSE)) {
			//TODO
		}
		
		// 14.
		if (valueObject.containsKey(Keywords.ID)) {
			
			JsonValue idValue = valueObject.get(Keywords.ID);
			
			if (!ValueType.STRING.equals(idValue.getValueType()) || !term.equals(idValue.toString())) {
				// 14.1.
				//TODO
				
				
			}
			
		// 15.
		} else if (term.indexOf(':', 1) != -1) {
			
		// 16.
		} else if (term.contains("/")) {
			
		// 17.
		} else if ("@type".equals(term)) {
			//TODO
		}
		
		//TODO
		
		// 21.
		if (valueObject.containsKey(Keywords.CONTEXT)) {
			
			// 21.1.
			//TODO
			
			// 21.2.
			JsonValue context = valueObject.get(Keywords.CONTEXT);
			
			// 21.3.
			try {
				ContextProcessor
						.with(activeContext, context, baseUrl)
						.overrideProtected(true)
						.remoteContexts(new ArrayList<>(remoteContexts))
						.validateScopedContext(false)
						.compute()
						;
			} catch (JsonLdError e) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_SCOPED_CONTEXT);
			}
					
			// 21.4.
			definition.setContext(context);
			definition.setBaseUrl(baseUrl);			
		}
		
		//TODO
		
		// 28
		activeContext.setTerm(term, definition);
		defined.put(term, Boolean.TRUE);
	}
	
	
	
}
