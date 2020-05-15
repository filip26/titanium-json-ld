package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#create-term-definition">Create Term Definition</a>
 *
 */
public final class TermDefinitionCreator {

	// mandatory
	ActiveContext activeContext;
	JsonValue localContext;
	String term;
	Map<String, Boolean> defined;
	
	// optional
	URI baseUrl;
	boolean protectedFlag;
	boolean overrideProtectedFlag;
	Collection remoteContexts;
	boolean validateScopedContext;

	private TermDefinitionCreator(ActiveContext activeContext, JsonValue localContext, String term, Map<String, Boolean> defined) {
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

	public static final TermDefinitionCreator with(	ActiveContext activeContext, JsonValue localContext, String term, Map<String, Boolean> defined) {
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
		
		// 1.
		if (defined.containsKey(term)) {
			
			if (Boolean.TRUE.equals(defined.get(term))) {
				return;
			}
			
			throw new JsonLdError(JsonLdErrorCode.CYCLIC_IRI_MAPPING);
		}
		
		// 2.
		
	}
	
	
	
}
