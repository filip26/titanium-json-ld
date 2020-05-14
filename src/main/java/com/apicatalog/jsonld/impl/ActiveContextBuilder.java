package com.apicatalog.jsonld.impl;

import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;

import com.apicatalog.jsonld.JsonLdContext;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;

/**
 * A context that is used to resolve terms while the processing algorithm is running.
 * 
 */
public class ActiveContextBuilder {

	private final ActiveContext activeContext; 
	private final JsonLdContext localContext; 
	private final URL baseUrl;	
	
	private Collection<JsonLdContext> remoteContexts;
	
	private boolean overrideProtected;
	
	private boolean propagate;
	
	private boolean validateScopedContext;
	
	private ActiveContextBuilder(
			ActiveContext activeContext, 
			JsonLdContext localContext, 
			URL baseUrl) {
		
		this.activeContext = activeContext;
		this.localContext = localContext;
		this.baseUrl = baseUrl;
		
		// default optional values
		this.remoteContexts = Collections.emptyList();
		this.overrideProtected = false;
		this.propagate = true;
		this.validateScopedContext = true;
	}

	public static final ActiveContextBuilder create(ActiveContext activeContext, JsonLdContext localContext, URL baseUrl) {
		return new ActiveContextBuilder(activeContext, localContext, baseUrl);
	}
	
	public ActiveContextBuilder remoteContexts(Collection<JsonLdContext> remoteContexts) {
		this.remoteContexts = remoteContexts;
		return this;
	}
	
	public ActiveContextBuilder overrideProtected(boolean overrideProtected) {
		this.overrideProtected = overrideProtected;
		return this;
	}
	
	public ActiveContextBuilder propagate(boolean propagate) {
		this.propagate = propagate;
		return this;
	}
	
	public ActiveContextBuilder validateScopedContext(boolean validateScopedContext) {
		this.validateScopedContext = validateScopedContext;
		return this;
	}

	public ActiveContext build() throws JsonLdError {
		// 1. Initialize result to the result of cloning active context, with inverse context set to null.
		ActiveContext result = new ActiveContext(activeContext);
		//TODO
		
		// 2. If local context is an object containing the member @propagate, 
		//    its value MUST be boolean true or false, set propagate to that value.
		//TODO

		// 3. If propagate is false, and result does not have a previous context, 
		//    set previous context in result to active context.
		if (!propagate && result.previousContext == null) {
			result.previousContext = activeContext;
		}
		
		// 4. If local context is not an array, set local context to an array containing only local context.
		
		// 5. For each item context in local context:
		for (JsonLdContext item : localContext.getContexts()) {
			// 5.1. If context is null:
			if (item == null) {
				// 5.1.1. If override protected is false and active context contains any protected term definitions,
				//       an invalid context nullification has been detected and processing is aborted.
				if (!overrideProtected) {
					//TODO
				}
				
				// 5.1.2. Initialize result as a newly-initialized active context, 
				//       setting both base IRI and original base URL to the value of original base URL in active context, 
				//       and, if propagate is false, previous context in result to the previous value of result.
				try {
					result = new ActiveContext(activeContext.baseUrl.toURI(), activeContext.baseUrl);
					if (!propagate) {
						//TODO
					}
					
				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_IRI, e);
				}

				// 5.1.3. Continue with the next context 
				continue;
			} 
			
//			if (item.isURI()) {
//				
//			}
		}
		
		return result;
	}
	
}
