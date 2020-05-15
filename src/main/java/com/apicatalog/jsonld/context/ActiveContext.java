package com.apicatalog.jsonld.context;

import java.net.URI;
import java.net.URL;

import com.apicatalog.jsonld.impl.DirectionType;

/**
 * A context that is used to resolve terms while the processing algorithm is running.
 * 
 */
public class ActiveContext {

	// the current base IRI 
	protected URI baseUri;
	
	// the original base URL
	protected URL baseUrl;
	
	protected Object inverseContext;	//TODO
	
	// an optional previous context, used when a non-propagated context is defined.
	protected ActiveContext previousContext;
	
	// an optional vocabulary mapping
	protected URI vocabularyMapping;
	
	// an optional default language 
	protected String defaultLanguage;
	
	// an optional default base direction ("ltr" or "rtl")
	protected DirectionType defaultBaseDirection;
	
	public ActiveContext(final URI baseUri, final URL baseUrl) {
		this.baseUri = baseUri;
		this.baseUrl = baseUrl;
	}
	
	// copy constructor
	public ActiveContext(final ActiveContext activeContext) {
		this.baseUri = activeContext.baseUri;
		this.baseUrl = activeContext.baseUrl;
		this.inverseContext = activeContext.inverseContext;
		this.previousContext = activeContext.previousContext;
		this.vocabularyMapping = activeContext.vocabularyMapping;
		this.defaultLanguage = activeContext.defaultLanguage;
		this.defaultBaseDirection = activeContext.defaultBaseDirection;
	}

	public boolean hasTermDefinition(String value) {
		// TODO Auto-generated method stub
		return false;
	}	
}
