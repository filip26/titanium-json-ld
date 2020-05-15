package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.jsonld.grammar.DirectionType;

/**
 * A context that is used to resolve terms while the processing algorithm is running.
 * 
 */
public class ActiveContext {

	// the active term definitions which specify how keys and values have to be interpreted
	Collection<TermDefinition> terms;
	
	// the current base IRI 
	URI baseUri;
	
	// the original base URL
	URI baseUrl;
	
	Object inverseContext;	//TODO
	
	// an optional previous context, used when a non-propagated context is defined.
	ActiveContext previousContext;
	
	// an optional vocabulary mapping
	URI vocabularyMapping;
	
	// an optional default language 
	String defaultLanguage;
	
	// an optional default base direction ("ltr" or "rtl")
	DirectionType defaultBaseDirection;
	
	public ActiveContext(final URI baseUri, final URI baseUrl) {
		this.baseUri = baseUri;
		this.baseUrl = baseUrl;
	}

	public ActiveContext(final URI baseUri, final URI baseUrl, final ActiveContext previousContext) {
		this.baseUri = baseUri;
		this.baseUrl = baseUrl;
		this.previousContext = previousContext;
	}

	// copy constructor
	public ActiveContext(final ActiveContext origin) {
		this.terms = origin.terms;
		this.baseUri = origin.baseUri;
		this.baseUrl = origin.baseUrl;
		this.inverseContext = origin.inverseContext;
		this.previousContext = origin.previousContext;
		this.vocabularyMapping = origin.vocabularyMapping;
		this.defaultLanguage = origin.defaultLanguage;
		this.defaultBaseDirection = origin.defaultBaseDirection;
	}

	public boolean hasTermDefinition(String value) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean containsProtectedTerm() {
		// TODO Auto-generated method stub
		return false;
	}	
}
