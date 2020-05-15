package com.apicatalog.jsonld.expansion;

import java.util.Map;
import java.util.Optional;

import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.Keywords;
/**
 * 
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#algorithm-4">IRI Expansion</a>
 *
 */
public final class UriExpansion {

	// mandatory
	private ActiveContext activeContext;
	private String value;
	
	// optional
	private boolean documentRelative;
	private boolean vocab;
	
	private JsonObject localContext;
	private Map defined;	//TODO
		
	private UriExpansion(final ActiveContext activeContext, final String value) {
		this.activeContext = activeContext;
		this.value = value;
		
		// default values
		this.documentRelative = false;
		this.vocab = false;
		this.localContext = null;
		this.defined = null;
	}
	
	public static final UriExpansion with(final ActiveContext activeContext, final String value) {
		return new UriExpansion(activeContext, value);
	}

	public UriExpansion documentRelative(boolean value) {
		this.documentRelative = false;
		return this;
	}

	public UriExpansion vocab(boolean value) {
		this.vocab = value;
		return this;
	}

	public UriExpansion localContext(JsonObject value) {
		this.localContext = value;
		return this;
	}
	
	public UriExpansion defined(Map value) {
		this.defined = value;
		return this;
	}
	
	public Optional<String> compute() {
		
		// 1. If value is a keyword or null, return value as is.
		if (value == null || Keywords.contains(value)) {
			return Optional.of(value);
		}
		
		// 2. If value has the form of a keyword (i.e., it matches the ABNF rule "@"1*ALPHA from [RFC5234]),
		//	  a processor SHOULD generate a warning and return null.
		if (Keywords.hasForm(value)) {
			//TODO varning
			return Optional.empty();
		}
		
		/*
		 *  3. If local context is not null, it contains an entry with a key that equals value, 
		 *     and the value of the entry for value in defined is not true, invoke the Create Term Definition algorithm, 
		 *     passing active context, local context, value as term, and defined. 
		 *     This will ensure that a term definition is created for value in active context during Context Processing 
		 */
		if (localContext != null) {
			//TODO
		}
		
		// 4. if active context has a term definition for value, 
		//	  and the associated IRI mapping is a keyword, return that keyword.
		if (activeContext.containsTerm(value)) {
			
			TermDefinition termDefinition = activeContext.getTerm(value);
//			System.out.println("        " + value + ", " + termDefinition.getUriMapping() + ", " + vocab);
			//TODO
			
			// 5. If vocab is true and the active context has a term definition for value, return the associated IRI mapping
			if (vocab) {
				return Optional.of(termDefinition.getUriMapping());
			}
		}
		
//		System.out.println("        " + value + ", " + vocab + ", " + documentRelative);
		
		// 6. If value contains a colon (:) anywhere after the first character, it is either an IRI, 
		//    a compact IRI, or a blank node identifier
		if (value.indexOf(':', 1) != -1) {
			//TODO
		}
		
		// 7. If vocab is true, and active context has a vocabulary mapping, 
		//    return the result of concatenating the vocabulary mapping with value.
		if (vocab) {
			//TODO
			
		// 8.
		} else {
			
		}
		
		// 9.
		return Optional.of(value);
	}
	
	
}
