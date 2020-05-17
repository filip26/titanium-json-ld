package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.expansion.UriExpansion;
import com.apicatalog.jsonld.grammar.Commons;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.Version;

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
		if (Keywords.TYPE.equals(term) && activeContext.inMode(Version.V1_0)) {
			
			if (ValueType.OBJECT.equals(value.getValueType())) {
				
				JsonObject map = value.asJsonObject();
				
				if (map.size() == 1 && map.containsKey(Keywords.PROTECTED)) {
					
				} else if (map.size() == 1 && map.containsKey(Keywords.CONTAINER)) {
					JsonValue container = map.get(Keywords.CONTAINER);
					if (!ValueType.STRING.equals(container.getValueType())
							|| !Keywords.SET.equals(((JsonString)container).getString())
							) {
						throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
						}
				
				} else if (map.size() == 2) {
					//TODO
				} else {
					throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
				}

			} else {			
				throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
			}

		}
		
		// 5.
		if (Keywords.contains(term)) {
			throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
			
		} else if (Keywords.hasForm(term)) {
			//TODO warning
			return;
		}
		
		// 6.
		TermDefinition previousDefinition = activeContext.removeTerm(term);

		JsonObject valueObject = null;
		Boolean simpleTerm = null;

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

			// 12.1.
			JsonValue type = valueObject.get(Keywords.TYPE);

			if (!ValueType.STRING.equals(type.getValueType())) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
			}

			// 12.2.
			String typeString = UriExpansion
						.with(activeContext, ((JsonString)type).getString())
						.localContext(localContext)
						.defined(defined)
						.compute()
						.orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING));

			// 12.3.
			if (((Keywords.JSON.equals(typeString) || Keywords.NONE.equals(typeString))
				&& activeContext.inMode(Version.V1_0) 
				)
				// 12.4.
				|| (Keywords.isNot(typeString, Keywords.ID, Keywords.JSON, Keywords.NONE, Keywords.VOCAB) 
						|| !Commons.isURI(typeString)
					)
			) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
			}

			// 12.5.
			definition.typeMapping = typeString;
		}
		
		// 13.
		if (valueObject.containsKey(Keywords.REVERSE)) {
			//TODO
		}
		
		// 14.
		if (valueObject.containsKey(Keywords.ID)) {
			
			JsonValue idValue = valueObject.get(Keywords.ID);
			
			if (!ValueType.STRING.equals(idValue.getValueType()) || !term.equals(((JsonString)idValue).getString())) {
				
				// 14.1.
				if (JsonValue.NULL.equals(idValue.getValueType())) {
					
				// 14.2
				} else {
					
					// 14.2.1
					if (!ValueType.STRING.equals(idValue.getValueType())) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
					}
					
					// 14.2.2
					//TODO
					
					// 14.2.3
					definition.uriMapping = UriExpansion
												.with(activeContext, ((JsonString)idValue).getString())
												.localContext(localContext)
												.defined(defined)
												.compute()
												.orElse(null);
					//TODO
					
					
					// 14.2.4
					if (term.indexOf(':', 1) != -1 /*TODO end limit - 1*/ || term.contains("/")) {
						
						// 14.2.4.1
						defined.put(term, Boolean.TRUE);
						
						// 14.2.4.2
						//TODO
					}
					
					// 14.2.5
					if (!term.contains(":") &&  !term.contains("/") && Boolean.TRUE.equals(simpleTerm)) {
					
						if (definition.uriMapping != null 
								&& Commons.isURI(definition.uriMapping) 
								&& Commons.endsWithGenDelim(definition.uriMapping)
								) {
							definition.prefixFlag = true;
						}
						//TODO
					}
					
				}				
			}
			
		// 15.
		} else if (term.indexOf(':', 1) != -1) {
			
		// 16.
		} else if (term.contains("/")) {
			
		// 17.
		} else if (Keywords.TYPE.equals(term)) {
			definition.setUriMapping(Keywords.TYPE);
			
		// 18.
		} else if (activeContext.vocabularyMapping != null) {
			//TODO
		}
		
		// 19.
		if (valueObject.containsKey(Keywords.CONTAINER)) {

			// 19.1.
			JsonValue container = valueObject.get(Keywords.CONTAINER);
			//TODO check value and throw an exception
			
			if (ValueType.ARRAY.equals(container.getValueType())) {
				container = container.asJsonArray().get(0);
			}
			
			// 19.2.
			//TODO
			if (!ValueType.STRING.equals(container.getValueType())) {
 
			}
			
			String containerMapping = ((JsonString)container).getString();	//FIXME
		
			
			// 19.3.
			definition.addContainerMapping(containerMapping);
			
			// 19.4.
			//TODO
		}
		
		// 20.
		
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
			definition.setLocalContext(context);
			definition.setBaseUrl(baseUrl);			
		}
		
		//TODO
		
		// 28
		activeContext.setTerm(term, definition);
		defined.put(term, Boolean.TRUE);
	}	
}