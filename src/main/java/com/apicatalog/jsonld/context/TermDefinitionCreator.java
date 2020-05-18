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
import com.apicatalog.jsonld.grammar.CompactUri;
import com.apicatalog.jsonld.grammar.DirectionType;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.utils.UriUtils;

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
			String expandedTypeString = UriExpansion
						.with(activeContext, ((JsonString)type).getString())
						.localContext(localContext)
						.defined(defined)
						.vocab(true)
						.compute();
			
			if (expandedTypeString == null) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
			}

			// 12.3.
			if (((Keywords.JSON.equals(expandedTypeString) || Keywords.NONE.equals(expandedTypeString))
				&& activeContext.inMode(Version.V1_0) 
				)
				// 12.4.
				|| (Keywords.isNot(expandedTypeString, Keywords.ID, Keywords.JSON, Keywords.NONE, Keywords.VOCAB) 
						&& UriUtils.isNotURI(expandedTypeString)
					)
			) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
			}

			// 12.5.
			definition.typeMapping = expandedTypeString;
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
					
					String idValueString = ((JsonString)idValue).getString();
					
					// 14.2.2
					if (!Keywords.contains(idValueString) && Keywords.hasForm(idValueString)) {
						//TODO generate warning
						return;
					}

					// 14.2.3
					definition.uriMapping = UriExpansion
												.with(activeContext, idValueString)
												.localContext(localContext)
												.defined(defined)
												.vocab(true)
												.compute();
					
					if (Keywords.CONTEXT.equals(definition.uriMapping)) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ALIAS);
					}
					
					if (!Keywords.contains(definition.uriMapping)
							&& !UriUtils.isURI(definition.uriMapping)
							//TODO not a blank node
							) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
					}

					// 14.2.4
					if (term.indexOf(':', 1) != -1 || term.contains("/")) {
						
						// 14.2.4.1
						defined.put(term, Boolean.TRUE);
						
						// 14.2.4.2
						String expandedTerm = UriExpansion.with(activeContext, term)
								.localContext(localContext)
								.defined(defined)
								.vocab(true)
								.compute();
						
						if (expandedTerm == null 
								|| !expandedTerm.equals(definition.uriMapping)
								) {
							throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
						}
					}
					
					// 14.2.5
					if (!term.contains(":") &&  !term.contains("/") && Boolean.TRUE.equals(simpleTerm)) {
					
						if (definition.uriMapping != null 
								&& (
									(UriUtils.isURI(definition.uriMapping) 
											&& UriUtils.endsWithGenDelim(definition.uriMapping))
									|| CompactUri.isBlank(definition.uriMapping)
									)
								) {
							definition.prefixFlag = true;
						}
					}
				}				
			}
			
		// 15.
		} else if (term.indexOf(':', 1) != -1) {
			
			final CompactUri compactUri = CompactUri.create(term);
			
			// 15.1.
			if (compactUri != null 
					&& compactUri.isNotBlank()
					&& localContext.containsKey(compactUri.getPrefix())
					) {
				
				TermDefinitionCreator.with(activeContext, localContext, compactUri.getPrefix(), defined).create();

			// 15.2.
			} else if (compactUri != null
					&& compactUri.isNotBlank()
					&& activeContext.containsTerm(compactUri.getPrefix())) {
				
				TermDefinition prefixDefinition = activeContext.getTerm(compactUri.getPrefix());
				
				definition.uriMapping = prefixDefinition.uriMapping.concat(compactUri.getSuffix());
				
			// 15.3.
			} else {
				definition.uriMapping = term;
			}
				
		// 16.
		} else if (term.contains("/")) {

			definition.uriMapping = UriExpansion.with(activeContext, term)
										.localContext(localContext)
										.defined(defined)
										.vocab(true)
										.compute();
			
			if (!UriUtils.isURI(definition.uriMapping)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
			}					
			
		// 17.
		} else if (Keywords.TYPE.equals(term)) {
			definition.uriMapping = Keywords.TYPE;
			
		// 18.
		} else if (activeContext.vocabularyMapping == null) {
			throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
			
		} else {
			definition.uriMapping = activeContext.vocabularyMapping.toString().concat(term);
		}
		
		// 19.
		if (valueObject.containsKey(Keywords.CONTAINER)) {

			// 19.1.
			JsonValue container = valueObject.get(Keywords.CONTAINER);
			
			if (ValueType.NULL.equals(container.getValueType())) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_CONTAINER_MAPPING);
			}			
			
			if (ValueType.ARRAY.equals(container.getValueType())) {
				//TODO
				container = container.asJsonArray().get(0);
			}
			
			if (!ValueType.STRING.equals(container.getValueType())) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_CONTAINER_MAPPING);
			}

			String containerString = ((JsonString)container).getString();
			

			// 19.2.
			if (activeContext.inMode(Version.V1_0) 
					&& (Keywords.ID.equals(containerString) 
						|| Keywords.GRAPH.equals(containerString)
						|| Keywords.TYPE.equals(containerString)	
						)) {
					throw new JsonLdError(JsonLdErrorCode.INVALID_CONTAINER_MAPPING);
			}
					
			// 19.3.			
			definition.addContainerMapping(containerString);

			// 19.4.
			if (definition.getContainerMapping().contains(Keywords.TYPE)) {
				
				// 19.4.1.
				if (definition.typeMapping == null) {
					definition.typeMapping = Keywords.ID;  
				}
				
				if (!Keywords.ID.equals(definition.typeMapping) && !Keywords.VOCAB.equals(definition.typeMapping)) {
					throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
				}
			}
		}
		
		// 20.
		if (valueObject.containsKey(Keywords.INDEX)) {
		
			// 20.1.
			if (activeContext.inMode(Version.V1_0)
					|| !definition.getContainerMapping().contains(Keywords.INDEX)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}

			// 20.2.
			JsonValue index = valueObject.get(Keywords.INDEX);
			
			if (!ValueType.STRING.equals(index.getValueType())) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}
			
			String indexString = ((JsonString)index).getString();
			
			String expandedIndex = 
					UriExpansion
						.with(activeContext, indexString)
						.localContext(localContext)
						.defined(defined)
						.vocab(true)
						.compute();
			
			if (expandedIndex == null || UriUtils.isNotURI(expandedIndex)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}

			definition.indexMapping = indexString;
		}
		
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

		// 22.
		if (valueObject.containsKey(Keywords.LANGUAGE) && !valueObject.containsKey(Keywords.TYPE)) {
			
			// 22.1. - 2.
			JsonValue language = valueObject.get(Keywords.LANGUAGE);
			
			if (ValueType.NULL.equals(language.getValueType())) {
				definition.languageMapping = null;
				
			} else if (ValueType.STRING.equals(language.getValueType())) {
				definition.languageMapping = ((JsonString)language).getString();
				
			} else {
				throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_MAPPING);
			}			
		}

		// 23.
		if (valueObject.containsKey(Keywords.DIRECTION) && !valueObject.containsKey(Keywords.TYPE)) {

			JsonValue direction = valueObject.get(Keywords.DIRECTION);
			
			if (ValueType.NULL.equals(direction.getValueType())) {
				definition.directionMapping = null;
				
			} else if (ValueType.STRING.equals(direction.getValueType())) {
				
				String directionString = ((JsonString)direction).getString();
				
				if ("ltr".equals(directionString)) {
					definition.directionMapping = DirectionType.LTR;
					
				} else 	if ("rtl".equals(directionString)) {
					definition.directionMapping = DirectionType.RTL;
					
				} else {
					throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
				}
				
			} else {
				throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
			}			
		}

		// 24.
		if (valueObject.containsKey(Keywords.NEST)) {
		//TODO
		}

		// 25.
		if (valueObject.containsKey(Keywords.PREFIX)) {

			// 25.1.
			if (activeContext.inMode(Version.V1_0)
					|| term.contains(":")
					|| term.contains("/")
					) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}
			
			// 25.2.
			JsonValue prefix = valueObject.get(Keywords.PREFIX);
			
			if (ValueType.TRUE.equals(prefix.getValueType())) {
				definition.prefixFlag = true;
				
			} else if (ValueType.FALSE.equals(prefix.getValueType())) {
				definition.prefixFlag = false;
				
			} else {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}
			
			// 25.3
			if (definition.prefixFlag && Keywords.contains(definition.uriMapping)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}
		}
		
		// 26.
		//TODO
		
		// 27.
		if (overrideProtectedFlag && previousDefinition != null && previousDefinition.protectedFlag) {
			//TODO
		}		
		// 28
		activeContext.setTerm(term, definition);
		defined.put(term, Boolean.TRUE);
	}	
}