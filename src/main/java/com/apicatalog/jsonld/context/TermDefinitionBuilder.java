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
import com.apicatalog.jsonld.expansion.UriExpansionBuilder;
import com.apicatalog.jsonld.grammar.CompactUri;
import com.apicatalog.jsonld.grammar.DirectionType;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.utils.JsonUtils;
import com.apicatalog.jsonld.utils.UriUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#create-term-definition">Create Term Definition</a>
 *
 */
public final class TermDefinitionBuilder {

	// mandatory
	ActiveContext activeContext;
	
	JsonObject localContext;
	
	String term;
	
	Map<String, Boolean> defined;
	
	// optional
	URI baseUrl;
	
	boolean protectedFlag;
	
	boolean overrideProtectedFlag;
	
	Collection<String> remoteContexts;
	
	boolean validateScopedContext;
	
	LoadDocumentCallback documentLoader;
	
	private TermDefinitionBuilder(ActiveContext activeContext, JsonObject localContext, String term, Map<String, Boolean> defined) {
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

	public static final TermDefinitionBuilder with(	ActiveContext activeContext, JsonObject localContext, String term, Map<String, Boolean> defined) {
		return new TermDefinitionBuilder(activeContext, localContext, term, defined);
	}
	
	public TermDefinitionBuilder baseUrl(URI baseUrl) {
		this.baseUrl = baseUrl;
		return this;
	}
	
	public TermDefinitionBuilder protectedFlag(boolean protectedFlag) {
		this.protectedFlag = protectedFlag;
		return this;
	}
	
	public TermDefinitionBuilder overrideProtectedFlag(boolean overrideProtectedFlag) {
		this.overrideProtectedFlag = overrideProtectedFlag;
		return this;
	}
	
	public TermDefinitionBuilder remoteContexts(Collection<String> remoteContexts) {
		this.remoteContexts = remoteContexts;
		return this;
	}

	public TermDefinitionBuilder validateScopedContext(boolean validateScopedContext) {
		this.validateScopedContext = validateScopedContext;
		return this;
	}
	
	public TermDefinitionBuilder documentLoader(LoadDocumentCallback documentLoader) {
		this.documentLoader = documentLoader;
		return this;
	}

	public void build() throws JsonLdError {
		
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
					//TODO
					
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

		if (value == null || JsonUtils.isNull(value)) {
			// 7.
			valueObject = Json.createObjectBuilder().add(Keywords.ID, JsonValue.NULL).build();
			
		} else if (JsonUtils.isString(value)) {
			// 8.
			valueObject = Json.createObjectBuilder().add(Keywords.ID, value).build();
			simpleTerm = true;
			
		} else if (JsonUtils.isObject(value)) {
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
			
			if (activeContext.inMode(Version.V1_0)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}
			
			if (JsonUtils.isNotBoolean(valueObject.get(Keywords.PROTECTED))) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROTECTED_VALUE);
			}
			
			definition.protectedFlag = JsonUtils.isTrue(valueObject.get(Keywords.PROTECTED));			
		}
		
		// 12.
		if (valueObject.containsKey(Keywords.TYPE)) {

			// 12.1.
			JsonValue type = valueObject.get(Keywords.TYPE);

			if (!ValueType.STRING.equals(type.getValueType())) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
			}

			// 12.2.
			String expandedTypeString = UriExpansionBuilder
						.with(activeContext, ((JsonString)type).getString())
						.localContext(localContext)
						.defined(defined)
						.vocab(true)
						.build();
			
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
			
			// 13.1.
			if (valueObject.containsKey(Keywords.ID) || valueObject.containsKey(Keywords.NEST)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY);
			}
			
			JsonValue reverse = valueObject.get(Keywords.REVERSE);
			
			// 13.2.
			if (JsonUtils.isNotString(reverse)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
			}
			
			String reverseString = ((JsonString)reverse).getString();
			
			// 13.3.
			if (Keywords.hasForm(reverseString)) {
				//TODO warning;
				return;
			}
			
			// 13.4.
			definition.uriMapping = UriExpansionBuilder.with(activeContext, reverseString)
										.localContext(localContext)
										.defined(defined)
										.vocab(true)
										.build();
			
			if (UriUtils.isNotURI(definition.uriMapping)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
			}
			
			// 13.5.
			if (valueObject.containsKey(Keywords.CONTAINER)) {
				
				JsonValue container = valueObject.get(Keywords.CONTAINER);
				
				if (JsonUtils.isNotString(container) && JsonUtils.isNotNull(container)) {
					throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY);
				}
				
				if (JsonUtils.isString(container)) {
				
					String containerString = ((JsonString)container).getString();
					
					if (Keywords.isNot(containerString, Keywords.SET, Keywords.INDEX)) {
					
						definition.addContainerMapping(containerString);
						
					} else {
						throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY);
					}
				}				
			}
			
			// 13.6.
			definition.reversePropertyFlag = true;
			
			// 13.7.
			activeContext.setTerm(term, definition);
			defined.put(term, Boolean.TRUE);
			return;
		}
		
		// 14.
		if (valueObject.containsKey(Keywords.ID)) {
			
			JsonValue idValue = valueObject.get(Keywords.ID);
			
			if (JsonUtils.isNotString(idValue) || !term.equals(((JsonString)idValue).getString())) {
				
				// 14.1.
				if (JsonUtils.isNull(idValue)) {
					
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
					definition.uriMapping = UriExpansionBuilder
												.with(activeContext, idValueString)
												.localContext(localContext)
												.defined(defined)
												.vocab(true)
												.build();
					
					if (Keywords.CONTEXT.equals(definition.uriMapping)) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ALIAS);
					}
					
					if (!Keywords.contains(definition.uriMapping)
							&& !UriUtils.isURI(definition.uriMapping)
							&& !CompactUri.isBlankNode(definition.uriMapping)
							) {
						throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
					}
					
					// 14.2.4
					if (term.indexOf(':', 1) != -1 || term.contains("/")) {
						
						// 14.2.4.1
						defined.put(term, Boolean.TRUE);
						
						// 14.2.4.2
						String expandedTerm = UriExpansionBuilder.with(activeContext, term)
								.localContext(localContext)
								.defined(defined)
								.vocab(true)
								.build();
						
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
									(UriUtils.isURI(definition.uriMapping) && UriUtils.endsWithGenDelim(definition.uriMapping))
									|| CompactUri.isBlankNode(definition.uriMapping)
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
				
				TermDefinitionBuilder
					.with(activeContext, localContext, compactUri.getPrefix(), defined)
					.documentLoader(documentLoader)
					.build();
			}
			// 15.2.
			if (compactUri != null
					&& compactUri.isNotBlank()
					&& activeContext.containsTerm(compactUri.getPrefix())) {
				
				TermDefinition prefixDefinition = activeContext.getTerm(compactUri.getPrefix());
				
				definition.uriMapping = prefixDefinition.uriMapping.concat(compactUri.getSuffix());
				
			// 15.3.
			} else if (UriUtils.isURI(term) || CompactUri.isBlankNode(term)){
				definition.uriMapping = term;
			}
				
		// 16.
		} else if (term.contains("/")) {

			definition.uriMapping = UriExpansionBuilder.with(activeContext, term)
										.localContext(localContext)
										.defined(defined)
										.vocab(true)
										.build();

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
			//TODO

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
					UriExpansionBuilder
						.with(activeContext, indexString)
						.localContext(localContext)
						.defined(defined)
						.vocab(true)
						.build();
			
			if (expandedIndex == null || UriUtils.isNotURI(expandedIndex)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}

			definition.indexMapping = indexString;
		}
		
		// 21.
		if (valueObject.containsKey(Keywords.CONTEXT)) {
	
			// 21.1.
			if (activeContext.inMode(Version.V1_0)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}
			
			// 21.2.
			JsonValue context = valueObject.get(Keywords.CONTEXT);
			
			// 21.3.
			try {
				ContextProcessor
						.with(activeContext, context, baseUrl)
						.overrideProtected(true)
						.remoteContexts(new ArrayList<>(remoteContexts))
						.validateScopedContext(false)
						.documentLoader(documentLoader)
						.compute();
				
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
			
			if (JsonUtils.isNull(language) || JsonUtils.isString(language)) {
				definition.languageMapping = language;
				
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

			// 24.1
			if (activeContext.inMode(Version.V1_0)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
			}
			
			JsonValue nest = valueObject.get(Keywords.NEST);
			
			if (JsonUtils.isNotString(nest)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
			}
			
			String nestString = ((JsonString)nest).getString();
			
			if (Keywords.contains(nestString) && !Keywords.NEST.equals(nestString)) {
				throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
			}
			
			definition.nestValue = nestString;
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
		if (!Keywords.allIsOneOf(valueObject.keySet(),
				Keywords.ID,
				Keywords.REVERSE,
				Keywords.CONTAINER,
				Keywords.CONTEXT,
				Keywords.DIRECTION,
				Keywords.INDEX,
				Keywords.LANGUAGE,
				Keywords.NEST,
				Keywords.PREFIX,
				Keywords.PROTECTED,
				Keywords.TYPE
				)) {
			throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
		}			
		
		// 27.
		if (overrideProtectedFlag && previousDefinition != null && previousDefinition.protectedFlag) {
			// 27.1.
			//TODO
			
			// 27.2.
			definition = previousDefinition;
		}		
		// 28
		activeContext.setTerm(term, definition);
		defined.put(term, Boolean.TRUE);
	}	
}