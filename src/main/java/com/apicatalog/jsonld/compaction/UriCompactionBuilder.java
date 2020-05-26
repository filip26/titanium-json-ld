package com.apicatalog.jsonld.compaction;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map.Entry;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.InverseContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.GraphObject;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.ListObject;
import com.apicatalog.jsonld.grammar.ValueObject;
import com.apicatalog.jsonld.utils.JsonUtils;
import com.apicatalog.jsonld.utils.UriResolver;
import com.apicatalog.jsonld.utils.UriUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#iri-compaction">IRI Compaction</a>
 *
 */
public final class UriCompactionBuilder {

    // required
    private final ActiveContext activeContext;
    private String variable;
    
    // optional
    private JsonValue value;
    private boolean vocab;
    private boolean reverse;
    
    public UriCompactionBuilder(final ActiveContext activeContext, final String variable) {
        this.activeContext = activeContext;
        this.variable = variable;
        
        // default values
        this.value = null;
        this.vocab = false;
        this.reverse = false;
    }
    
    public static UriCompactionBuilder with(ActiveContext activeContext, String variable) {
        return new UriCompactionBuilder(activeContext, variable);
    }
    
    public UriCompactionBuilder value(JsonValue value) {
        this.value = value;
        return this;
    }

    public UriCompactionBuilder vocab(boolean vocab) {
        this.vocab = vocab;
        return this;
    }
    
    public UriCompactionBuilder reverse(boolean reverse) {
        this.reverse = reverse;
        return this;
    }
    
    public String build() throws JsonLdError {
        
        // 1.
        if (variable == null) {
            return null;
        }
        
        // 2.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }

        // 3.
        InverseContext inverseContext = activeContext.getInverseContext();
        
        // 4.
        if (vocab && inverseContext.contains(variable)) {
            
            // 4.1.
            String defaultLanguage = Keywords.NONE;

            if (activeContext.getDefaultLanguage() != null) {
                
                defaultLanguage = activeContext.getDefaultLanguage().toLowerCase();
                
                if (activeContext.getDefaultBaseDirection() != null) {
                    defaultLanguage += "_".concat(activeContext.getDefaultBaseDirection().name().toLowerCase());
                }
                
            } else if (activeContext.getDefaultBaseDirection() != null) {
                defaultLanguage = "_".concat(activeContext.getDefaultBaseDirection().name().toLowerCase());                
            }
            
            // 4.2.
            if (JsonUtils.contains(Keywords.PRESERVE, value)) {
                //TODO
            }
            
            // 4.3.
            Collection<String> containers = new ArrayList<>();
            
            String typeLanguage = Keywords.LANGUAGE;    //FIXME 
            
            //TODO
            
            // 4.6.
            if (reverse) {
                //TODO
                
            // 4.7.
            } else if (ListObject.isListObject(value)) {
                //TODO
                
            // 4.8.
            } else if (GraphObject.isGraphObject(value)) {
                //TODO
                
            // 4.9.
            } else {
                
                // 4.9.1.
                if (ValueObject.isValueObject(value)) {
                    
                // 4.9.2.                    
                } else {
                    //TODO
                }
                
                // 4.9.3.
                containers.add(Keywords.SET);
            }
            
            // 4.10.
            containers.add(Keywords.NONE);
            
            //TODO
            
            // 4.14.
            Collection<String> preferredValues = new ArrayList<>();
            
            //TODO
            
            // 4.20.
            String term = activeContext.selectTerm(variable, containers, typeLanguage, preferredValues).select();
            
            // 4.21.
            if (term != null) {
                return term;
            }
        }
        
        // 5.
        if (vocab && activeContext.getVocabularyMapping() != null) {
            
            // 5.1.
            if (variable.startsWith(activeContext.getVocabularyMapping()) 
                    && variable.length() > activeContext.getVocabularyMapping().length()) {
                
                String suffix = variable.substring(activeContext.getVocabularyMapping().length());
                
                if (!activeContext.containsTerm(suffix)) {
                    return suffix;
                }
                
            }
        }
        
        // 6.
        String compactUri = null;
        
        // 7.
        for (Entry<String, TermDefinition> termEntry : activeContext.getTermsMapping().entrySet()) {
            
            TermDefinition termDefinition = termEntry.getValue();
            
            // 7.1.
            if (termDefinition.getUriMapping() == null
                    || variable.equals(termDefinition.getUriMapping())
                    || !variable.startsWith(termDefinition.getUriMapping())
                    || termDefinition.isNotPrefix()
                    ) {
                continue;
            }
            
            // 7.2.
            String compacttUriCandidate = 
                            termEntry.getKey()
                                    .concat(":")
                                    .concat(variable.substring(termDefinition.getUriMapping().length()));

            // 7.3.
            if (compactUri == null 
                    || (compacttUriCandidate.compareTo(compactUri) < 0
                            && !activeContext.containsTerm(compacttUriCandidate))
                    || (activeContext.containsTerm(compacttUriCandidate) 
                            && variable.equals(activeContext.getTerm(compacttUriCandidate))
                            && value == null
                            )
                    ) {
                compactUri = compacttUriCandidate;
            }
        }
        
        /// 8.
        if (compactUri != null) {
            return compactUri;
        }
        
        // 9.
        if (UriUtils.isAbsoluteUri(variable)) {
            
            URI uri = URI.create(variable);
            
            if (uri.getScheme() != null) {

                //FIXME
                TermDefinition uriTermDefinition = activeContext.getTerm(uri.getScheme());
                
                if (uriTermDefinition != null && uriTermDefinition.isPrefix()) {
                    //TODO

                }
                            
            }
            
        }
        
        // 10.
        if (!vocab && activeContext.getBaseUri() != null) {
            return UriResolver.makeRelative(activeContext.getBaseUri(), variable);
        }
        
        // 11.
        return variable;
    }

}
