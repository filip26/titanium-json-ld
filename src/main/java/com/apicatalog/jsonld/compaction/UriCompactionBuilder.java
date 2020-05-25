package com.apicatalog.jsonld.compaction;

import java.util.Map.Entry;

import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.CompactUri;

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

    public UriCompactionBuilder setValue(JsonValue value) {
        this.value = value;
        return this;
    }
    
    public UriCompactionBuilder setReverse(boolean reverse) {
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
            activeContext.setInverseContext(activeContext.createInverse().build());     //TODO
        }

        // 3.
        ActiveContext inverseContext = activeContext.getInverseContext();
        
        // 4.
        if (vocab && inverseContext.containsTerm(variable)) {
            //TODO
        }
        
        // 5.
        if (vocab && activeContext.getVocabularyMapping() != null) {
            //TODO            
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
                                    .concat(termDefinition.getUriMapping().substring(variable.length()));

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
            return compactUri.toString();
        }
        
        // 9.
        //TODO
        
        // 10.
        if (!vocab) {
            //TODO
        }
        
        // 11.
        return variable;
    }

}
