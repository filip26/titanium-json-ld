package com.apicatalog.jsonld.context;

import java.util.stream.Collectors;

import javax.json.JsonString;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#inverse-context-creation">Inverse Context Creation</a>
 *
 */
public final class InverseContextBuilder {

    private final ActiveContext activeContext;
    
    private InverseContextBuilder(final ActiveContext activeContext) {
        this.activeContext = activeContext;
    }
    
    public static final InverseContextBuilder with(final ActiveContext activeContext) {
        return new InverseContextBuilder(activeContext);  
    }

    public InverseContext build() {
        
        // 1.
        final InverseContext result = new InverseContext();
        
        // 2.
        final String defaultLanguage = activeContext.getDefaultLanguage() != null
                                    ? activeContext.getDefaultLanguage().toLowerCase()
                                    : Keywords.NONE;
                                    
        // 3
        for (final String term : activeContext.getTerms().stream().sorted().collect(Collectors.toList())) {
        
            // 3.1.
            if (activeContext.getTerm(term).isEmpty()) {
                continue;
            }

            final TermDefinition termDefinition = activeContext.getTerm(term).get();

            // 3.2.
            String container = Keywords.NONE;
            
            if (termDefinition.getContainerMapping() != null 
                    && !termDefinition.getContainerMapping().isEmpty()) {
                
                container = termDefinition.getContainerMapping().stream().sorted().collect(Collectors.joining());
            }
            
            // 3.3.
            final String variable = termDefinition.getUriMapping();
                        
            result.setIfAbsent(variable, container, Keywords.ANY, Keywords.NONE, term);

            // 3.10.
            if (termDefinition.isReverseProperty()) {

                // 3.10.1
                result.setIfAbsent(variable, container, Keywords.TYPE, Keywords.REVERSE, term);
            
            // 3.11.
            } else if (Keywords.NONE.equals(termDefinition.getTypeMapping()) ) {
                
                // 3.11.1.
                result.setIfAbsent(variable, container, Keywords.LANGUAGE, Keywords.ANY, term);

                // 3.11.2.
                result.setIfAbsent(variable, container, Keywords.TYPE, Keywords.ANY, term);

            // 3.12.
            } else if (termDefinition.getTypeMapping() != null) {

                // 3.12.1
                result.setIfAbsent(variable, container, Keywords.TYPE, termDefinition.getTypeMapping(), term);
                
            // 3.13.
            } else if (termDefinition.getLanguageMapping() != null
                        && termDefinition.getDirectionMapping() != null
                    ) {
         
                // 3.13.1.
                String langDir = Keywords.NULL;
                
                // 3.13.2.
                if (JsonUtils.isString(termDefinition.getLanguageMapping())
                        && termDefinition.getDirectionMapping() != DirectionType.NULL
                        ) {
                    
                    langDir = ((JsonString)termDefinition.getLanguageMapping())
                                    .getString()
                                    .concat("_")
                                    .concat(termDefinition.getDirectionMapping().name())
                                    .toLowerCase();

                // 3.13.3.
                } else if (JsonUtils.isString(termDefinition.getLanguageMapping())) {
                    
                    langDir = ((JsonString)termDefinition.getLanguageMapping()).getString().toLowerCase();
                    
                // 3.13.4.
                } else if (termDefinition.getDirectionMapping() != DirectionType.NULL) {
                    
                    langDir = "_".concat(termDefinition.getDirectionMapping().name().toLowerCase());
                }
                
                // 3.13.5.
                result.setIfAbsent(variable, container, Keywords.LANGUAGE, langDir, term);
                
            // 3.14.
            } else if (termDefinition.getLanguageMapping() != null) {

                /// 3.14.1.
                String language = Keywords.NULL;
                
                if (JsonUtils.isString(termDefinition.getLanguageMapping())) {
                    language = ((JsonString)termDefinition.getLanguageMapping()).getString().toLowerCase();
                }
                              
                // 3.14.2.
                result.setIfAbsent(variable, container, Keywords.LANGUAGE, language, term);
                
            // 3.15.
            } else if (termDefinition.getDirectionMapping() != null) {

                // 3.15.1.
                String direction = Keywords.NONE;
                
                if (termDefinition.getDirectionMapping() != DirectionType.NULL) {
                    direction = "_".concat(termDefinition.getDirectionMapping().name().toLowerCase());
                }

                // 3.15.2.
                result.setIfAbsent(variable, container, Keywords.LANGUAGE, direction, term);

            // 3.16.
            } else if (activeContext.getDefaultBaseDirection() != null) {
                
                // 3.16.1.
                final String langDir = (activeContext.getDefaultLanguage() != null 
                                    ? activeContext.getDefaultLanguage()
                                    : ""
                                    )
                                    .concat("_")
                                    .concat(activeContext.getDefaultBaseDirection().name())
                                    .toLowerCase();
                
                // 3.16.2.
                result.setIfAbsent(variable, container, Keywords.LANGUAGE, langDir, term);

                // 3.16.3.
                result.setIfAbsent(variable, container, Keywords.LANGUAGE, Keywords.NONE, term);
                
                // 3.16.4.
                result.setIfAbsent(variable, container, Keywords.TYPE, Keywords.NONE, term);
                
            // 3.17.
            } else {
                
                // 3.17.1.
                result.setIfAbsent(variable, container, Keywords.LANGUAGE, defaultLanguage, term);
                
                // 3.17.2.
                result.setIfAbsent(variable, container, Keywords.LANGUAGE, Keywords.NONE, term);
                
                // 3.17.2.
                result.setIfAbsent(variable, container, Keywords.TYPE, Keywords.NONE, term);
            }
        }
        
        // 4.
        return result;
    }
}
