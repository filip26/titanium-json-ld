package com.apicatalog.jsonld.context;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.grammar.Keywords;

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
        InverseContext result = new InverseContext();
        
        // 2.
        String defaultLanguage = activeContext.getDefaultLanguage() != null
                                    ? activeContext.getDefaultLanguage().toLowerCase()
                                    : Keywords.NONE;
        
        // 3
        for (String term : activeContext.getTerms().stream().sorted().collect(Collectors.toList())) {
        
            TermDefinition termDefinition = activeContext.getTerm(term);
            
            // 3.1.
            if (termDefinition == null) {   //TODO does active context contain null values??
                continue;
            }
            
            // 3.2.
            String container = Keywords.NONE;
            
            if (termDefinition.getContainerMapping() != null 
                    && !termDefinition.getContainerMapping().isEmpty()) {
                
                container = termDefinition.getContainerMapping().stream().sorted().collect(Collectors.joining());
            }
            
            // 3.3.
            String variable = termDefinition.uriMapping;
            
            // 3.4.-5.
            Map<String, Map<String, Map<String, String>>> containerMap = result.getValue(variable);
            
            if (containerMap == null) {
                containerMap = new LinkedHashMap<>();
                result.add(variable, containerMap);
            }
            
            // 3.6.-9.
            Map<String, Map<String, String>> typeLanguageMap = containerMap.get(container);
            
            Map<String, String> languageMap = null;
            
            Map<String, String> typeMap = null;
            
            if (typeLanguageMap == null) {
                //TODO create class, get rid of Map, refactor this whole block 
                typeLanguageMap = new LinkedHashMap<>();
                
                languageMap = new LinkedHashMap<>();
                typeLanguageMap.put(Keywords.LANGUAGE, languageMap);
                
                typeMap = new LinkedHashMap<>();
                typeLanguageMap.put(Keywords.TYPE, typeMap);
                
                Map<String, String> anyMap =new LinkedHashMap<>();
                anyMap.put(Keywords.NONE, term);
                
                typeLanguageMap.put(Keywords.ANY, anyMap);
                
                containerMap.put(container, typeLanguageMap);
                
            } else {
                
                languageMap = typeLanguageMap.get(Keywords.LANGUAGE);
                typeMap = typeLanguageMap.get(Keywords.TYPE);
            }

            // 3.10.
            if (termDefinition.isReverseProperty()) {
                //TODO
            
            // 3.11.
            } else if (Keywords.NONE.equals(termDefinition.getTypeMapping()) ) {
                
            // 3.12.
            } else if (termDefinition.getTypeMapping() != null) {
                
            // 3.13.
            } else if (termDefinition.getLanguageMapping() != null && termDefinition.getDirectionMapping() != null) {
                
            // 3.14.
                //TODO
            
            // 3.17.
            } else {
                
                // 3.17.1.
                if (!languageMap.containsKey(defaultLanguage)) {
                    languageMap.put(defaultLanguage, term);
                }
                
                // 3.17.2.
                if (!languageMap.containsKey(Keywords.NONE)) {
                    languageMap.put(Keywords.NONE, term);
                }
                
                // 3.17.2.
                if (!typeMap.containsKey(Keywords.NONE)) {
                    typeMap.put(Keywords.NONE, term);
                }
            }
        }
        
        return result;
    }
    
}