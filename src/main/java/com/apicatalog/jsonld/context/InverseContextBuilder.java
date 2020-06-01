package com.apicatalog.jsonld.context;

import java.util.LinkedHashMap;
import java.util.Map;
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

                // 3.10.1
                if (!typeMap.containsKey(Keywords.REVERSE)) {
                    typeMap.put(Keywords.REVERSE, term);
                }
            
            // 3.11.
            } else if (Keywords.NONE.equals(termDefinition.getTypeMapping()) ) {
                
                // 3.11.1.
                if (!languageMap.containsKey(Keywords.ANY)) {
                    languageMap.put(Keywords.ANY, term);
                }

                // 3.11.2.
                if (!typeMap.containsKey(Keywords.ANY)) {
                    typeMap.put(Keywords.ANY, term);
                }

            // 3.12.
            } else if (termDefinition.getTypeMapping() != null) {

                // 3.12.1
                if (!typeMap.containsKey(termDefinition.getTypeMapping())) {
                    typeMap.put(termDefinition.getTypeMapping(), term);
                }

                
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
                if (!languageMap.containsKey(langDir)) {
                    languageMap.put(langDir, term);
                }
                
            // 3.14.
            } else if (termDefinition.getLanguageMapping() != null) {

                /// 3.14.1.
                String language = Keywords.NULL;
                
                if (JsonUtils.isString(termDefinition.getLanguageMapping())) {
                    language = ((JsonString)termDefinition.getLanguageMapping()).getString().toLowerCase();
                }
                              
                // 3.14.2.
                if (!languageMap.containsKey(language)) {
                    
                    languageMap.put(language, term);
                }

                
            // 3.15.
            } else if (termDefinition.getDirectionMapping() != null) {

                // 3.15.1.
                String direction = Keywords.NONE;
                
                if (termDefinition.getDirectionMapping() != DirectionType.NULL) {
                    direction = "_".concat(termDefinition.getDirectionMapping().name().toLowerCase());
                }

                // 3.15.2.
                if (!languageMap.containsKey(direction)) {
                    languageMap.put(direction, term);
                }

            // 3.16.
            } else if (activeContext.getDefaultBaseDirection() != null) {
                
                // 3.16.1.
                String langDir = (activeContext.getDefaultLanguage() != null 
                                    ? activeContext.getDefaultLanguage()
                                    : ""
                                    )
                                    .concat("_")
                                    .concat(activeContext.getDefaultBaseDirection().name())
                                    .toLowerCase();
                
                // 3.16.2.
                if (!languageMap.containsKey(langDir)) {
                    languageMap.put(langDir, term);
                }

                // 3.16.3.
                if (!languageMap.containsKey(Keywords.NONE)) {
                    languageMap.put(Keywords.NONE, term);
                }
                
                // 3.16.4.
                if (!typeMap.containsKey(Keywords.NONE)) {
                    typeMap.put(Keywords.NONE, term);
                }
                
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
        
        // 4.
        return result;
    }
    
}
