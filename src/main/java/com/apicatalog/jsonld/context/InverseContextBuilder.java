/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.context;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;

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
        for (final String termName : activeContext.getTerms().stream().sorted().collect(Collectors.toList())) {
        
            // 3.1.
            if (!activeContext.getTerm(termName).isPresent()) {
                continue;
            }

            // 3.3.
            final Optional<String> variable = activeContext.getTerm(termName).map(TermDefinition::getUriMapping);
            
            if (!variable.isPresent()) {
                continue;
            }
            
            final String variableValue = variable.get();
            
            final Optional<Collection<String>> containerMapping = 
                                                    activeContext
                                                        .getTerm(termName)
                                                        .map(TermDefinition::getContainerMapping);

            // 3.2.
            final String container = containerMapping
                                            .filter(collection -> !collection.isEmpty())
                                            .orElse(Arrays.asList(Keywords.NONE))
                                            .stream()
                                            .sorted()
                                            .collect(Collectors.joining());
            
            result.setIfAbsent(variableValue, container, Keywords.ANY, Keywords.NONE, termName);

            final Optional<JsonValue> languageMapping = activeContext
                                                            .getTerm(termName)
                                                            .map(TermDefinition::getLanguageMapping);
            
            final Optional<DirectionType> directionMapping = activeContext
                                                                .getTerm(termName)
                                                                .map(TermDefinition::getDirectionMapping);

            // 3.10.
            if (activeContext.getTerm(termName).map(TermDefinition::isReverseProperty).orElse(false)) {

                // 3.10.1
                result.setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.REVERSE, termName);
            
                continue;
            } 
            
            final Optional<String> typeMapping = activeContext.getTerm(termName).map(TermDefinition::getTypeMapping);
            
            // 3.11.
            if (typeMapping.filter(Keywords.NONE::equals).isPresent()) {
                
                // 3.11.1.
                result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.ANY, termName);
                // 3.11.2.
                result.setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.ANY, termName);

                continue;
            }
                        
            // 3.12.
            if (typeMapping.isPresent()) {
                
                // 3.12.1
                result.setIfAbsent(
                                variableValue, 
                                container, 
                                Keywords.TYPE, 
                                typeMapping.get(),
                                termName
                                );
                continue;
            } 
            
            // 3.13.
            if (languageMapping.isPresent() && directionMapping.isPresent()) {
         
                // 3.13.1.
                final String langDir;
                
                // 3.13.2.
                if (languageMapping.filter(JsonUtils::isString).isPresent()
                        && directionMapping.get() != DirectionType.NULL
                        ) {
                    
                    langDir = languageMapping
                                    .map(JsonString.class::cast)
                                    .map(JsonString::getString)
                                    .orElse("")
                                    .concat("_")
                                    .concat(directionMapping.get().name())
                                    .toLowerCase();

                // 3.13.3.
                } else if (languageMapping.filter(JsonUtils::isString).isPresent()) {
                    
                    langDir = ((JsonString)languageMapping.get()).getString().toLowerCase();
                    
                // 3.13.4.
                } else if (directionMapping.get() != DirectionType.NULL) {
                    
                    langDir = "_".concat(directionMapping.get().name().toLowerCase());
                    
                } else {
                    
                    langDir = Keywords.NULL; 
                }
                
                // 3.13.5.
                result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, langDir, termName);
                
                continue;
            } 
            
            // 3.14.
            if (languageMapping.isPresent()) {

                /// 3.14.1.
                final String language = JsonUtils.isString(languageMapping.get())
                                            ? ((JsonString)languageMapping.get()).getString().toLowerCase()
                                            : Keywords.NULL;
                              
                // 3.14.2.
                result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, language, termName);
                
                continue;
            } 
            
            // 3.15.
            if (directionMapping.isPresent()) {

                // 3.15.1.
                final String direction = (directionMapping.get() != DirectionType.NULL)
                                            ? "_".concat(directionMapping.get().name().toLowerCase())
                                            : Keywords.NONE;

                // 3.15.2.
                result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, direction, termName);

                continue;
            } 
            
            // 3.16.
            if (activeContext.getDefaultBaseDirection() != null) {
                                
                // 3.16.1.
                final String langDir = (activeContext.getDefaultLanguage() != null 
                                            ? activeContext.getDefaultLanguage()
                                            : ""
                                            )
                                    .concat("_")
                                    .concat(activeContext.getDefaultBaseDirection().name())
                                    .toLowerCase();

                // 3.16.2.
                result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, langDir, termName);
                // 3.16.3.
                result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.NONE, termName);
                // 3.16.4.
                result.setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.NONE, termName);
                
                continue;
            }

            // 3.17.1.
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, defaultLanguage, termName);
            // 3.17.2.                    
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.NONE, termName);
            // 3.17.2.                    
            result.setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.NONE, termName);
        }
        
        // 4.
        return result;
    }
}
