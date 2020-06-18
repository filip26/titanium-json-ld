package com.apicatalog.jsonld.context;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.json.JsonString;
import javax.json.JsonValue;

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

            // 3.3.
            final Optional<String> variable = activeContext.getTerm(term).map(TermDefinition::getUriMapping);
            
            final Optional<Collection<String>> containerMapping = 
                                                    activeContext
                                                        .getTerm(term)
                                                        .map(TermDefinition::getContainerMapping);

            // 3.2.
            final String container = containerMapping
                                            .filter(Predicate.not(Collection::isEmpty))
                                            .orElse(Arrays.asList(Keywords.NONE))
                                            .stream()
                                            .sorted()
                                            .collect(Collectors.joining());
            
            variable.ifPresent(v -> result.setIfAbsent(v, container, Keywords.ANY, Keywords.NONE, term));

            final Optional<JsonValue> languageMapping = activeContext
                                                            .getTerm(term)
                                                            .map(TermDefinition::getLanguageMapping);
            
            final Optional<DirectionType> directionMapping = activeContext
                                                                .getTerm(term)
                                                                .map(TermDefinition::getDirectionMapping);

            // 3.10.
            if (activeContext.getTerm(term).map(TermDefinition::isReverseProperty).orElse(false)) {

                // 3.10.1
                variable.ifPresent(v -> result.setIfAbsent(v, container, Keywords.TYPE, Keywords.REVERSE, term));
            
            // 3.11.
            } else if (activeContext.getTerm(term)
                                    .map(TermDefinition::getTypeMapping)
                                    .map(Keywords.NONE::equals).orElse(false)) {
                
                variable.ifPresent(v -> {
                    // 3.11.1.
                    result.setIfAbsent(v, container, Keywords.LANGUAGE, Keywords.ANY, term);
                    // 3.11.2.
                    result.setIfAbsent(v, container, Keywords.TYPE, Keywords.ANY, term);
                });

            // 3.12.
            } else if (activeContext.getTerm(term).map(TermDefinition::getTypeMapping).isPresent()) {

                // 3.12.1
                variable.ifPresent(v -> result.setIfAbsent(
                                                    v, 
                                                    container, 
                                                    Keywords.TYPE, 
                                                    activeContext
                                                            .getTerm(term)
                                                            .map(TermDefinition::getTypeMapping)
                                                            .get(),
                                                    term
                                                )
                                    );

            // 3.13.
            } else if (languageMapping.isPresent() && directionMapping.isPresent()) {
         
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
                variable.ifPresent(v -> result.setIfAbsent(v, container, Keywords.LANGUAGE, langDir, term));
                
            // 3.14.
            } else if (languageMapping.isPresent()) {

                /// 3.14.1.
                final String language = JsonUtils.isString(languageMapping.get())
                                            ? ((JsonString)languageMapping.get()).getString().toLowerCase()
                                            : Keywords.NULL;
                              
                // 3.14.2.
                variable.ifPresent(v -> result.setIfAbsent(v, container, Keywords.LANGUAGE, language, term));
                
            // 3.15.
            } else if (directionMapping.isPresent()) {

                // 3.15.1.
                final String direction = (directionMapping.get() != DirectionType.NULL)
                                            ? "_".concat(directionMapping.get().name().toLowerCase())
                                            : Keywords.NONE;

                // 3.15.2.
                variable.ifPresent(v -> result.setIfAbsent(v, container, Keywords.LANGUAGE, direction, term));

            // 3.16.
            } else if (activeContext.getDefaultBaseDirection() != null) {
                                
                variable.ifPresent(v -> {
                    
                    // 3.16.1.
                    final String langDir = (activeContext.getDefaultLanguage() != null 
                                                ? activeContext.getDefaultLanguage()
                                                : ""
                                                )
                                        .concat("_")
                                        .concat(activeContext.getDefaultBaseDirection().name())
                                        .toLowerCase();
    
                    // 3.16.2.
                    result.setIfAbsent(v, container, Keywords.LANGUAGE, langDir, term);
                    // 3.16.3.
                    result.setIfAbsent(v, container, Keywords.LANGUAGE, Keywords.NONE, term);
                    // 3.16.4.
                    result.setIfAbsent(v, container, Keywords.TYPE, Keywords.NONE, term);
                });
                
            // 3.17.
            } else {
                
                variable.ifPresent(v -> {
                    // 3.17.1.
                    result.setIfAbsent(v, container, Keywords.LANGUAGE, defaultLanguage, term);
                    // 3.17.2.                    
                    result.setIfAbsent(v, container, Keywords.LANGUAGE, Keywords.NONE, term);
                    // 3.17.2.                    
                    result.setIfAbsent(v, container, Keywords.TYPE, Keywords.NONE, term);
                });
            }
        }
        
        // 4.
        return result;
    }
}
