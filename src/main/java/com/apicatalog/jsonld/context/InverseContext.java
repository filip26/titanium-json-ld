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
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.lang.Keywords;

/**
 * InverseContext stores mapping keyed by (variable, container, type) -> (key ->
 * value)
 */
public final class InverseContext {

    private record Key(String variable, String container, String type) {
    }

    private final Set<String> variables;
    private final Map<Key, Map<String, String>> mapping;

    private InverseContext() {
        this.variables = new HashSet<>();
        this.mapping = new LinkedHashMap<>();
    }

    public boolean contains(final String variable) {
        return variables.contains(variable);
    }

    public boolean contains(String variable, String container, String type) {
        return mapping.containsKey(new Key(variable, container, type));
    }

    public String get(String variable, String container, String type, String key) {
        var inner = mapping.get(new Key(variable, container, type));
        if (inner != null) {
            return inner.get(key);
        }
        return null;
    }

    /**
     *
     * @see <a href=
     *      "https://www.w3.org/TR/json-ld11-api/#inverse-context-creation">Inverse
     *      Context Creation</a>
     *
     */
    static InverseContext newContext(ActiveContext activeContext) {

        // 1.
        final InverseContext result = new InverseContext();

        // 2.
        final String defaultLanguage = activeContext.defaultLanguage() != null
                ? activeContext.defaultLanguage().toLowerCase()
                : Keywords.NONE;
        // 3
        activeContext.terms()
                .stream()
                .filter(termName -> activeContext
                        .findTerm(termName)
                        .map(TermDefinition::uriMapping)
                        .isPresent())
                .sorted()
                .forEach(termName -> processTerm(
                        activeContext,
                        termName,
                        result,
                        activeContext
                                .findTerm(termName)
                                .map(TermDefinition::uriMapping)
                                .get(),
                        defaultLanguage));

        // 4.
        return result;
    }

    private static void processTerm(
            final ActiveContext activeContext,
            final String termName,
            final InverseContext result,
            final String variableValue,
            final String defaultLanguage) {

        // 3.2.
        final String container = activeContext
                .findTerm(termName)
                .map(TermDefinition::containerMapping)
                .filter(collection -> !collection.isEmpty())
                .orElseGet(() -> Arrays.asList(Keywords.NONE))
                .stream()
                .sorted()
                .collect(Collectors.joining());

        result.setIfAbsent(variableValue, container, Keywords.ANY, Keywords.NONE, termName);

        // 3.10.
        if (activeContext.findTerm(termName).filter(TermDefinition::isReverseProperty).isPresent()) {

            // 3.10.1
            result.setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.REVERSE, termName);
            return;
        }

        final Optional<String> typeMapping = activeContext.findTerm(termName).map(TermDefinition::typeMapping);

        // 3.11.
        if (typeMapping.filter(Keywords.NONE::equals).isPresent()) {

            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.ANY, termName)
                    .setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.ANY, termName);
            return;
        }

        // 3.12.
        if (typeMapping.isPresent()) {

            // 3.12.1
            result.setIfAbsent(variableValue, container, Keywords.TYPE, typeMapping.get(), termName);
            return;
        }

        final var languageMapping = activeContext
                .findTerm(termName)
                .map(TermDefinition::languageMapping);

        final var directionMapping = activeContext
                .findTerm(termName)
                .map(TermDefinition::directionMapping);

        if (languageMapping.isPresent()) {

            // 3.13.1.
            final String langDir;

            final String language = languageMapping.get();

            // 3.13.
            if (directionMapping.isPresent()) {

                final Direction direction = directionMapping.get();

                // 3.13.2.
                if (language != null) {

                    if (direction != Direction.NULL) {

                        langDir = language
                                .concat("_")
                                .concat(direction.name())
                                .toLowerCase();
                        // 3.13.3.
                    } else {
                        langDir = language.toLowerCase();
                    }

                    // 3.13.4.
                } else if (direction != Direction.NULL) {

                    langDir = "_".concat(direction.name().toLowerCase());

                } else {
                    langDir = Keywords.NULL;
                }

            } else {
                langDir = language != null
                        ? language.toLowerCase()
                        : Keywords.NULL;
            }

            // 3.13.5.
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, langDir, termName);

            // 3.15.
        } else if (directionMapping.isPresent()) {

            // 3.15.1.
            final String direction = directionMapping
                    .filter(d -> d != Direction.NULL)
                    .map(d -> "_".concat(d.name().toLowerCase()))
                    .orElse(Keywords.NONE);

            // 3.15.2.
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, direction, termName);

            // 3.16.
        } else {
            final String langDir = activeContext.defaultBaseDirection() != null
                    ? (activeContext.defaultLanguage() != null
                            ? activeContext.defaultLanguage()
                            : "")
                            .concat("_")
                            .concat(activeContext.defaultBaseDirection().name())
                            .toLowerCase()
                    : defaultLanguage;

            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, langDir, termName)
                    .setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.NONE, termName)
                    .setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.NONE, termName);
        }
    }

    private InverseContext setIfAbsent(
            String variable,
            String container,
            String type,
            String key,
            String value) {
        variables.add(variable);
        mapping.computeIfAbsent(new Key(variable, container, type), k -> new LinkedHashMap<>())
                .putIfAbsent(key, value);
        return this;
    }
}
