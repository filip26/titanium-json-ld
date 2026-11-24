/*
 * Copyright 2020-2026 the original author or authors.
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Predicate;

import com.apicatalog.jsonld.lang.Keywords;

/**
 * InverseContext stores mapping keyed by (variable, container, type) -> (key ->
 * value)
 */
public final class InverseContext {

    private static record Key(String variable, String container, String type) {
    }

    private final Set<String> variables;
    private final Map<Key, Map<String, String>> mapping;

    private InverseContext() {
        this.variables = new HashSet<>();
        this.mapping = new HashMap<>();
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
    static InverseContext newInstance(ActiveContext activeContext) {

        // 1.
        final var result = new InverseContext();

        if (!activeContext.terms().isEmpty()) {

            // 3
            final var termMapping = new ArrayList<Entry<String, String>>();

            for (var term : activeContext.terms()) {

                var mapping = activeContext.findTerm(term).map(TermDefinition::uriMapping);

                if (mapping.isPresent()) {
                    termMapping.add(Map.entry(term, mapping.get()));
                }
            }

            if (!termMapping.isEmpty()) {

                final var defaultLanguage = activeContext.defaultLanguage() != null
                        ? activeContext.defaultLanguage().toLowerCase()
                        : Keywords.NONE;

                termMapping.sort(Map.Entry.comparingByKey());
                termMapping.forEach(mapping -> processTerm(
                        activeContext,
                        mapping.getKey(),
                        result,
                        mapping.getValue(),
                        defaultLanguage));
            }
        }
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
        final var containerMapping = activeContext
                .findTerm(termName)
                .map(TermDefinition::containerMapping)
                .filter(Predicate.not(Collection::isEmpty));

        final String container;

        if (containerMapping.isPresent()) {

            var sorted = new ArrayList<>(containerMapping.get());
            Collections.sort(sorted);
            container = String.join("", sorted);

        } else {
            container = Keywords.NONE;
        }

        result.setIfAbsent(variableValue, container, Keywords.ANY, Keywords.NONE, termName);

        // 3.10.
        if (activeContext.findTerm(termName).filter(TermDefinition::isReverseProperty).isPresent()) {

            // 3.10.1
            result.setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.REVERSE, termName);
            return;
        }

        final var typeMapping = activeContext
                .findTerm(termName)
                .map(TermDefinition::typeMapping);

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

        final var termDefinition = activeContext.findTerm(termName);

        final var languageMapping = termDefinition.map(TermDefinition::languageMapping);

        final var directionMapping = termDefinition.map(TermDefinition::directionMapping);

        if (languageMapping.isPresent()) {

            // 3.13.1.
            final String langDir;

            final var language = languageMapping.get();

            // 3.13.
            if (directionMapping.isPresent()) {

                final Direction direction = directionMapping.get();

                // 3.13.2.
                if (language != null) {

                    if (direction != Direction.NULL) {
                        langDir = (language + "_" + direction.name()).toLowerCase();

                    } else {
                        // 3.13.3.
                        langDir = language.toLowerCase();
                    }

                } else if (direction != Direction.NULL) {
                    // 3.13.4.
                    langDir = "_" + direction.name().toLowerCase();

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

        } else if (directionMapping.isPresent()) {

            // 3.15.1.
            final var direction = directionMapping
                    .filter(d -> d != Direction.NULL)
                    .map(d -> "_" + d.name().toLowerCase())
                    .orElse(Keywords.NONE);

            // 3.15.2.
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, direction, termName);

        } else {
            // 3.16.
            final var langDir = activeContext.defaultDirection() != null
                    ? ((activeContext.defaultLanguage() != null
                            ? activeContext.defaultLanguage()
                            : "")
                            + "_"
                            + activeContext.defaultDirection().name())
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
