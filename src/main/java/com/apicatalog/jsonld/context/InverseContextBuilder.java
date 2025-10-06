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

import java.util.Optional;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 * Inverse Context Creation (simplified / Java 17)
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#inverse-context-creation">
 *      Inverse Context Creation</a>
 */
public final class InverseContextBuilder {

    private final ActiveContext activeContext;

    private InverseContextBuilder(final ActiveContext activeContext) {
        this.activeContext = activeContext;
    }

    public static InverseContextBuilder with(final ActiveContext activeContext) {
        return new InverseContextBuilder(activeContext);
    }

    public InverseContext build() {

        final InverseContext result = new InverseContext();

        final String defaultLanguage = Optional.ofNullable(activeContext.getDefaultLanguage())
                .map(String::toLowerCase)
                .orElse(Keywords.NONE);

        activeContext.getTerms().stream()
                .filter(termName -> activeContext.getTerm(termName).map(TermDefinition::getUriMapping).isPresent())
                .sorted()
                .forEach(termName -> {
                    final var termOpt = activeContext.getTerm(termName);
                    final var variableValue = termOpt.map(TermDefinition::getUriMapping).orElseThrow();
                    processTerm(termName, termOpt, result, variableValue, defaultLanguage);
                });

        return result;
    }

    private void processTerm(
            final String termName,
            final Optional<TermDefinition> termOpt,
            final InverseContext result,
            final String variableValue,
            final String defaultLanguage) {

        // container: sorted concatenation or Keywords.NONE
        final String container = termOpt
                .map(TermDefinition::getContainerMapping)
                .filter(c -> !c.isEmpty())
                .map(c -> c.stream().sorted().collect(Collectors.joining()))
                .orElse(Keywords.NONE);

        result.setIfAbsent(variableValue, container, Keywords.ANY, Keywords.NONE, termName);

        // reverse property
        if (termOpt.filter(TermDefinition::isReverseProperty).isPresent()) {
            result.setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.REVERSE, termName);
            return;
        }

        final Optional<String> typeMapping = termOpt.map(TermDefinition::getTypeMapping);

        if (typeMapping.filter(Keywords.NONE::equals).isPresent()) {
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.ANY, termName)
                    .setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.ANY, termName);
            return;
        }

        if (typeMapping.isPresent()) {
            result.setIfAbsent(variableValue, container, Keywords.TYPE, typeMapping.get(), termName);
            return;
        }

        final Optional<JsonValue> languageMapping = termOpt.map(TermDefinition::getLanguageMapping);
        final Optional<DirectionType> directionMapping = termOpt.map(TermDefinition::getDirectionMapping);

        if (languageMapping.isPresent()) {

            final String langDir = langDirOf(languageMapping.get(), directionMapping);
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, langDir, termName);

        } else if (directionMapping.isPresent()) {

            final String direction = directionMapping
                    .filter(d -> d != DirectionType.NULL)
                    .map(d -> "_".concat(d.name().toLowerCase()))
                    .orElse(Keywords.NONE);

            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, direction, termName);

        } else {
            final String langDir = Optional.ofNullable(activeContext.getDefaultBaseDirection())
                    .map(d -> (Optional.ofNullable(activeContext.getDefaultLanguage()).orElse(""))
                            .concat("_")
                            .concat(d.name()))
                    .map(String::toLowerCase)
                    .orElse(defaultLanguage);

            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, langDir, termName)
                    .setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.NONE, termName)
                    .setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.NONE, termName);
        }
    }

    /**
     * Build language + direction token according to spec rules: - if language is a
     * string: - if direction present and not NULL -> "lang_dir" - else -> "lang" -
     * if language is not a string: - if direction present and not NULL -> "_dir" -
     * else -> Keywords.NULL
     */
    private static String langDirOf(final JsonValue language, final Optional<DirectionType> directionOpt) {

        final boolean hasDirection = directionOpt.filter(d -> d != DirectionType.NULL).isPresent();
        final DirectionType direction = directionOpt.orElse(DirectionType.NULL);

        if (JsonUtils.isString(language)) {
            final String lang = ((JsonString) language).getString();
            if (hasDirection) {
                return (lang + "_" + direction.name()).toLowerCase();
            }
            return lang.toLowerCase();
        }

        if (hasDirection) {
            return ("_" + direction.name()).toLowerCase();
        }

        return Keywords.NULL;
    }
}
