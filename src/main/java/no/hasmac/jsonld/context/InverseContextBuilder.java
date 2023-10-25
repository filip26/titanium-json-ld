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
package no.hasmac.jsonld.context;

import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.DirectionType;
import no.hasmac.jsonld.lang.Keywords;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#inverse-context-creation">Inverse Context Creation</a>
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

        // 1.
        final InverseContext result = new InverseContext();

        // 2.
        final String defaultLanguage = activeContext.getDefaultLanguage() != null
                ? activeContext.getDefaultLanguage().toLowerCase()
                : Keywords.NONE;
        // 3

        Collection<String> terms = activeContext.getTerms();


        ArrayList<String> sortedFilteredTerms = new ArrayList<>(terms.size());
        for (String termName : terms) {
            TermDefinition term = activeContext.getTermNullable(termName);
            if (term != null && term.getUriMapping() != null) {
                sortedFilteredTerms.add(termName);
            }

        }

        sortedFilteredTerms.sort(String::compareTo);

        for (String termName : sortedFilteredTerms) {
            processTerm(
                    termName,
                    result,
                    activeContext.getTerm(termName).map(TermDefinition::getUriMapping).get(),
                    defaultLanguage);
        }

        // 4.
        return result;
    }

    private void processTerm(final String termName, InverseContext result, final String variableValue, final String defaultLanguage) {

        // 3.2.


        List<String> listToSort = new ArrayList<>();
        for (String s : activeContext
                .getTerm(termName)
                .map(TermDefinition::getContainerMapping)
                .filter(collection -> !collection.isEmpty())
                .orElseGet(() -> Arrays.asList(Keywords.NONE))) {
            listToSort.add(s);
        }
        listToSort.sort(null);
        StringBuilder sb1 = new StringBuilder();
        for (String s : listToSort) {
            sb1.append(s);
        }
        final String container = sb1.toString();

        result.setIfAbsent(variableValue, container, Keywords.ANY, Keywords.NONE, termName);

        // 3.10.
        if (activeContext.getTerm(termName).filter(TermDefinition::isReverseProperty).isPresent()) {

            // 3.10.1
            result.setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.REVERSE, termName);

            return;
        }

        final Optional<String> typeMapping = activeContext.getTerm(termName).map(TermDefinition::getTypeMapping);

        // 3.11.
        if (typeMapping.filter(Keywords.NONE::equals).isPresent()) {

            result
                    .setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.ANY, termName)
                    .setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.ANY, termName);

            return;
        }

        // 3.12.
        if (typeMapping.isPresent()) {

            // 3.12.1
            result.setIfAbsent(variableValue, container, Keywords.TYPE, typeMapping.get(), termName);
            return;
        }

        final Optional<JsonValue> languageMapping =
                activeContext
                        .getTerm(termName)
                        .map(TermDefinition::getLanguageMapping);

        final Optional<DirectionType> directionMapping =
                activeContext
                        .getTerm(termName)
                        .map(TermDefinition::getDirectionMapping);


        if (languageMapping.isPresent()) {

            // 3.13.1.
            final String langDir;

            final JsonValue language = languageMapping.get();

            // 3.13.
            if (directionMapping.isPresent()) {

                final DirectionType direction = directionMapping.get();

                // 3.13.2.
                if (JsonUtils.isString(language)) {

                    if (direction != DirectionType.NULL) {

                        langDir = ((JsonString) language).getString()
                                .concat("_")
                                .concat(direction.name())
                                .toLowerCase();
                        // 3.13.3.
                    } else {
                        langDir = ((JsonString) language).getString().toLowerCase();
                    }

                    // 3.13.4.
                } else if (direction != DirectionType.NULL) {

                    langDir = "_".concat(direction.name().toLowerCase());

                } else {
                    langDir = Keywords.NULL;
                }

            } else {
                langDir = JsonUtils.isString(language)
                        ? ((JsonString) language).getString().toLowerCase()
                        : Keywords.NULL;
            }

            // 3.13.5.
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, langDir, termName);

            // 3.15.
        } else if (directionMapping.isPresent()) {

            // 3.15.1.
            final String direction = directionMapping
                    .filter(d -> d != DirectionType.NULL)
                    .map(d -> "_".concat(d.name().toLowerCase()))
                    .orElse(Keywords.NONE);

            // 3.15.2.
            result.setIfAbsent(variableValue, container, Keywords.LANGUAGE, direction, termName);

            // 3.16.
        } else {
            final String langDir = activeContext.getDefaultBaseDirection() != null

                    ? (activeContext.getDefaultLanguage() != null
                    ? activeContext.getDefaultLanguage()
                    : ""
            )
                    .concat("_")
                    .concat(activeContext.getDefaultBaseDirection().name())
                    .toLowerCase()

                    : defaultLanguage;

            result
                    .setIfAbsent(variableValue, container, Keywords.LANGUAGE, langDir, termName)
                    .setIfAbsent(variableValue, container, Keywords.LANGUAGE, Keywords.NONE, termName)
                    .setIfAbsent(variableValue, container, Keywords.TYPE, Keywords.NONE, termName);
        }
    }
}
