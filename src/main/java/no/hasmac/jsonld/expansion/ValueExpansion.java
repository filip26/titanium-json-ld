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
package no.hasmac.jsonld.expansion;

import java.util.Optional;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.context.TermDefinition;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.DirectionType;
import no.hasmac.jsonld.lang.Keywords;

import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value
 *      Expansion Algorithm</a>
 *
 */
public final class ValueExpansion {

    // required
    private final ActiveContext activeContext;

    // runtime
    private Optional<TermDefinition> definition;

    private ValueExpansion(final ActiveContext activeContext) {
        this.activeContext = activeContext;
    }

    public static ValueExpansion with(final ActiveContext activeContext) {
        return new ValueExpansion(activeContext);
    }

    public JsonObject expand(final JsonValue value, final String activeProperty) throws JsonLdError {

        definition = activeContext.getTerm(activeProperty);

        final Optional<String> typeMapping = definition.map(TermDefinition::getTypeMapping);

        if (typeMapping.isPresent()) {

            // 1.
            if (Keywords.ID.equals(typeMapping.get())) {

                String idValue = null;

                if (JsonUtils.isString(value)) {
                    idValue = ((JsonString) value).getString();

                // custom extension allowing to process numeric ids
                } else if (activeContext.getOptions().isNumericId() && JsonUtils.isNumber(value)) {
                    idValue = ((JsonNumber) value).toString();
                }

                if (idValue != null) {
                    final String expandedValue = activeContext.uriExpansion().documentRelative(true)
                            .vocab(false).expand(idValue);

                    return JsonProvider.instance().createObjectBuilder().add(Keywords.ID, expandedValue).build();
                }

            // 2.
            } else if (Keywords.VOCAB.equals(typeMapping.get()) && JsonUtils.isString(value)) {

                String expandedValue = activeContext.uriExpansion().documentRelative(true)
                        .vocab(true).expand(((JsonString) value).getString());

                return JsonProvider.instance().createObjectBuilder().add(Keywords.ID, expandedValue).build();
            }
        }

        // 3.
        final JsonObjectBuilder result = JsonProvider.instance().createObjectBuilder().add(Keywords.VALUE, value);

        // 4.
        if (typeMapping
                    .filter(t -> !Keywords.ID.equals(t) && !Keywords.VOCAB.equals(t) && !Keywords.NONE.equals(t))
                    .isPresent()) {

            result.add(Keywords.TYPE, typeMapping.get());

            // 5.
        } else if (JsonUtils.isString(value)) {
            buildStringValue(result);
        }

        // 6.
        return result.build();
    }

    private void buildStringValue(final JsonObjectBuilder result) {

        // 5.1.
        final JsonValue language = definition
                                            .map(TermDefinition::getLanguageMapping)
                                            .orElseGet(() -> activeContext.getDefaultLanguage() != null
                                                                ? JsonProvider.instance().createValue(activeContext.getDefaultLanguage())
                                                                : null);
        // 5.2.
        final DirectionType direction = definition
                                            .map(TermDefinition::getDirectionMapping)
                                            .orElseGet(() -> activeContext.getDefaultBaseDirection());

        // 5.3.
        if (JsonUtils.isNotNull(language)) {
            result.add(Keywords.LANGUAGE, language);
        }

        // 5.4.
        if (direction != null && !DirectionType.NULL.equals(direction)) {
            result.add(Keywords.DIRECTION, JsonProvider.instance().createValue(direction.name().toLowerCase()));
        }
    }
}
