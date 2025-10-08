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
package com.apicatalog.jsonld.expansion;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonNumber;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value
 *      Expansion Algorithm</a>
 *
 */
public final class ValueExpansion {

    /**
     * Expand a JSON value according to the active context and active property.
     *
     * @param activeProperty the active property name
     * @param value          the input JSON value
     * @return expanded value as a map (keys are JSON-LD keywords)
     * @throws JsonLdError when expansion fails (passed through from context
     *                     helpers)
     */
    public static Map<String, ?> expand(final Context activeContext, final String activeProperty, final JsonValue value) throws JsonLdError {

        final Optional<TermDefinition> definition = activeContext.getTerm(activeProperty);

        final Optional<String> typeMapping = definition.map(TermDefinition::getTypeMapping);

        // 1.
        if (typeMapping.filter(Keywords.ID::equals).isPresent()) {

            String idValue = null;

            if (value instanceof JsonString jsonString) {
                idValue = jsonString.getString();

                // custom extension allowing to process numeric ids
            } else if (activeContext.runtime().isNumericId()
                    && value instanceof JsonNumber jsonNumber) {
                idValue = jsonNumber.toString();
            }

            if (idValue != null) {
                final String expandedValue = activeContext.uriExpansion()
                        .documentRelative(true)
                        .vocab(false)
                        .expand(idValue);

                return Map.of(Keywords.ID, expandedValue);
            }

            // 2.
        } else if (typeMapping.filter(Keywords.VOCAB::equals).isPresent()
                && value instanceof JsonString jsonString) {

            return Map.of(Keywords.ID, activeContext.uriExpansion()
                    .documentRelative(true)
                    .vocab(true)
                    .expand(jsonString.getString()));
        }

        // 4.
        if (typeMapping
                .filter(t -> !Keywords.ID.equals(t)
                        && !Keywords.VOCAB.equals(t)
                        && !Keywords.NONE.equals(t))
                .isPresent()) {

            return Map.of(
                    Keywords.TYPE, typeMapping.get(),
                    Keywords.VALUE, JsonUtils.getScalar(value));
        }

        if (value instanceof JsonString jsonString) {

            var map = new HashMap<String, Object>(3);
            map.put(Keywords.VALUE, jsonString.getString());

            // 5.1.
            var language = definition
                    .map(TermDefinition::getLanguageMapping)
                    .orElseGet(activeContext::getDefaultLanguage);

            // 5.3.
            if (language != null) {
                map.put(Keywords.LANGUAGE, language);
            }

            // 5.2.
            var direction = definition
                    .map(TermDefinition::getDirectionMapping)
                    .orElseGet(activeContext::getDefaultBaseDirection);

            // 5.4.
            if (direction != null && !DirectionType.NULL.equals(direction)) {
                map.put(Keywords.DIRECTION, direction.name().toLowerCase());
            }

            return map;
        }

        // 6.
        return Map.of(Keywords.VALUE, JsonUtils.getScalar(value));
    }
}
