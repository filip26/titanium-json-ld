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
 * Implements the JSON-LD Value Expansion algorithm.
 *
 * <p>
 * This is a utility class that provides a single static method to expand a JSON
 * value.
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value
 *      Expansion Algorithm</a>
 */
public final class ValueExpansion {

    /**
     * Expands a JSON value based on the rules of the Value Expansion algorithm.
     *
     * @param context  The active context to use for expansion.
     * @param property The active property being expanded, which determines which
     *                 term definition to apply.
     * @param value    The JSON value to expand.
     * @return A {@code Map} representing the expanded value object, typically
     *         containing keys like {@code @id}, {@code @value}, {@code @type},
     *         {@code @language}, or {@code @direction}.
     * @throws JsonLdError If an error occurs during IRI expansion.
     * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value
     *      Expansion Algorithm</a>
     */
    public static Map<String, ?> expand(final Context context, final String property, final JsonValue value) throws JsonLdError {

        final Optional<TermDefinition> definition = context.getTerm(property);

        final String typeMapping = definition
                .map(TermDefinition::getTypeMapping)
                .orElse(Keywords.NONE);

        switch (typeMapping) {
        case Keywords.ID:
            String idValue = null;

            if (value instanceof JsonString jsonString) {
                idValue = jsonString.getString();

                // custom extension allowing to process numeric ids
            } else if (context.runtime().isNumericId()
                    && value instanceof JsonNumber jsonNumber) {
                idValue = jsonNumber.toString();
            }

            if (idValue != null) {
                return Map.of(Keywords.ID, context.uriExpansion()
                        .documentRelative(true)
                        .vocab(false)
                        .expand(idValue));
            }
            break;

        case Keywords.VOCAB:
            if (value instanceof JsonString jsonString) {
                return Map.of(Keywords.ID, context.uriExpansion()
                        .documentRelative(true)
                        .vocab(true)
                        .expand(jsonString.getString()));
            }
            break;

        case Keywords.NONE:
            break;

        // type mapping is not ID, VOCAB, NONE
        default:
            return Map.of(
                    Keywords.TYPE, typeMapping,
                    Keywords.VALUE, JsonUtils.asScalar(value));

        }

        if (value instanceof JsonString jsonString) {

            var map = new HashMap<String, String>(3);
            map.put(Keywords.VALUE, jsonString.getString());

            // 5.1.
            var language = definition
                    .map(TermDefinition::getLanguageMapping)
                    .orElseGet(context::getDefaultLanguage);

            // 5.3.
            if (language != null && !Keywords.NULL.equals(language)) {
                map.put(Keywords.LANGUAGE, language);
            }

            // 5.2.
            var direction = definition
                    .map(TermDefinition::getDirectionMapping)
                    .orElseGet(context::getDefaultBaseDirection);

            // 5.4.
            if (direction != null && !DirectionType.NULL.equals(direction)) {
                map.put(Keywords.DIRECTION, direction.name().toLowerCase());
            }

            return map;
        }

        // 6.
        return Map.of(Keywords.VALUE, JsonUtils.asScalar(value));
    }
}
