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

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.Direction;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.expansion.Expansion.Params;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.TreeAdapter;

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
     * @param adapter
     * @param params
     * @return A {@code Map} representing the expanded value object, typically
     *         containing keys like {@code @id}, {@code @value}, {@code @type},
     *         {@code @language}, or {@code @direction}.
     * @throws JsonLdException If an error occurs during URI expansion.
     * 
     * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value
     *      Expansion Algorithm</a>
     */
    public static Map<String, ?> expand(
            final Context context,
            final String property,
            final Object value,
            final TreeAdapter adapter,
            final Params params) throws JsonLdException {

        final var definition = context.findTerm(property);

        final var typeMapping = definition
                .map(TermDefinition::getTypeMapping)
                .orElse(Keywords.NONE);

        switch (typeMapping) {
        case Keywords.ID:
            
            String idValue = null;
            
            if (adapter.isString(value)) {
                idValue = adapter.stringValue(value);

            } else
            // custom extension allowing to process numeric ids
            if (params.options().useNumericId() && adapter.isNumber(value)) {
                idValue = adapter.asString(value);
            }

            if (idValue != null) {

                final var id = UriExpansion.with(context, params.options().loader(), params.runtime())
                        .documentRelative(true)
                        .vocab(false)
                        .expand(idValue);
                
                return Map.of(Keywords.ID, id);
            }
            break;

        case Keywords.VOCAB:
            if (adapter.isString(value)) {
                final var id = UriExpansion.with(context, params.options().loader(), params.runtime())
                        .documentRelative(true)
                        .vocab(true)
                        .expand(adapter.stringValue(value));

                return Map.of(Keywords.ID, id);
            }
            break;

        case Keywords.NONE:
            break;

        // type mapping is not ID, VOCAB, NONE
        default:
            return Map.of(
                    Keywords.TYPE, typeMapping,
                    Keywords.VALUE, asScalar(value, adapter));
        }

        if (adapter.isString(value)) {

            var map = new HashMap<String, String>(3);
            map.put(Keywords.VALUE, adapter.stringValue(value));

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
            if (direction != null && !Direction.NULL.equals(direction)) {
                map.put(Keywords.DIRECTION, direction.name().toLowerCase());
            }

            return map;
        }

        // 6.
        return Map.of(Keywords.VALUE, asScalar(value, adapter));
    }

    private static Object asScalar(Object node, TreeAdapter adapter) {
        if (node == null) {
            return null;
        }

        switch (adapter.type(node)) {
        case NULL:
            return null;

        case FALSE:
            return false;

        case TRUE:
            return true;

        case NUMBER:
            return adapter.numericValue(node);

        case STRING:
            return adapter.stringValue(node);

        default:
            return adapter.asString(node);
        }
    }
}
