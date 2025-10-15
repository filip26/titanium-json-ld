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

import java.io.IOException;
import java.util.Map;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;

import jakarta.json.JsonValue;

/**
 * Implements the JSON-LD Expansion algorithm steps for scalar values.
 * <p>
 * This class handles steps 4.1 through 4.3 of the algorithm.
 * </p>
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 */
final class ScalarExpansion {

    /**
     * Expands a scalar value based on the active context and property.
     * <p>
     * This method implements the following steps from the Expansion Algorithm:
     * </p>
     * <ul>
     * <li><b>4.1.</b> If the {@code property} is {@code null} or {@code "@graph"},
     * the free-floating scalar is dropped by returning {@code null}.</li>
     * <li><b>4.2.</b> If a {@code propertyContext} is defined, a new active context
     * is created by processing it. The value is then expanded against this new
     * context.</li>
     * <li><b>4.3.</b> The scalar {@code element} is expanded by invoking the
     * <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value
     * Expansion</a> algorithm.</li>
     * </ul>
     *
     * @param context         the active context
     * @param property        the active property, which is the key associated with
     *                        the {@code element}
     * @param propertyContext a property-scoped context to apply, or {@code null} if
     *                        none
     * @param element         the scalar {@link JsonValue} to expand
     * @return a {@link Map} representing the expanded value object, or {@code null}
     *         if the scalar is dropped
     * @throws JsonLdError if an error occurs during expansion
     * @throws IOException 
     */
    public static Map<String, ?> expand(
            final Context context,
            final String property,
            final JsonValue propertyContext,
            final JsonValue element) throws JsonLdError, IOException {

        /*
         * 4.1. If active property is null or @graph, drop the free-floating scalar by
         * returning null.
         */
        if (property == null || Keywords.GRAPH.equals(property)) {
            return null;
        }

        /*
         * 4.2. If property-scoped context is defined, set active context to the result
         * of the Context Processing algorithm, passing active context, property-scoped
         * context as local context, and base URL from the term definition for active
         * property in active context.
         */
        if (propertyContext != null) {
            return context
                    .newContext()
                    .create(propertyContext,
                            JakartaAdapter.instance(),
                            context.getTerm(property)
                                    .map(TermDefinition::getBaseUrl)
                                    .orElse(null))
                    .expandValue(property, element);
        }

        /*
         * 4.3. Return the result of the Value Expansion algorithm, passing the active
         * context, active property, and element as value.
         */
        return context.expandValue(property, element);
    }
}
