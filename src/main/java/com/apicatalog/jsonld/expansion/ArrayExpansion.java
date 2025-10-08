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

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.node.ListNode;

import jakarta.json.JsonArray;
import jakarta.json.JsonValue;

/**
 * Implements Step 5 of the JSON-LD
 * <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 * Algorithm</a>.
 *
 * <p>
 * This class handles the expansion of a {@link JsonArray}. It encapsulates the
 * state and logic for this specific operation. It is used by creating an
 * instance with an active context and the array to be expanded, setting
 * optional flags through its fluent API, and then calling the {@link #expand()}
 * method.
 * </p>
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 */
public final class ArrayExpansion {

    // mandatory
    private Context context;
    private JsonArray element;
    private String property;
    private URI baseUrl;

    // optional
    private boolean frameExpansion;
    private boolean ordered;
    private boolean fromMap;

    private ArrayExpansion(
            final Context context,
            final JsonArray element,
            final String property,
            final URI baseUrl) {
        this.context = context;
        this.element = element;
        this.property = property;
        this.baseUrl = baseUrl;

        // default values
        this.frameExpansion = false;
        this.ordered = false;
        this.fromMap = false;
    }

    /**
     * Creates a new, configurable instance to expand a {@link JsonArray}.
     *
     * @param context  the active context
     * @param element  the {@link JsonArray} to expand
     * @param property the active property that the array is associated with
     * @param baseUrl  the base URL for resolving relative IRIs
     * @return a new {@code ArrayExpansion} instance ready for configuration and
     *         execution
     */
    public static final ArrayExpansion with(final Context context, final JsonArray element,
            final String property, final URI baseUrl) {
        return new ArrayExpansion(context, element, property, baseUrl);
    }

    /**
     * Executes the array expansion logic.
     * <p>
     * This method implements the following steps from the Expansion Algorithm:
     * </p>
     * <ul>
     * <li><b>5.2.</b> Iterates through each item in the input array.</li>
     * <li><b>5.2.1.</b> Recursively expands each item by invoking the main
     * Expansion algorithm.</li>
     * <li><b>5.2.2.</b> If the active property has a {@code @list} container
     * mapping and the expanded item is an array, it is converted into a list
     * object.</li>
     * <li><b>5.2.3.</b> Appends the expanded item(s) to the result list, filtering
     * out any {@code null} values and flattening nested arrays.</li>
     * <li><b>5.3.</b> Returns the resulting collection.</li>
     * </ul>
     *
     * @return a {@link Collection} containing the expanded values, which may
     *         include {@link java.util.Map}s, {@link String}s, or other JSON-LD
     *         node representations.
     * @throws JsonLdError if an error occurs during the recursive expansion of an
     *                     item
     */
    public Collection<?> expand() throws JsonLdError {

        final List<Object> result = new ArrayList<>(element.size());

        // 5.2.
        for (final JsonValue item : element) {

            context.runtime().tick();

            // 5.2.1
            Object expanded = Expansion
                    .with(context, item, property, baseUrl)
                    .frameExpansion(frameExpansion)
                    .ordered(ordered)
                    .fromMap(fromMap)
                    .compute();

            // 5.2.2
            if (expanded instanceof Collection<?> list
                    && context.getTerm(property)
                            .map(TermDefinition::getContainerMapping)
                            .filter(c -> c.contains(Keywords.LIST)).isPresent()) {

                expanded = ListNode.toList(list);
            }

            // 5.2.3
            if (expanded instanceof Collection<?> collection) {
                collection.stream()
                        .filter(Objects::nonNull)
                        .forEach(result::add);

                // append non-null element
            } else if (expanded != null) {
                result.add(expanded);
            }

        }

        // 5.3
        return result;
    }

    /**
     * Sets the {@code frameExpansion} flag.
     *
     * @param value if {@code true}, indicates that expansion is being performed as
     *              part of the JSON-LD Framing algorithm.
     * @return this instance, for method chaining.
     */
    public ArrayExpansion frameExpansion(boolean value) {
        this.frameExpansion = value;
        return this;
    }

    /**
     * Sets the {@code ordered} flag.
     *
     * @param value if {@code true}, properties and values are processed in lexical
     *              order.
     * @return this instance, for method chaining.
     */
    public ArrayExpansion ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    /**
     * Sets the {@code fromMap} flag.
     *
     * @param value if {@code true}, indicates that the expanded element originates
     *              from a map (JSON object).
     * @return this instance, for method chaining.
     */
    public ArrayExpansion fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }
}
