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
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.LdAdapter;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.processor.Execution;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.NodeType;
import com.apicatalog.tree.io.PolyNode;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public final class Expansion {

    public record Params(
            boolean frameExpansion,
            boolean fromMap,
            URI baseUrl,
            Options options,
            Execution runtime) {

        /**
         * The {@code frameExpansion} flag.
         *
         * @return {@code true}, indicates that expansion is being performed as part of
         *         the JSON-LD Framing algorithm.
         */
        @Override
        public boolean frameExpansion() {
            return frameExpansion;
        }
//
//    /**
//     * Sets the {@code ordered} flag.
//     *
//     * @param value if {@code true}, properties and values are processed in lexical
//     *              order.
//     * @return this instance, for method chaining.
//     */
//    public ArrayExpansion ordered(boolean value) {
//        this.ordered = value;
//        return this;
//    }
//
//    /**
//     * Sets the {@code fromMap} flag.
//     *
//     * @param value if {@code true}, indicates that the expanded element originates
//     *              from a map (JSON object).
//     * @return this instance, for method chaining.
//     */
//    public ArrayExpansion fromMap(boolean value) {
//        this.fromMap = value;
//        return this;
//    }
    }

    public static final Object expand(
            final Context activeContext,
            final Object node,
            final NodeAdapter nodeAdapter,
            final String activeProperty,
            final Params params

    ) throws JsonLdException, IOException {

        // 1. If element is null, return null
        if (nodeAdapter.isNull(node)) {
            return null;
        }

        var nodeType = nodeAdapter.type(node);

        // 5. If element is an array,
        if (nodeType == NodeType.COLLECTION) {
            return array(activeContext, node, nodeAdapter, activeProperty, params);
        }

        // 3. If active property has a term definition in active context with a local
        // context, initialize property-scoped context to that local context.
        var propertyContext = activeContext
                .findTerm(activeProperty)
                .map(TermDefinition::getLocalContext)
                .orElse(null);

        // 4. If element is a scalar
        if (nodeType.isScalar()) {
            return scalar(activeContext,
                    activeProperty,
                    propertyContext,
                    node,
                    nodeAdapter,
                    params);
        }

        // 6. Otherwise element is a map
        return new ObjectExpansion(activeContext,
                propertyContext,
                node,
                nodeAdapter,
                activeProperty,
                new Params(
                        params.frameExpansion && !Keywords.DEFAULT.equals(activeProperty),
                        params.fromMap,
                        params.baseUrl,
                        params.options,
                        params.runtime))
                .expand();
    }

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
     * @param node            the scalar to expand
     * @return a {@link Map} representing the expanded value object, or {@code null}
     *         if the scalar is dropped
     * @throws JsonLdException if an error occurs during expansion
     * @throws IOException
     */
    static final Map<String, ?> scalar(
            final Context context,
            final String property,
            final PolyNode propertyContext,
            final Object node,
            final NodeAdapter nodeAdapter,
            final Params params) throws JsonLdException, IOException {

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
            return ValueExpansion.expand(context
                    .newContext(params.options().loader())
                    .build(propertyContext.node(),
                            propertyContext.adapter(),
                            context.findTerm(property)
                                    .map(TermDefinition::getBaseUrl)
                                    .orElse(null)),
                    property,
                    node,
                    nodeAdapter,
                    params.options());
        }

        /*
         * 4.3. Return the result of the Value Expansion algorithm, passing the active
         * context, active property, and element as value.
         */
        return ValueExpansion.expand(context, property, node, nodeAdapter, params.options());
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
     * @throws JsonLdException if an error occurs during the recursive expansion of an
     *                     item
     * @throws IOException
     */
    static final Collection<?> array(final Context context,
            final Object node,
            final NodeAdapter nodeAdapter,
            final String property,
            final Params params) throws JsonLdException, IOException {

        if (nodeAdapter.isEmptyCollection(node)) {
            return List.of();
        }

        final var result = new ArrayList<Object>();

        // 5.2.
        for (final var item : nodeAdapter.asIterable(node)) {

            params.runtime().tick();

            // 5.2.1
            var expanded = expand(context, item, nodeAdapter, property, params);

            // 5.2.2
            if (expanded instanceof Collection<?> list
                    && context.findTerm(property)
                            .map(TermDefinition::getContainerMapping)
                            .filter(c -> c.contains(Keywords.LIST)).isPresent()) {

                expanded = LdAdapter.toList(list);
            }

            // 5.2.3
            if (expanded instanceof Collection<?> collection) {
                collection.stream()
                        .filter(Objects::nonNull)
                        .forEach(result::add);

            } else if (expanded != null) {
                // append non-null element
                result.add(expanded);
            }
        }

        // 5.3
        return result;
    }
}