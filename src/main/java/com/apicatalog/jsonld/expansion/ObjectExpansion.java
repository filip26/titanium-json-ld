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

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.expansion.Expansion.Params;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LdAdapter;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.web.uri.UriUtils;

/**
 * 
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public final class ObjectExpansion {

    private final Object element;
    private final TreeAdapter adapter;
    private final TreeIO propertyContext;
    private final Params params;

    private Context activeContext;

    public ObjectExpansion(
            final TreeIO propertyContext,
            final Object element,
            final TreeAdapter adapter,
            final Params params) {

        this.propertyContext = propertyContext;
        this.element = element;
        this.adapter = adapter;
        this.params = params;
    }

    public Object expand(final Context context, final String property) throws JsonLdException {

        params.runtime().onBeginMap(property);

        activeContext = initPreviousContext(
                context,
                element,
                adapter,
                params);

        // 8. initialize property context
        if (propertyContext != null) {
            activeContext = activeContext
                    .newContext(params.options().loader(), params.runtime())
                    .overrideProtected(true)
                    .build(
                            propertyContext,
                            activeContext
                                    .findTerm(property)
                                    .map(TermDefinition::getBaseUrl)
                                    .orElse(null));
        }

        // 9. initialize local context
        final var contextValue = adapter.property(Keywords.CONTEXT, element);

        if (contextValue != null) {
            activeContext = activeContext
                    .newContext(params.options().loader(), params.runtime())
                    .acceptInlineContext(params.options().useInlineContexts())
                    .collectKeys(params.runtime()::onContextKeys)
                    .build(contextValue, adapter, params.baseUrl());

        }

        // 10.
        final var typeContext = activeContext;

        final var typeKey = processTypeScoped(typeContext);

        final var inputType = findInputType(typeKey);

        final var result = new LinkedHashMap<String, Object>();

        ObjectExpansion1314
                .with(params)
                .inputType(inputType)
                .result(result)
                .typeContext(typeContext)
                .nest(new LinkedHashMap<>())
                .expand(activeContext, element, adapter, property);

        params.runtime().onEndMap(property);

        // 15.
        if (result.containsKey(Keywords.VALUE)) {
            return normalizeValue(result, property, params.frameExpansion());
        }
        // 16.
        if (result.containsKey(Keywords.TYPE)) {
            return normalizeType(result, property, params.frameExpansion());
        }
        // 17.
        if (result.containsKey(Keywords.LIST) || result.containsKey(Keywords.SET)) {
            return normalizeContainer(result, property, params.frameExpansion());
        }

        return normalize(result, property, params.frameExpansion());
    }

    private static Context initPreviousContext(
            final Context context,
            final Object element,
            final TreeAdapter adapter,
            final Params params) throws JsonLdException {

        // 7. If active context has a previous context, the active context is not
        // propagated.
        // If from map is undefined or false, and element does not contain an entry
        // expanding to @value,
        // and element does not consist of a single entry expanding to @id (where
        // entries are IRI expanded),
        // set active context to previous context from active context,
        // as the scope of a term-scoped context does not apply when processing new node
        // objects.
        if (context.getPreviousContext() != null && !params.fromMap()) {

            boolean revert = true;

            var it = adapter.keyStream(element).sorted(TreeIO.comparingElement(adapter::stringValue)).iterator();

            while (it.hasNext()) {

                params.runtime().tick();

                var key = adapter.stringValue(it.next());

                final String expandedKey = UriExpansion.with(context, params.options().loader(), params.runtime())
                        .vocab(true)
                        .expand(key);

                if (Keywords.VALUE.equals(expandedKey)
                        || (Keywords.ID.equals(expandedKey) && (adapter.isSingleEntry(element)))) {
                    revert = false;
                    break;
                }
            }

            if (revert) {
                return context.getPreviousContext();
            }
        }
        return context;
    }

    private String processTypeScoped(final Context typeContext) throws JsonLdException {

        String typeKey = null;

        final var typeKeys = adapter
                .keyStream(element)
                .sorted(TreeIO.comparingElement(adapter::asString))
                .iterator();

        // 11.
        while (typeKeys.hasNext()) {

            final var key = adapter.asString(typeKeys.next());

            params.runtime().tick();

            var expandedKey = UriExpansion.with(activeContext, params.options().loader(), params.runtime())
                    .vocab(true)
                    .expand(key);

            if (Keywords.TYPE.equals(expandedKey)) {

                if (typeKey == null) {
                    typeKey = key;
                }

//                System.out.println("0- " + key + ", " + typeKey + "," + expandedKey);
                params.runtime().onTypeKey(key);

                // 11.2
                var terms = adapter.asStream(adapter.property(key, element))
                        .filter(adapter::isString)
                        .map(adapter::stringValue)
                        .sorted()
                        .iterator();

                while (terms.hasNext()) {

                    params.runtime().tick();

                    final var term = terms.next();

                    final var localContext = typeContext
                            .findTerm(term)
                            .map(TermDefinition::getLocalContext)
                            .orElse(null);

                    if (localContext != null) {

                        activeContext = activeContext
                                .newContext(params.options().loader(), params.runtime())
                                .propagate(false)
                                .collectKeys(params.runtime()::onContextKeys)
                                .build(localContext,
                                        activeContext.findTerm(term)
                                                .map(TermDefinition::getBaseUrl)
                                                .orElse(null));
                    }
                }
            }
        }

        return typeKey;
    }

    private String findInputType(final String typeKey) throws JsonLdException {

        // Initialize input type to expansion of the last value of the first entry in
        // element
        // expanding to @type (if any), ordering entries lexicographically by key. Both
        // the key and
        // value of the matched entry are IRI expanded.
        if (typeKey != null) {

            final var type = adapter.property(typeKey, element);

            if (adapter.isCollection(type)) {

                final var lastValue = adapter.elementStream(type)
                        .filter(adapter::isString)
                        .map(adapter::stringValue)
                        .sorted()
                        .reduce((first, second) -> second);

                if (lastValue.isPresent()) {
                    return UriExpansion.with(activeContext, params.options().loader(), params.runtime())
                            .vocab(true)
                            .expand(lastValue.get());
                }

            } else if (adapter.isString(type)) {
                return UriExpansion.with(activeContext, params.options().loader(), params.runtime())
                        .vocab(true)
                        .expand(adapter.stringValue(type));
            }
        }

        return null;
    }

    private static Map<String, ?> normalizeValue(
            final Map<String, ?> result,
            final String activeProperty,
            final boolean frameExpansion) throws JsonLdException {

        // 15.1.
        if (LdAdapter.isNotValueNode(result)) {
            throw new JsonLdException(ErrorCode.INVALID_VALUE_OBJECT);
        }

        if ((result.containsKey(Keywords.DIRECTION) || result.containsKey(Keywords.LANGUAGE))
                && result.containsKey(Keywords.TYPE)) {
            throw new JsonLdException(ErrorCode.INVALID_VALUE_OBJECT, "Invalid @value [" + result + "]");
        }

        // 15.2.
        var type = result.get(Keywords.TYPE);

        if (type == null
                || type instanceof Collection<?> c && !c.contains(Keywords.JSON)
                || type instanceof String s && !s.equals(Keywords.JSON)) {

            var value = result.get(Keywords.VALUE);

            // 15.3.
            if (value == null || value instanceof Collection<?> c && c.isEmpty()) {
                return null;

            } else if (!frameExpansion
                    && !(value instanceof String) && result.containsKey(Keywords.LANGUAGE)) {
                // 15.4
                throw new JsonLdException(ErrorCode.INVALID_LANGUAGE_TAGGED_VALUE);

            } else if (!frameExpansion
                    && type != null
                    && (!(type instanceof String uri) || UriUtils.isNotURI(uri))) {
                // 15.5
                throw new JsonLdException(ErrorCode.INVALID_TYPED_VALUE, "Invalid @type [" + type + "].");
            }
        }
        return normalize(result, activeProperty, frameExpansion);
    }

    private Map<String, ?> normalizeType(
            final Map<String, Object> result,
            final String activeProperty,
            final boolean frameExpansion) throws JsonLdException {

        final var type = result.get(Keywords.TYPE);

        if (!(type instanceof Collection)) {
            result.put(Keywords.TYPE, List.of(type));
        }

        return normalize(result, activeProperty, frameExpansion);
    }

    private Object normalizeContainer(
            final Map<String, ?> result,
            final String activeProperty,
            final boolean frameExpansion) throws JsonLdException {

        // 17.1.
        if (result.size() > 2 || result.size() == 2 && !result.containsKey(Keywords.INDEX)) {
            throw new JsonLdException(ErrorCode.INVALID_SET_OR_LIST_OBJECT, "Invalid object [" + result + "].");
        }

        // 17.2.
        var set = result.get(Keywords.SET);

        if (set instanceof Map<?, ?> rawMap) {
            @SuppressWarnings("unchecked")
            var map = (Map<String, Object>) rawMap;
            return normalize(map, activeProperty, frameExpansion);

        } else if (set != null) {
            return set;
        }

        return normalize(result, activeProperty, frameExpansion);
    }

    private static Map<String, ?> normalize(
            final Map<String, ?> result,
            final String activeProperty,
            final boolean frameExpansion) throws JsonLdException {

        // Extension: JSON-LD-STAR (Experimental)
        if (result.containsKey(Keywords.ANNOTATION)
                && ((activeProperty == null || activeProperty.isBlank())
                        || Keywords.GRAPH.equals(activeProperty)
                        || Keywords.INCLUDED.equals(activeProperty)
                        || !LdAdapter.isAnnotation(result.get(Keywords.ANNOTATION)))) {
            throw new JsonLdException(ErrorCode.INVALID_ANNOTATION);
        }

        // 18.
        if (result.size() == 1 && result.containsKey(Keywords.LANGUAGE)) {
            return null;
        }

        // 19.
        if (activeProperty == null || Keywords.GRAPH.equals(activeProperty)) {

            // 19.1. If result is a map which is empty, or contains only the entries @value
            // or @list, set result to null
            if (!frameExpansion && result.isEmpty()
                    || result.containsKey(Keywords.VALUE)
                    || result.containsKey(Keywords.LIST)) {
                return null;
            }

            // 19.2. if result is a map whose only entry is @id, set result to null. When
            // the frameExpansion flag is set, a map containing only the @id entry is
            // retained.
            if (!frameExpansion && result.size() == 1 && result.containsKey(Keywords.ID)) {
                return null;
            }

        }
        return result;
    }
}