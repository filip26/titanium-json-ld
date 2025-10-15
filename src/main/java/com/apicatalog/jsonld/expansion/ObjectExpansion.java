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
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.node.ValueNode;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.NodeAdapter;

import jakarta.json.JsonObject;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public final class ObjectExpansion {

    // mandatory
    private Context activeContext;
    private PolyNode propertyContext;
    private Object element;
    private NodeAdapter adapter;
    private String activeProperty;
    private URI baseUrl;

    // optional
    private boolean frameExpansion;
    private boolean ordered;
    private boolean fromMap;

    private ObjectExpansion(final Context activeContext,
            final PolyNode propertyContext,
            final Object element, final NodeAdapter adapter,
            final String activeProperty, final URI baseUrl) {
        this.activeContext = activeContext;
        this.propertyContext = propertyContext;
        this.element = element;
        this.adapter = adapter;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        // default values
        this.frameExpansion = false;
        this.ordered = false;
        this.fromMap = false;
    }

    public static final ObjectExpansion with(final Context activeContext,
            final PolyNode propertyContext,
            final Object node,
            final NodeAdapter nodeAdapter,
            final String activeProperty,
            final URI baseUrl) {
        return new ObjectExpansion(activeContext,
                propertyContext,
                node, nodeAdapter, activeProperty, baseUrl);
    }

    public Object expand() throws JsonLdError, IOException {

        initPreviousContext();

        // 8. init property context
        if (propertyContext != null) {
            activeContext = activeContext
                    .newContext()
                    .overrideProtected(true)
                    .create(
                            propertyContext.node(),
                            propertyContext.adapter(),
                            activeContext
                                    .getTerm(activeProperty)
                                    .map(TermDefinition::getBaseUrl)
                                    .orElse(null));
        }

        initLocalContext();

        // 10.
        final Context typeContext = activeContext;

        final String typeKey = processTypeScoped(typeContext);

        final String inputType = findInputType(typeKey);

        final Map<String, Object> result = new LinkedHashMap<>();

        ObjectExpansion1314
                .with()
                .inputType(inputType)
                .result(result)
                .typeContext(typeContext)
                .nest(new LinkedHashMap<>())
                .frameExpansion(frameExpansion)
                .ordered(ordered)
                .expand(activeContext, (JsonObject) element, adapter, activeProperty, baseUrl);

        // 15.
        if (result.containsKey(Keywords.VALUE)) {
            return normalizeValue(result);

            // 16.
        } else if (result.containsKey(Keywords.TYPE)) {
            return normalizeType(result);

            // 17.
        } else if (result.containsKey(Keywords.LIST) || result.containsKey(Keywords.SET)) {
            return normalizeContainer(result);
        }

        return normalize(result);
    }

    public ObjectExpansion frameExpansion(boolean value) {
        this.frameExpansion = value;
        return this;
    }

    public ObjectExpansion ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ObjectExpansion fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }

    private void initPreviousContext() throws JsonLdError, IOException {

        // 7. If active context has a previous context, the active context is not
        // propagated.
        // If from map is undefined or false, and element does not contain an entry
        // expanding to @value,
        // and element does not consist of a single entry expanding to @id (where
        // entries are IRI expanded),
        // set active context to previous context from active context,
        // as the scope of a term-scoped context does not apply when processing new node
        // objects.
        if (activeContext.getPreviousContext() != null && !fromMap) {

            boolean revert = true;

            var it = adapter.keyStream(element, PolyNode.comparingElement(adapter::stringValue)).iterator();

            while (it.hasNext()) {
//final String key : Utils.index(element.keySet(), true)
                activeContext.runtime().tick();

                var key = adapter.stringValue(it.next());

                final String expandedKey = activeContext
                        .uriExpansion()
                        .vocab(true)
                        .expand(key);

                if (Keywords.VALUE.equals(expandedKey)
                        || (Keywords.ID.equals(expandedKey) && (adapter.isSingleElement(element)))) {
                    revert = false;
                    break;
                }
            }

            if (revert) {
                activeContext = activeContext.getPreviousContext();
            }
        }
    }

    private void initLocalContext() throws JsonLdError, IOException {
        // 9.
        var contextValue = adapter.property(Keywords.CONTEXT, element);

        if (contextValue != null) {
            activeContext = activeContext
                    .newContext()
                    .create(contextValue, adapter, baseUrl);
        }
    }

    private String processTypeScoped(final Context typeContext) throws JsonLdError, IOException {

        String typeKey = null;

        var it = adapter.keyStream(element, PolyNode.comparingElement(adapter::stringValue)).iterator();

        // 11.
        while (it.hasNext()) {
//            for (var key : Utils.index(adapter.keys(element), true)) {            

            var key = adapter.stringValue(it.next());

            activeContext.runtime().tick();

            var expandedKey = activeContext
                    .uriExpansion()
                    .vocab(true)
                    .expand(key);

            if (!Keywords.TYPE.equals(expandedKey)) {
                continue;

            } else if (typeKey == null) {
                typeKey = key;
            }

            // 11.2
            final Iterator<String> terms = adapter.asStream(adapter.property(key, element))
//                    .toStream(element.get(key))
                    .filter(adapter::isString)
                    .map(adapter::stringValue)
                    .sorted()
                    .iterator();

            while (terms.hasNext()) {

                activeContext.runtime().tick();

                final String term = terms.next();

                final Optional<PolyNode> localContext = typeContext
                        .getTerm(term).map(TermDefinition::getLocalContext);

                if (localContext.isPresent()) {
                    activeContext = activeContext
                            .newContext()
                            .propagate(false)
                            .create(localContext.get().node(),
                                    localContext.get().adapter(),
                                    activeContext.getTerm(term)
                                            .map(TermDefinition::getBaseUrl)
                                            .orElse(null));
                }
            }
        }

        return typeKey;
    }

    private String findInputType(final String typeKey) throws JsonLdError, IOException {

        // Initialize input type to expansion of the last value of the first entry in
        // element
        // expanding to @type (if any), ordering entries lexicographically by key. Both
        // the key and
        // value of the matched entry are IRI expanded.
        if (typeKey != null) {

            var type = adapter.property(typeKey, element);

            if (adapter.isCollection(type)) {

                var lastValue = adapter.elementStream(type)
                        .filter(adapter::isString)
                        .map(adapter::stringValue)
                        .sorted()
                        .reduce((first, second) -> second);

                if (lastValue.isPresent()) {
                    return activeContext.uriExpansion()
                            .vocab(true)
                            .expand(lastValue.get());
                }

            } else if (adapter.isString(type)) {
                return activeContext.uriExpansion()
                        .vocab(true)
                        .expand(adapter.stringValue(type));
            }
        }

        return null;
    }

    private Map<String, ?> normalizeValue(final Map<String, ?> result) throws JsonLdError {

        // 15.1.
        if (ValueNode.isNotValueNode(result)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
        }

        if ((result.containsKey(Keywords.DIRECTION) || result.containsKey(Keywords.LANGUAGE))
                && result.containsKey(Keywords.TYPE)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT, "Invalid @value [" + result + "]");
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

                // 15.4
            } else if (!frameExpansion
                    && !(value instanceof String) && result.containsKey(Keywords.LANGUAGE)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_VALUE);

                // 15.5
            } else if (!frameExpansion
                    && type != null
                    && (!(type instanceof String uri) || UriUtils.isNotURI(uri))) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TYPED_VALUE, "Invalid @type [" + type + "].");
            }
        }
        return normalize(result);
    }

    private Map<String, ?> normalizeType(final Map<String, Object> result) throws JsonLdError {

        var type = result.get(Keywords.TYPE);

        if (!(type instanceof Collection)) {
            result.put(Keywords.TYPE, Set.of(type));
        }

        return normalize(result);
    }

    private Object normalizeContainer(final Map<String, ?> result) throws JsonLdError {

        // 17.1.
        if (result.size() > 2 || result.size() == 2 && !result.containsKey(Keywords.INDEX)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_SET_OR_LIST_OBJECT, "Invalid object [" + result + "].");
        }

        // 17.2.
        var set = result.get(Keywords.SET);

        if (set instanceof Map<?, ?> rawMap) {
            @SuppressWarnings("unchecked")
            var map = (Map<String, Object>) rawMap;
            return normalize(map);

        } else if (set != null) {
            return set;
        }

        return normalize(result);
    }

    private Map<String, ?> normalize(final Map<String, ?> result) throws JsonLdError {

        // Extension: JSON-LD-STAR (Experimental)
//        if (result.containsKey(Keywords.ANNOTATION)
//                && (StringUtils.isBlank(activeProperty)
//                        || Keywords.GRAPH.equals(activeProperty)
//                        || Keywords.INCLUDED.equals(activeProperty)
//                        || result.get(Keywords.ANNOTATION).filter(NodeObject::isNotAnnotationObject).isPresent())) {
//            throw new JsonLdError(JsonLdErrorCode.INVALID_ANNOTATION);
//        }

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