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
package com.apicatalog.jsonld.processor;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.compaction.Compaction;
import com.apicatalog.jsonld.compaction.UriCompaction;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.framing.Frame;
import com.apicatalog.jsonld.framing.Framing;
import com.apicatalog.jsonld.framing.FramingState;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.runtime.Execution;
import com.apicatalog.tree.io.Tree;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-framing/#dom-jsonldprocessor-frame">JsonLdProcessor.frame()</a>
 *
 */
public final class Framer {

    public static Map<String, ?> frame(
            final Document document,
            final Document frame,
            final Options options,
            final Execution runtime) throws JsonLdException {

        final var contextNode = Context.extract(frame.content());

        return Context.inject(
                Framer.frame(
                        Framer.expand(document, options, runtime),
                        Frame.of(frame, options, runtime),
                        Framer.context(
                                document.url(),
                                contextNode,
                                Frame.contextBase(frame, options),
                                options,
                                runtime),
                        options),
                contextNode);
    }

    public static final Context context(
            final URI baseUrl,
            final Tree localContext,
            final URI localContextBase,
            final Options options,
            final Execution runtime) throws JsonLdException {

        // 10-11.
        return new Context.Builder(
                baseUrl,
                baseUrl,
                options.mode())
                .runtime(runtime)
                .loader(options.loader())
                .update(localContext, options.useInlineContexts(), localContextBase)
                .build();
    }

    public static final Collection<?> expand(
            Document document,
            final Options options,
            final Execution runtime) throws JsonLdException {
        return Expander.expand(
                document,
                Options.copyOf(options).ordered(false),
                runtime);
    }

    public static final Collection<?> expand(
            Tree document,
            final Options options,
            final Execution runtime) throws JsonLdException {
        return Expander.expand(
                document,
                Expander.context(
                        null,
                        null,
                        options,
                        runtime),
                options.base(),
                Options.copyOf(options).ordered(false),
                runtime);
    }

    public static final Map<String, ?> frame(
            final Collection<?> expanded,
            final Frame frame,
            final Context context,
            final Options options) throws JsonLdException {

        final var runtime = Execution.of(options);

        // 14.
        final var state = new FramingState();

        state.setEmbed(options.embed()); // 14.1.
        state.setEmbedded(false); // 14.2.
        state.setExplicitInclusion(options.isExplicit()); // 14.3.
        state.setRequireAll(options.isRequiredAll()); // 14.4.
        state.setOmitDefault(options.isOmitDefault()); // 14.5.
        state.setGraphMap(new NodeMapBuilder(expanded, new NodeMap()).build()); // 14.7.

        final String graphKey = UriCompaction.withVocab(context, Keywords.GRAPH, options);

        if (frame.isDefault(graphKey)) {
            state.setGraphName(Keywords.DEFAULT); // 14.6.

        } else {
            state.setGraphName(Keywords.MERGED);
            state.getGraphMap().merge();
        }

        // Build reverse property index for efficient lookups
        state.setReversePropertyIndex(buildReversePropertyIndex(state.getGraphMap()));

        // 15.
        final var resultMap = new LinkedHashMap<String, Object>();

        // 16.
        Framing.with(state,
                new ArrayList<>(state.getGraphMap().subjects(state.getGraphName())),
                frame,
                resultMap,
                null)
                .ordered(options.isOrdered())
                .frame();

        Stream<?> result = null;

        // 17. - remove blank @id
        if (!context.isV10()) {

            final var values = resultMap.values();

            final var remove = findBlankNodes(values);

            if (!remove.isEmpty()) {
                result = values.stream().map(v -> Framer.removeBlankIdKey(v, remove));

            } else {
                result = values.stream();
            }
        }

        if (result == null) {
            result = resultMap.values().stream();
        }

        // 18. - remove preserve
        final var filtered = result
                .map(Framer::removePreserve)
                .toList();

        // 19.
        var compactedOutput = Compaction
                .with(context, options, runtime)
                .compactArrays(options.isCompactArrays())
                .ordered(options.isOrdered())
                .compact(filtered);

        // 19.1.
        if (compactedOutput instanceof Collection<?> col) {

            if (col.isEmpty()) {
                compactedOutput = Map.of();

            } else {
                // 19.2.
                compactedOutput = Map.of(graphKey, compactedOutput);
            }
        }

        // 20.
        compactedOutput = replaceNull(compactedOutput);

        final var omitGraph = options.isOmitGraph() == null
                ? context.isV11()
                : options.isOmitGraph();

        // 21.
        if (!omitGraph && !((Map<?, ?>) compactedOutput).containsKey(graphKey)) {
            if (((Map<?, ?>) compactedOutput).isEmpty()) {

                compactedOutput = Map.of(
                        graphKey,
                        List.of());

            } else {
                compactedOutput = Map.of(
                        graphKey,
                        List.of(compactedOutput));
            }
        }

        // 19.3.
        if (compactedOutput instanceof Map map) {
            @SuppressWarnings("unchecked")
            final var typedMap = (Map<String, ?>) map;
            return typedMap;
        }

        throw new IllegalStateException();
    }

    private static final Object removePreserve(Object value) {

        if (value instanceof Collection<?> array) {
            return array.stream().map(Framer::removePreserve).toList();
        }

        if (value instanceof Map<?, ?> map) {

            final var object = new LinkedHashMap<Object, Object>();

            for (final var entry : map.entrySet()) {

                if (Keywords.PRESERVE.equals(entry.getKey())) {
                    return entry.getValue();
                    // ((Collection<?>) entry.getValue()).iterator().next();
                }

                object.put(entry.getKey(), removePreserve(entry.getValue()));
            }

            return object;
        }

        return value;
    }

    private static final Object replaceNull(Object node) {

        if (node == null || Keywords.NULL.equals(node)) {
            return null;
        }

        if (node instanceof Collection<?> col) {

            final var result = col.stream().map(Framer::replaceNull).toList();

            return result.size() != 1
                    || result.get(0) != null
                            ? result
                            : List.of();
        }

        if (node instanceof Map<?, ?> map) {

            final var result = new LinkedHashMap<>(map.size());

            for (var entry : map.entrySet()) {
                result.put(entry.getKey(), replaceNull(entry.getValue()));
            }

            return result;
        }

        return node;
    }

    private static final Object removeBlankIdKey(Object value, List<String> blankNodes) {

        if (value instanceof Collection<?> array) {
            return array.stream().map(item -> removeBlankIdKey(item, blankNodes)).toList();
        }

        if (value instanceof Map<?, ?> map) {

            final var object = new LinkedHashMap<Object, Object>();

            for (final var entry : map.entrySet()) {

                if (Keywords.ID.equals(entry.getKey())
                        && entry.getValue() instanceof String stringValue
                        && blankNodes.contains(stringValue)) {
                    continue;
                }

                object.put(entry.getKey(), removeBlankIdKey(entry.getValue(), blankNodes));
            }

            return object;
        }

        return value;
    }

    private static final List<String> findBlankNodes(final Collection<?> array) {

        Map<String, Integer> candidates = new HashMap<>();

        array.forEach(v -> findBlankNodes(v, candidates));

        return candidates.entrySet().stream().filter(e -> e.getValue() == 1).map(Entry::getKey).collect(Collectors.toList());
    }

    private static final void findBlankNodes(Object value, final Map<String, Integer> blankNodes) {

        if (value instanceof String string) {

            if (BlankNode.isWellFormed(string)) {
                Integer count = blankNodes.computeIfAbsent(string, x -> 0);
                blankNodes.put(string, ++count);
            }

            return;
        }
        if (value instanceof Collection<?> array) {
            array.forEach(item -> findBlankNodes(item, blankNodes));
            return;
        }

        if (value instanceof Map<?, ?> map) {
            map.values().forEach(v -> findBlankNodes(v, blankNodes));
        }
    }

    private static Map<String, Map<String, Map<String, Set<String>>>> buildReversePropertyIndex(final NodeMap graphMap) {

        final var index = new HashMap<String, Map<String, Map<String, Set<String>>>>();

        for (final var graphName : graphMap.graphs()) {

            final var graphIndex = index.computeIfAbsent(graphName, k -> new HashMap<>());

            for (final var subject : graphMap.subjects(graphName)) {

                final var node = (Map<String, ?>) graphMap
                        .find(graphName, subject)
                        .orElse(Map.of());

                if (node == null) {
                    continue;
                }

                for (final var propEntry : node.entrySet()) {

                    final var property = propEntry.getKey();

                    if (Keywords.contains(property)) {
                        continue;
                    }

                    final var value = propEntry.getValue();

                    if (value instanceof Collection<?> items) {

                        for (final var item : items) {

                            if (item instanceof Map map
                                    && map.get(Keywords.ID) instanceof String targetId) {

                                graphIndex
                                        .computeIfAbsent(property, k -> new HashMap<>())
                                        .computeIfAbsent(targetId, k -> new HashSet<>())
                                        .add(subject);
                            }
                        }

                    } else if (value instanceof Map map
                            && map.get(Keywords.ID) instanceof String targetId) {

                        graphIndex
                                .computeIfAbsent(property, k -> new HashMap<>())
                                .computeIfAbsent(targetId, k -> new HashSet<>())
                                .add(subject);
                    }
                }
            }
        }
        return index;
    }
}
