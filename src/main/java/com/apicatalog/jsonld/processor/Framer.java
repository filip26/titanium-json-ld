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

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.compaction.Compaction;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.framing.Frame;
import com.apicatalog.jsonld.framing.Framing;
import com.apicatalog.jsonld.framing.FramingState;
import com.apicatalog.jsonld.json.JsonProvider;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;
import com.apicatalog.tree.io.jakarta.JakartaMaterializer;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.tree.io.traverse.Visitor;

import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-framing/#dom-jsonldprocessor-frame">JsonLdProcessor.frame()</a>
 *
 */
public final class Framer {

    private Framer() {
    }

    public static final JsonObject frame(final URI input, final Document frame, final JsonLdOptions options) throws JsonLdError, IOException {
        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + input + "].");
        }

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Returned document is null [" + input + "].");
        }

        return frame(remoteDocument, frame, options);
    }

    public static final JsonObject frame(final Document input, final URI frameUri, final JsonLdOptions options) throws JsonLdError, IOException {
        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + frameUri + "].");
        }

        final Document frameDocument = options.getDocumentLoader().loadDocument(frameUri, new LoaderOptions());

        if (frameDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Returned frame is null [" + frameUri + "] is null.");
        }

        return frame(input, frameDocument, options);
    }

    public static final JsonObject frame(final Document input, final Document frame, final JsonLdOptions options) throws JsonLdError, IOException {

        if (frame == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame or Frame.Document is null.");
        }

        final JsonStructure frameStructure;

        if (frame instanceof JsonDocument x) {
            frameStructure = x.getJsonContent()
                    .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame is not JSON object but null."));
        } else {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame is not JSON object but null.");
        }

        if (JsonUtils.isNotObject(frameStructure)) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame is not JSON object but [" + frameStructure + "].");
        }

        final JsonObject frameObject = frameStructure.asJsonObject();

        // 4.
        final JsonLdOptions expansionOptions = new JsonLdOptions(options);

        expansionOptions.setOrdered(false);

        var expandedInput = Expander.expand(input, expansionOptions, false);
//        JsonArray expandedInput = JsonValue.EMPTY_JSON_ARRAY;
//        var a = new JakartaMaterializer().node(expandedInput, NativeAdapter.instance());
        // 7.
        System.out.println("FX " + ((JsonDocument)frame).getContent().node());
        var expandedFrame = Expander.expand(frame, expansionOptions, true);

      new Visitor().root(expandedFrame, NativeAdapter.instance()).traverse(
      
      v -> {
          System.out.println(v.node() + ", " + v.nodeType() + ", " + v.nodeContext() + ", " + v.node().getClass());
      }
      
      );

        
     
        JsonValue context = JsonValue.EMPTY_JSON_OBJECT;

        if (frameObject.containsKey(Keywords.CONTEXT)) {
            context = frameObject.get(Keywords.CONTEXT);
        }

        // 9.
        final URI contextBase = (frame.getContextUrl() != null)
                ? frame.getDocumentUrl()
                : options.getBase();

        // 10-11.
        final ActiveContext activeContext = new ActiveContext(input.getDocumentUrl(), input.getDocumentUrl(), ProcessingRuntime.of(options))
                .newContext()
                .build(context, JakartaAdapter.instance(), contextBase);

        final String graphKey = activeContext.uriCompaction().vocab(true).compact(Keywords.GRAPH);

        // 13.
        boolean frameDefault = false;
        for (final String key : frameObject.keySet()) {
            if (key.equals(graphKey)) {
                frameDefault = true;
                break;
            }
        }

        // 14.
        final FramingState state = new FramingState();

        state.setEmbed(options.getEmbed()); // 14.1.
        state.setEmbedded(false); // 14.2.
        state.setExplicitInclusion(options.isExplicit()); // 14.3.
        state.setRequireAll(options.isRequiredAll()); // 14.4.
        state.setOmitDefault(options.isOmitDefault()); // 14.5.

        state.setGraphMap(new NodeMapBuilder(expandedInput, new NodeMap()).build()); // 14.7.

        if (frameDefault) {
            state.setGraphName(Keywords.DEFAULT); // 14.6.

        } else {
            state.setGraphName(Keywords.MERGED);
            state.getGraphMap().merge();
        }

        // Build reverse property index for efficient lookups
        state.setReversePropertyIndex(buildReversePropertyIndex(state.getGraphMap()));

        // 15.
        final var resultMap = new LinkedHashMap<String, Object>();
//        final var resultMap = MapBuilder.create();


        // 16.
        Framing.with(state,
                new ArrayList<>(state.getGraphMap().subjects(state.getGraphName())),
                Frame.of(expandedFrame),
                resultMap,
                null)
                .ordered(options.isOrdered())
                .frame();

        Stream<?> result = null;

        // 17. - remove blank @id
        if (!activeContext.runtime().isV10()) {

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

        var xy = new JakartaMaterializer().node(filtered, NativeAdapter.instance());
        System.out.println(expandedInput);
        System.out.println(resultMap.values());
        System.out.println(xy);
        // 19.
        // FIXME
        JsonValue compactedResults = Compaction
                .with(activeContext)
                .compactArrays(options.isCompactArrays())
                .ordered(options.isOrdered())
                .compact(xy);

        // 19.1.
        if (JsonUtils.isEmptyArray(compactedResults)) {
            compactedResults = JsonValue.EMPTY_JSON_OBJECT;

            // 19.2.
        } else if (JsonUtils.isArray(compactedResults)) {

            compactedResults = JsonProvider.instance().createObjectBuilder()
                    .add(graphKey, compactedResults).build();

        }

        // 20.
        compactedResults = replaceNull(compactedResults);

        final boolean omitGraph;

        if (options.isOmitGraph() == null) {

            omitGraph = activeContext.runtime().isV11();

        } else {
            omitGraph = options.isOmitGraph();
        }

        // 21.
        if (!omitGraph && !compactedResults.asJsonObject().containsKey(graphKey)) {
            if (compactedResults.asJsonObject().isEmpty()) {

                compactedResults = JsonProvider.instance().createObjectBuilder().add(graphKey,
                        JsonValue.EMPTY_JSON_ARRAY).build();

            } else {

                compactedResults = JsonProvider.instance().createObjectBuilder().add(graphKey,
                        JsonProvider.instance().createArrayBuilder().add(compactedResults)).build();
            }
        }

        // 19.3.
        if (!JsonUtils.isEmptyArray(context) && !JsonUtils.isEmptyObject(context)) {
            compactedResults = JsonProvider.instance().createObjectBuilder(compactedResults.asJsonObject()).add(Keywords.CONTEXT, context).build();
        }

        return compactedResults.asJsonObject();
    }

    public static final JsonObject frame(final URI input, final URI frame, final JsonLdOptions options) throws JsonLdError, IOException {
        return frame(getDocument(input, options), getDocument(frame, options), options);
    }

    private static Document getDocument(final URI document, final JsonLdOptions options) throws JsonLdError {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + document + "].");
        }

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(document, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Cannot load document [" + document + "].");
        }

        return remoteDocument;
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
                            //((Collection<?>) entry.getValue()).iterator().next();
                }

                object.put(entry.getKey(), removePreserve(entry.getValue()));
            }

            return object;
        }

        return value;
    }

    private static final JsonValue replaceNull(JsonValue value) {

        if (JsonUtils.isString(value) && Keywords.NULL.equals(((JsonString) value).getString())) {
            return JsonValue.NULL;

        } else if (JsonUtils.isScalar(value)) {
            return value;

        } else if (JsonUtils.isArray(value)) {

            final JsonArrayBuilder array = JsonProvider.instance().createArrayBuilder();

            value.asJsonArray().forEach(item -> array.add(replaceNull(item)));

            final JsonArray result = array.build();

            return result.size() != 1 || JsonUtils.isNotNull(result.get(0)) ? result : JsonValue.EMPTY_JSON_ARRAY;
        }

        final JsonObjectBuilder object = JsonProvider.instance().createObjectBuilder();

        value.asJsonObject().entrySet().forEach(entry -> object.add(entry.getKey(), replaceNull(entry.getValue())));

        return object.build();
    }

    private static final Object removeBlankIdKey(Object value, List<String> blankNodes) {

        if (value instanceof Collection<?> array) {
            return array.stream().map(item -> removeBlankIdKey(item, blankNodes)).toList();
//            final JsonArrayBuilder array = JsonProvider.instance().createArrayBuilder();
//
//            value.asJsonArray().forEach(item -> array.add(removeBlankIdKey(item, blankNodes)));
//
//            return array.build();
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

        final Map<String, Map<String, Map<String, Set<String>>>> index = new HashMap<>();

        for (final String graphName : graphMap.graphs()) {

            final Map<String, Map<String, Set<String>>> graphIndex = index.computeIfAbsent(graphName, k -> new HashMap<>());

            for (final String subject : graphMap.subjects(graphName)) {

                // TODO
                final var node = (Map<String, ?>) graphMap.find(graphName, subject).orElse(Collections.emptyMap());

                if (node == null) {
                    continue;
                }

                for (final var propEntry : node.entrySet()) {

                    final String property = propEntry.getKey();

                    if (Keywords.contains(property)) {
                        continue;
                    }

                    final Object value = propEntry.getValue();

                    if (value instanceof Collection<?> items) {

                        for (final var item : items) {

                            if (item instanceof Map map && map.containsKey(Keywords.ID)) {

                                final var targetId = map.get(Keywords.ID).toString(); // TODO ?!

                                graphIndex
                                        .computeIfAbsent(property, k -> new HashMap<>())
                                        .computeIfAbsent(targetId, k -> new HashSet<>())
                                        .add(subject);
                            }
                        }

                    } else if (value instanceof Map map && map.containsKey(Keywords.ID)) {

                        final var targetId = (String) map.get(Keywords.ID);

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
