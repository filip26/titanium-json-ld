package com.apicatalog.jsonld.processor;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.compaction.Compaction;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.framing.Frame;
import com.apicatalog.jsonld.framing.Framing;
import com.apicatalog.jsonld.framing.FramingState;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-framing/#dom-jsonldprocessor-frame">JsonLdProcessor.frame()</a>
 *
 */
public final class FramingProcessor {

    private FramingProcessor() {
    }
    
    public static final JsonObject frame(final RemoteDocument input, final RemoteDocument frame, final JsonLdOptions options) throws JsonLdError {

        // 4.
        final JsonLdOptions expansionOptions = new JsonLdOptions(options);
        expansionOptions.setOrdered(false);

        JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions, false);

        // 7.
        JsonArray expandedFrame = ExpansionProcessor.expand(frame, expansionOptions, true);

        // 8.
        final JsonObject frameObject = frame.getDocument().getJsonStructure().asJsonObject(); 

        JsonValue context = JsonValue.EMPTY_JSON_OBJECT;
        
        if (frameObject.containsKey(Keywords.CONTEXT)    
                ) {
            context = frameObject.get(Keywords.CONTEXT);   
        }
            
        // 9.
        final URI contextBase = (frame.getContextUrl() != null)
                                ? frame.getDocumentUrl()
                                : options.getBase();

        // 10-11.
        final ActiveContext activeContext =
                                new ActiveContext(input.getDocumentUrl(), input.getDocumentUrl(), options)
                                            .newContext()
                                            .create(context, contextBase);
        
        // 13.
        final List<String> frameKeysExpanded = new ArrayList<>();
        
        for (final String key : frameObject.keySet()) {
            frameKeysExpanded.add(activeContext.uriExpansion().vocab(true).expand(key));
        }

        boolean frameDefault = frameKeysExpanded.contains(Keywords.GRAPH); 
                
        // 14.
        final FramingState state = new FramingState();
        
        state.setEmbed(options.getEmbed());     // 14.1.
        state.setEmbedded(false);               // 14.2.
        state.setExplicitInclusion(options.isExplicit());   // 14.3.
        state.setRequireAll(options.isRequiredAll());       // 14.4.
        state.setOmitDefault(options.isOmitDefault());      // 14.5.
        
        state.setGraphMap(NodeMapBuilder.with(expandedInput, new NodeMap()).build());   // 14.7.
        
        if (frameDefault) {
            state.setGraphName(Keywords.DEFAULT); // 14.6.

        } else {
            state.setGraphName(Keywords.MERGED);
            state.getGraphMap().merge();
        }
        
        // 15.
        Map<String, JsonValue> resultMap = new LinkedHashMap<>();
        
        // 16.
        Framing.with(state, 
                    new ArrayList<>(state.getGraphMap().subjects(state.getGraphName())), 
                    Frame.of(expandedFrame), 
                    resultMap, 
                    null
                    )
                .ordered(options.isOrdered())
                .frame();
        
        Collection<JsonValue> result = resultMap.values();        
        
        // 17. - remove blank @id
        if (!activeContext.inMode(Version.V1_0)) {
            result = removeBlankId(result);
        }
        
        // 18. - remove preserve
        result = result.stream().map(FramingProcessor::removePreserve).collect(Collectors.toList());
        
        // 19.
        JsonValue compactedResults = Compaction
                                                .with(activeContext)
                                                .compactArrays(options.isCompactArrays())
                                                .ordered(options.isOrdered())
                                                .compact(JsonUtils.toJsonArray(result));

        // 19.1.
        if (JsonUtils.isEmptyArray(compactedResults)) {
            compactedResults = JsonValue.EMPTY_JSON_OBJECT;
            
        // 19.2.
        } else if (JsonUtils.isArray(compactedResults)) {
        
            final String key = activeContext.uriCompaction().vocab(true).compact(Keywords.GRAPH);
            
            compactedResults = Json.createObjectBuilder()
                                    .add(key, compactedResults).build();
        
        }

        // 20.
        compactedResults = replaceNull(compactedResults);
        
        final boolean omitGraph;
        
        if (options.isOmitGraph() == null) {
            
            omitGraph = activeContext.inMode(Version.V1_1);
            
        } else {
            omitGraph = options.isOmitGraph();
        }
        
        // 21.
        if (!omitGraph && !compactedResults.asJsonObject().containsKey(Keywords.GRAPH)) {
                
            if (compactedResults.asJsonObject().isEmpty()) {
            
                compactedResults = Json.createObjectBuilder().add(Keywords.GRAPH, 
                        JsonValue.EMPTY_JSON_ARRAY
                        ).build();

            } else {
            
                compactedResults = Json.createObjectBuilder().add(Keywords.GRAPH, 
                                        Json.createArrayBuilder().add(compactedResults)
                                        ).build();
            }
        }

        // 19.3.
        if (JsonUtils.isNotEmptyArray(context) && JsonUtils.isNotEmptyObject(context)) {
            compactedResults = Json.createObjectBuilder(compactedResults.asJsonObject()).add(Keywords.CONTEXT, context).build();
        }
                
        return compactedResults.asJsonObject();
    }

    public static final JsonObject frame(final URI input, final URI frame, final JsonLdOptions options) throws JsonLdError {
        return frame(getDocument(input, options), getDocument(frame, options), options);
    }
    
    private static RemoteDocument getDocument(final URI document, final JsonLdOptions options) throws JsonLdError {
        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        final RemoteDocument remoteDocument = 
                                options
                                    .getDocumentLoader()
                                    .loadDocument(document,
                                            new LoadDocumentOptions()
                                                    .setExtractAllScripts(options.isExtractAllScripts()));

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Cannot load document [" + document + "].");
        }
        
        return remoteDocument;
    }
        
    private static final JsonValue removePreserve(JsonValue value) {
        
        if (JsonUtils.isScalar(value)) {
            return value;
        }
        
        if (JsonUtils.isArray(value)) {
            
            final JsonArrayBuilder array = Json.createArrayBuilder();
            
            for (final JsonValue item : value.asJsonArray()) {
                array.add(removePreserve(item));
            }
            
            return array.build();
        }
        
        final JsonObjectBuilder object = Json.createObjectBuilder();
        
        for (final Entry<String, JsonValue> entry : value.asJsonObject().entrySet()) {
            
            if (Keywords.PRESERVE.equals(entry.getKey())) {
                
                return entry.getValue().asJsonArray().get(0);
            }
            object.add(entry.getKey(), removePreserve(entry.getValue()));
        }
        
        return object.build();
    }
    
    private static final JsonValue replaceNull(JsonValue value) {
        
        if (JsonUtils.isString(value) && Keywords.NULL.equals(((JsonString)value).getString())) {
            return JsonValue.NULL;
            
        } else if (JsonUtils.isScalar(value)) {
            return value;
            
        } else if (JsonUtils.isArray(value)) {
            
            JsonArrayBuilder array = Json.createArrayBuilder();
            
            value.asJsonArray().stream().map(FramingProcessor::replaceNull).forEach(array::add);
            
            JsonArray result = array.build();
            
            return result.size() != 1 || JsonUtils.isNotNull(result.get(0)) ? result : JsonValue.EMPTY_JSON_ARRAY;
        }
        
        JsonObjectBuilder object = Json.createObjectBuilder();
        
        for (Entry<String, JsonValue> entry : value.asJsonObject().entrySet()) {
            
            object.add(entry.getKey(), replaceNull(entry.getValue()));
            
        }
        return object.build();
    }

    private static final Collection<JsonValue> removeBlankId(Collection<JsonValue> array) {
        
        Map<String, Integer> candiates = new HashMap<>();
        
        array.stream().forEach(v -> findBlankNodes(v, candiates));
        
        List<String> remove = candiates.entrySet().stream().filter(e -> e.getValue() == 1).map(Entry::getKey).collect(Collectors.toList());
        
        if (remove.isEmpty()) {
            return array;
        }
        
        return array.stream().map(v -> removeBlankIdKey(v, remove)).collect(Collectors.toList());
    }

    private static final JsonValue removeBlankIdKey(JsonValue value, List<String> blankNodes) {
        
        if (JsonUtils.isScalar(value)) {
            return value;
        }
        if (JsonUtils.isArray(value)) {
            
            JsonArrayBuilder array = Json.createArrayBuilder();
            
            for (JsonValue item : value.asJsonArray()) {
                array.add(removeBlankIdKey(item, blankNodes));
            }
            
            return array.build();
        }
        
        JsonObjectBuilder object = Json.createObjectBuilder();
        
        for (Entry<String, JsonValue> entry : value.asJsonObject().entrySet()) {
            
            if (Keywords.ID.equals(entry.getKey()) 
                    && JsonUtils.isString(entry.getValue())
                    && blankNodes.contains(((JsonString)entry.getValue()).getString())) {
                
                    continue;
            }
            
            object.add(entry.getKey(), removeBlankIdKey(entry.getValue(), blankNodes));
        }
        
        return object.build();
    }

    private static final void findBlankNodes(JsonValue value, final Map<String, Integer> blankNodes) {

        if (JsonUtils.isString(value)) {
            
            if (BlankNode.isWellFormed(((JsonString)value).getString())) {
                Integer count = blankNodes.computeIfAbsent(((JsonString)value).getString(), x -> Integer.valueOf(0));
                blankNodes.put(((JsonString)value).getString(), ++count);
            }            
            
            return;
        }
        
        if (JsonUtils.isScalar(value)) {
            return;
        }
        
        if (JsonUtils.isArray(value)) {
            for (JsonValue item : value.asJsonArray()) {
                findBlankNodes(item, blankNodes);        
            }
            return;
        }
        
        for (Entry<String, JsonValue> entry : value.asJsonObject().entrySet()) {
            
            findBlankNodes(entry.getValue(), blankNodes);
        }
    }
}
