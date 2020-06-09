package com.apicatalog.jsonld.processor;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.compaction.CompactionBuilder;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.ActiveContextBuilder;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.framing.FramingBuilder;
import com.apicatalog.jsonld.framing.FramingState;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
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
        
        JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions);
        
        // 7.
        expansionOptions.setFrameExpansion(false);
        JsonArray expandedFrame = ExpansionProcessor.expand(frame, expansionOptions);
        
        // 8.
        final JsonValue context;
        
        if (JsonUtils.isObject(frame.getDocument().asJsonStructure())
                && frame.getDocument().asJsonStructure().asJsonObject().containsKey(Keywords.CONTEXT)    
                ) {
            context = frame.getDocument().asJsonStructure().asJsonObject().get(Keywords.CONTEXT);
            
        } else {
            context = JsonValue.NULL;
        }
            
        // 9.
        final URI contextBase = (frame.getContextUrl() != null)
                                ? frame.getDocumentUrl()
                                : options.getBase();

        // 10.
        ActiveContext activeContext = ActiveContextBuilder
                                        .with(new ActiveContext(options), context, contextBase, options).build();
        // 11.
        //TODO
        
        // 12.
        activeContext.createInverseContext();

        // 13.
        
        boolean frameDefault = options.isFrameDefault();
        //TODO expands to GRAPH        
        if (!frameDefault && frame.getDocument().asJsonStructure().asJsonObject().containsKey(Keywords.GRAPH)) {
            frameDefault = true;
        }
        
        // 14.
        final FramingState state = new FramingState();
        
        state.setEmbed(options.getEmbed());     // 14.1.
        state.setEmbedded(false);               // 14.2.
        state.setExplicitInclusion(options.isExplicit());   // 14.3.
        state.setRequireAll(options.isRequiredAll());       // 14.4.
        state.setOmitDefault(options.isOmitDefault());      // 14.5.
        if (frameDefault) {
            state.setGraphName(Keywords.DEFAULT); // 14.6.
        }
        state.setGraphMap(NodeMapBuilder.with(expandedInput, new NodeMap()).build());   // 14.7.
        
        //TODO
        
        // 15.
        List<JsonValue> results = new ArrayList<>();
        
        // 16.
        FramingBuilder.with(state, 
                            new ArrayList<>(state.getGraphMap().subjects(state.getGraphName())), 
                            expandedFrame, 
                            results, 
                            null
                            ).build();
        
        // 17.
        //TODO      
        
        // 18.
        //TODO
        
        // 19.
        JsonValue compactedResults = CompactionBuilder
                                        .with(activeContext, null, JsonUtils.toJsonArray(results))
                                        .compactArrays(options.isCompactArrays())
                                        .ordered(options.isOrdered())
                                        .build();
        // 19.1.
        if (JsonUtils.isEmptyArray(compactedResults)) {
            compactedResults = JsonValue.EMPTY_JSON_OBJECT;
            
        // 19.2.
        } else if (JsonUtils.isArray(compactedResults)) {
            //TODO
        }
        
        
        
        
        
        // 20.
        //TODO
        
        // 21.
        //TODO
        
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
    
}
