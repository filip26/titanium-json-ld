package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.JsonArray;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.flattening.BlankNodeIdGenerator;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.rdf.Rdf;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-api/#dom-jsonldprocessor-tordf">JsonLdProcessor.toRdf()</a>
 *
 */
public final class ToRdfProcessor {

    ToRdfProcessor() {
    }

    public static final Dataset toRdf(final URI input, final JsonLdOptions options) throws JsonLdError {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        final RemoteDocument remoteDocument = 
                                options
                                    .getDocumentLoader()
                                    .loadDocument(input,
                                            new LoadDocumentOptions()
                                                    .setExtractAllScripts(options.isExtractAllScripts()));

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
        
        return toRdf(remoteDocument, options);
    }

    public static final Dataset toRdf(RemoteDocument input, final JsonLdOptions options) throws JsonLdError {

        // 2.
        JsonLdOptions expansionOptions = new JsonLdOptions();
        expansionOptions.setBase(options.getBase());
//FIXME        expansionOptions.setExpandContext(options.getExpandContext());
        
        JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions);
       
        // 3.
        Dataset dataset = Rdf.createDataset();
        
        // 4.
        NodeMap nodeMap = new NodeMap();
        BlankNodeIdGenerator idGenerator = new BlankNodeIdGenerator();
        
        // 5.
        NodeMapBuilder.with(expandedInput, nodeMap, idGenerator).build();
        
        // 6.
        
        // 7.
        return dataset;
    }
}
