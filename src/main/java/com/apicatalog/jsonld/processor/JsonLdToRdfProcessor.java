package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.JsonArray;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.deseralization.JsonLdToRdfBuilder;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-api/#dom-jsonldprocessor-tordf">JsonLdProcessor.toRdf()</a>
 *
 */
public final class JsonLdToRdfProcessor {

    JsonLdToRdfProcessor() {
    }

    public static final RdfDataset toRdf(final URI input, final JsonLdOptions options) throws JsonLdError {

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

    public static final RdfDataset toRdf(RemoteDocument input, final JsonLdOptions options) throws JsonLdError {

        final JsonLdOptions expansionOptions = new JsonLdOptions();
        expansionOptions.setBase(options.getBase());
//FIXME        expansionOptions.setExpandContext(options.getExpandContext());
        
        final JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions);

        return JsonLdToRdfBuilder
                        .with(
                            NodeMapBuilder.with(expandedInput, new NodeMap()).build(),
                            Rdf.createDataset()
                            )
                        .produceGeneralizedRdf(options.isProduceGeneralizedRdf())
                        .rdfDirection(options.getRdfDirection())
                        .build();     
    }
}
