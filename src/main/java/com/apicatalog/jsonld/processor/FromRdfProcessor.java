package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.JsonArray;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.serialization.RdfToJsonld;

public final class FromRdfProcessor {

    private FromRdfProcessor() {
    }
    
    public static final JsonArray fromRdf(final Document document, final JsonLdOptions options) throws JsonLdError {

        return RdfToJsonld
                    .with(document.getRdfContent().orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Expected RDF document but got [" + document.getContentType() + "]")))
                    .ordered(options.isOrdered())
                    .rdfDirection(options.getRdfDirection())
                    .useNativeTypes(options.isUseNativeTypes())
                    .useRdfType(options.isUseRdfType())
                    .processingMode(options.getProcessingMode())
                    .build();
    }

    public static JsonArray fromRdf(URI documentUri, JsonLdOptions options) throws JsonLdError {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        final Document remoteDocument = 
                                options
                                    .getDocumentLoader()
                                    .loadDocument(documentUri,
                                            new DocumentLoaderOptions()
                                                    );

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
        
        return fromRdf(remoteDocument, options);
    }
}
