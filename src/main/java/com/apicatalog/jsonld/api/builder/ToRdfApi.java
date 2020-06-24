package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.ToRdfProcessor;
import com.apicatalog.rdf.RdfDataset;

public final class ToRdfApi implements CommonApi<ToRdfApi>, LoaderApi<ToRdfApi>{

    // required
    private final RemoteDocument document;
    private final URI documentUri;
    
    // optional
    private JsonLdOptions options;
    
    public ToRdfApi(URI documentUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.options = new JsonLdOptions();
    }

    public ToRdfApi(RemoteDocument document) {
        this.document = document;
        this.documentUri = null;
        this.options = new JsonLdOptions();
    }

    @Override
    public ToRdfApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }

    public ToRdfApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

    public ToRdfApi context(String contextUri) {
        return context(URI.create(contextUri));
    }

    public ToRdfApi produceGeneralizedRdf(RdfDirection rdfDirection) {
        options.setRdfDirection(rdfDirection);
        return this;
    }

    public ToRdfApi produceGeneralizedRdf() {
        return produceGeneralizedRdf(true);
    }

    public ToRdfApi produceGeneralizedRdf(boolean enable) {
        options.setProduceGeneralizedRdf(enable);
        return this;
    }

    @Override
    public ToRdfApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    @Override
    public ToRdfApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    @Override
    public ToRdfApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    @Override
    public ToRdfApi loader(LoadDocumentCallback loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    @Override
    public ToRdfApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }
    
    @Override
    public ToRdfApi ordered() {
        return ordered(true);
    }
    
    public RdfDataset get() throws JsonLdError {
        if (documentUri != null) {
            return ToRdfProcessor.toRdf(documentUri, options);
        }
        
        if (document != null) {
            return ToRdfProcessor.toRdf(document, options);
        }
        
        throw new IllegalArgumentException();
    }
}
