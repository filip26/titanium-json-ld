package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import javax.json.JsonArray;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.ExpansionProcessor;
import com.apicatalog.jsonld.uri.UriUtils;

public final class ExpansionApi implements CommonApi<ExpansionApi>, LoaderApi<ExpansionApi>, ContextApi<ExpansionApi> {

    // required
    private final URI documentUri;
    private final RemoteDocument document;
    
    // optional
    private JsonLdOptions options;
    
    public ExpansionApi(URI documentUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.options = new JsonLdOptions();
    }

    public ExpansionApi(RemoteDocument document) {
        this.document = document;
        this.documentUri = null;
        this.options = new JsonLdOptions();
    }

    @Override
    public ExpansionApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }
    
    @Override
    public ExpansionApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

    @Override
    public ExpansionApi context(String contextLocation) {
        return context(contextLocation != null ? UriUtils.create(contextLocation) : null);
    }

    @Override
    public ExpansionApi context(JsonStructure context) {
        options.setExpandContext(context != null ? RemoteDocument.of(context) : null);
        return this;
    }

    @Override
    public ExpansionApi context(RemoteDocument context) {
        options.setExpandContext(context);
        return this;
    }

    @Override
    public ExpansionApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    @Override
    public ExpansionApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    @Override
    public ExpansionApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    @Override
    public ExpansionApi loader(LoadDocumentCallback loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    @Override
    public ExpansionApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }
    
    @Override
    public ExpansionApi ordered() {
        return ordered(true);
    }

    /**
     * Get the result of the document expansion.
     * 
     * @return {@link JsonArray} representing expanded document
     * @throws JsonLdError
     */
    public JsonArray get() throws JsonLdError {
        if (document != null) {
            return ExpansionProcessor.expand(document, options, false);
            
        } else if (documentUri != null) {
            return ExpansionProcessor.expand(documentUri, options);
        }
        throw new IllegalStateException();
    }
}
