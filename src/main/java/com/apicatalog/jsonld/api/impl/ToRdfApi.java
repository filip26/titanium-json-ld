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
package com.apicatalog.jsonld.api.impl;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.processor.ToRdfProcessor;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.RdfDataset;

public final class ToRdfApi implements CommonApi<ToRdfApi>, LoaderApi<ToRdfApi>, ContextApi<ToRdfApi>{

    // required
    private final Document document;
    private final URI documentUri;
    
    // optional
    private JsonLdOptions options;
    
    public ToRdfApi(URI documentUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.options = new JsonLdOptions();
    }

    public ToRdfApi(Document document) {
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

    @Override
    public ToRdfApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

    @Override
    public ToRdfApi context(String contextLocation) {
        
        if (contextLocation != null) {
            if (!UriUtils.isNotURI(contextLocation)) {
                throw new IllegalArgumentException("Context location must be valid URI or null but is [" + contextLocation + ".");
            }
            options.setExpandContext(UriUtils.create(contextLocation));
            
        } else {
            options.setExpandContext((Document)null);
        }
        
        return this;
    }
    
    @Override
    public ToRdfApi context(JsonStructure context) {
        options.setExpandContext(context != null ? JsonDocument.of(context) : null);
        return this;
    }

    @Override
    public ToRdfApi context(Document context) {
        options.setExpandContext(context);
        return this;
    }

    /**
     * If set to true, the JSON-LD processor may emit blank nodes for triple predicates, otherwise they will be omitted.
     * @param enable
     * @return builder instance
     */
    public ToRdfApi produceGeneralizedRdf(boolean enable) {
        options.setProduceGeneralizedRdf(enable);
        return this;
    }

    /**
     * The JSON-LD processor may emit blank nodes for triple predicates.
     * 
     * @return builder instance
     */    
    public ToRdfApi produceGeneralizedRdf() {
        return produceGeneralizedRdf(true);
    }

    /**
     * Determines how value objects containing a base direction are transformed to and from RDF.
     * 
     * @param direction
     * @return builder instance
     */
    public ToRdfApi rdfDirection(RdfDirection direction) {
        options.setRdfDirection(direction);
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
    public ToRdfApi base(String baseLocation) {
        
        if (baseLocation != null && !UriUtils.isNotURI(baseLocation)) {
            throw new IllegalArgumentException("Base location must be valid URI or null but is [" + baseLocation + ".");
        }
        
        return base(UriUtils.create(baseLocation));
    }

    @Override
    public ToRdfApi loader(DocumentLoader loader) {
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
    
    /**
     * Transform provided <code>JSON-LD</code> document into {@link RdfDataset}.
     * 
     * @return {@link RdfDataset} representing provided <code>JSON-LD</code> document
     * @throws JsonLdError
     */
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
