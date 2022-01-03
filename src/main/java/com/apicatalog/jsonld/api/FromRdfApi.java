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
package com.apicatalog.jsonld.api;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.processor.FromRdfProcessor;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.RdfDataset;

import jakarta.json.JsonArray;

public final class FromRdfApi implements CommonApi<FromRdfApi>, LoaderApi<FromRdfApi> {

    // required
    private final Document document;
    private final URI documentUri;

    // optional
    private JsonLdOptions options;

    public FromRdfApi(Document document) {
        this.document = document;
        this.documentUri = null;
        this.options = new JsonLdOptions();
    }

    public FromRdfApi(URI documentUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.options = new JsonLdOptions();
    }

    @Override
    public FromRdfApi options(JsonLdOptions options) {

        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }

    @Override
    public FromRdfApi mode(JsonLdVersion processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    @Override
    public FromRdfApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    @Override
    public FromRdfApi base(String baseUri) {
        return base(UriUtils.create(baseUri));
    }

    @Override
    public FromRdfApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }

    @Override
    public FromRdfApi loader(DocumentLoader loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    public FromRdfApi nativeTypes() {
        return nativeTypes(true);
    }

    public FromRdfApi nativeTypes(boolean useNativeTypes) {
        options.setUseNativeTypes(useNativeTypes);
        return this;
    }

    /**
     * Get <code>JSON-LD</code> representation of the provided {@link RdfDataset}.
     *
     * @return {@link JsonArray} representing <code>JSON-LD</code> document
     * @throws JsonLdError
     */
    public JsonArray get() throws JsonLdError {

        if (document != null) {
            return FromRdfProcessor.fromRdf(document, options);
        }

        if (documentUri != null) {
            return FromRdfProcessor.fromRdf(documentUri, options);
        }

        throw new IllegalStateException();
    }
}
