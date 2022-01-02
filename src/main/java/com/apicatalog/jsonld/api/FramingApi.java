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

import com.apicatalog.jsonld.JsonLdEmbed;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.processor.FramingProcessor;
import com.apicatalog.jsonld.uri.UriUtils;

import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;

public final class FramingApi implements CommonApi<FramingApi>, LoaderApi<FramingApi>, ContextApi<FramingApi> {

    // required
    private final Document document;
    private final URI documentUri;
    private final Document frame;
    private final URI frameUri;

    // optional
    private JsonLdOptions options;

    public FramingApi(URI documentUri, URI frameUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.frame = null;
        this.frameUri = frameUri;
        this.options = new JsonLdOptions();
    }

    public FramingApi(Document document, Document frame) {
        this.document = document;
        this.documentUri = null;
        this.frame = frame;
        this.frameUri = null;
        this.options = new JsonLdOptions();
    }

    @Override
    public FramingApi options(JsonLdOptions options) {

        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }

    @Override
    public FramingApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

    @Override
    public FramingApi context(String contextLocation) {

        URI contextUri = null;

        if (contextLocation != null) {

            contextUri = UriUtils.create(contextLocation);

            if (contextUri == null) {
                throw new IllegalArgumentException("Context location must be valid URI or null but is [" + contextLocation + ".");
            }
        }

        return context(contextUri);
    }

    @Override
    public FramingApi context(JsonStructure context) {
        options.setExpandContext(context != null ?  JsonDocument.of(context) : null);
        return this;
    }

    @Override
    public FramingApi context(Document context) {
        options.setExpandContext(context);
        return this;
    }

    @Override
    public FramingApi mode(JsonLdVersion processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    @Override
    public FramingApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    @Override
    public FramingApi base(String baseUri) {
        return base(UriUtils.create(baseUri));
    }

    @Override
    public FramingApi loader(DocumentLoader loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    @Override
    public FramingApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }

    public FramingApi embed(JsonLdEmbed value) {
        options.setEmbed(value);
        return this;
    }

    public FramingApi explicit(boolean enable) {
        options.setExplicit(enable);
        return this;
    }

    public FramingApi explicit() {
        return explicit(true);
    }

    public FramingApi omitDefault(boolean enable) {
        options.setOmitDefault(enable);
        return this;
    }

    public FramingApi omitDefault() {
        return omitDefault(true);
    }

    public FramingApi omitGraph(boolean enable) {
        options.setOmitGraph(enable);
        return this;
    }

    public FramingApi omitGraph() {
        return omitGraph(true);
    }

    public FramingApi requiredAll(boolean enable) {
        options.setRequiredAll(enable);
        return this;
    }

    public FramingApi requiredAll() {
        return requiredAll(true);
    }

    /**
     * Get the result of framing.
     *
     * @return {@link JsonObject} representing framed document
     * @throws JsonLdError
     */
    public JsonObject get() throws JsonLdError {
        if (documentUri != null && frameUri != null) {
            return FramingProcessor.frame(documentUri, frameUri, options);
        }
        if (document != null && frame != null) {
            return FramingProcessor.frame(document, frame, options);
        }

        throw new IllegalStateException();
    }
}