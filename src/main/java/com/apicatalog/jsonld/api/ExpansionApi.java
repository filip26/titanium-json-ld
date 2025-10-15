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

import java.io.IOException;
import java.net.URI;
import java.util.Collection;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.ProcessingPolicy;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.JsonLdLoader;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.uri.UriUtils;

import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;

public final class ExpansionApi implements CommonApi<ExpansionApi>, LoaderApi<ExpansionApi>, ContextApi<ExpansionApi> {

    // required
    private final URI documentUri;
    private final Document document;

    // optional
    private JsonLdOptions options;

    public ExpansionApi(URI documentUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.options = new JsonLdOptions();
    }

    public ExpansionApi(Document document) {
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
    public ExpansionApi context(JsonStructure context) {
        options.setExpandContext(context != null ? JsonDocument.of(context) : null);
        return this;
    }

    @Override
    public ExpansionApi context(Document context) {
        options.setExpandContext(context);
        return this;
    }

    @Override
    public ExpansionApi mode(JsonLdVersion processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    @Override
    public ExpansionApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    @Override
    public ExpansionApi loader(JsonLdLoader loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    @Override
    public ExpansionApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }

    /**
     * Get the result of the document expansion.
     *
     * @return {@link JsonArray} representing expanded document
     * @throws JsonLdError if the document expansion fails
     * @throws IOException 
     */
    public Collection<?> get() throws JsonLdError, IOException {
        if (document != null) {
            return Expander.expand(document, options, false);

        } else if (documentUri != null) {
            return Expander.expand(documentUri, options);
        }
        throw new IllegalStateException();
    }

    /**
     * Experimental: Accept numeric @id. Disabled by default.
     *
     * @return builder instance
     */
    public ExpansionApi numericId() {
        options.setNumericId(true);
        return this;
    }

    /**
     * Experimental: Enables JSON-LD-STAR extension. Disabled by default.
     *
     * @see <a href="https://json-ld.github.io/json-ld-star">JSON-LD-STAR Draft</a>
     *
     * @return builder instance
     */
    public ExpansionApi rdfStar() {
        options.setRdfStar(true);
        return this;
    }

    /**
     * Set a processing policy determining how to proceed when an undefined term is
     * found during an expansion. An unknown term is ignored by default.
     * 
     * @param policy a processing policy
     * @return builder instance
     */
    public ExpansionApi undefinedTermsPolicy(ProcessingPolicy policy) {
        options.setUndefinedTermsPolicy(policy);
        return this;
    }
}
