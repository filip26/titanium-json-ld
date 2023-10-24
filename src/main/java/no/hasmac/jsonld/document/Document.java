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
package no.hasmac.jsonld.document;

import java.net.URI;
import java.util.Optional;

import no.hasmac.jsonld.http.media.MediaType;
import no.hasmac.rdf.RdfDataset;

import jakarta.json.JsonStructure;

/**
 * A document that can be processed by the processor.
 *
 * This can either be {@link JsonStructure}, representing JSON-LD or JSON document,
 * or {@link RdfDataset}
 *
 * Implemented by {@link JsonDocument}, {@link RdfDocument}, and provided by {@link DocumentParser}.
 *
 */
public interface Document {

    /**
     * The <a href="https://tools.ietf.org/html/rfc2045#section-5">Content-Type</a>
     * of the loaded document, exclusive of any optional parameters.
     *
     * @return <code>Content-Type</code> of the loaded document, never <code>null</code>
     */
    MediaType getContentType();

    /**
     * The value of the HTTP Link header when profile attribute matches <code>http://www.w3.org/ns/json-ld#context</code>.
     *
     * @return attached {@link URI} referencing document context or <code>null</code> if not available
     */
    URI getContextUrl();

    void setContextUrl(URI contextUrl);

    /**
     * The final {@link URI} of the loaded document.
     *
     * @return {@link URI} of the loaded document or <code>null</code> if not available
     */
    URI getDocumentUrl();

    void setDocumentUrl(URI documentUrl);

    /**
     * The value of any <code>profile</code> parameter retrieved as part of the
     * original {@link #getContentType()}.
     *
     * @return document profile or {@link Optional#empty()}
     */
    Optional<String> getProfile();

    /**
     * Get the document content as parsed {@link JsonStructure}.
     *
     * @return {@link JsonStructure} or {@link Optional#empty()} if document content is not JSON based
     */
    public default  Optional<JsonStructure> getJsonContent() {
        return Optional.empty();
    }

    /**
     * Get the document content as parsed {@link RdfDataset}.
     *
     * @return {@link RdfDataset} or {@link Optional#empty()} if document content is not in <code>application/n-quads</code> representation
     */
    public default Optional<RdfDataset> getRdfContent() {
        return Optional.empty();
    }
}
