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
package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.Collection;
import java.util.List;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;

/**
 * Loads JSON-LD documents identified by a URI.
 *
 * <p>
 * Used by JSON-LD processors to obtain input documents, contexts, or frames
 * from various sources such as HTTP endpoints, local files, classpath
 * resources, or preloaded mappings. Implementations may handle specific URI
 * schemes or apply custom resolution strategies.
 * </p>
 *
 * <p>
 * Common implementations include:
 * <ul>
 * <li>{@link HttpLoader} — retrieves documents over HTTP or HTTPS</li>
 * <li>{@link FileLoader} — reads documents from the local file system</li>
 * <li>{@link ClasspathLoader} — loads documents from the application
 * classpath</li>
 * <li>{@link StaticLoader} — resolves documents from in-memory mappings</li>
 * <li>{@link SchemeRouter} — delegates to loaders based on URI scheme</li>
 * <li>{@link CacheLoader} — caches results of document retrieval</li>
 * </ul>
 * </p>
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#loaddocumentcallback">
 *      JSON-LD 1.1 API — LoadDocumentCallback</a>
 */
public interface DocumentLoader {

    /**
     * Loads the JSON-LD document identified by the given URI.
     *
     * <p>
     * This method resolves and retrieves a document that can be used as input,
     * context, or frame in JSON-LD processing. The behavior may depend on the
     * implementation and options provided — for example, whether to request
     * specific profiles or extract script elements.
     * </p>
     *
     * @param url     the absolute URI of the document to load (must not be
     *                {@code null})
     * @param options loader configuration options controlling retrieval behavior
     * @return the loaded {@link Document} representation
     * @throws JsonLdException if the document cannot be retrieved, parsed, or
     *                         resolved
     */
    Document loadDocument(URI url, Options options) throws JsonLdException;

    /**
     * Returns the default {@link Options} instance used by loaders that do not
     * require custom configuration.
     *
     * @return immutable default loader options
     */
    public static Options defaultOptions() {
        return Options.DEFAULT;
    }

    /**
     * Loader configuration options controlling how documents are retrieved and
     * interpreted.
     *
     * <p>
     * Options may specify behavior such as whether to extract JSON-LD from embedded
     * scripts, request particular profiles, or influence how linked contexts are
     * processed.
     * </p>
     *
     * @see <a href="https://www.w3.org/TR/json-ld11-api/#loaddocumentoptions">
     *      JSON-LD 1.1 API — LoadDocumentOptions</a>
     */
    public record Options(
            boolean extractAllScripts,
            String profile,
            Collection<String> requestProfile) {

        static final Options DEFAULT = new Options(false, null, null);

        public Options {
            extractAllScripts = false;
            profile = null;
            requestProfile = requestProfile == null ? List.of() : List.copyOf(requestProfile);
        }
    }
}
