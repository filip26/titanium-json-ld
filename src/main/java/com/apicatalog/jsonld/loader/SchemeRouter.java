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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.jsonld.Document;

/**
 * A {@link DocumentLoader} that delegates document loading to other loaders
 * based on the URI scheme.
 *
 * <p>
 * The {@code SchemeRouter} acts as a dispatcher: it maps URI schemes (such as
 * {@code http}, {@code https}, or {@code file}) to specific
 * {@link DocumentLoader} instances. This allows different loading strategies to
 * coexist within a single environment.
 * </p>
 *
 * <p>
 * If no loader is registered for a given scheme, an optional fallback loader
 * can be used. If neither a route nor a fallback is available, the router
 * throws a {@link LoaderException}.
 * </p>
 *
 * <p>
 * Typical usage:
 * </p>
 *
 * <pre>{@code
 * 
 * static final var httpLoader = HttpLoader.newLoader(parser);
 * 
 * DocumentLoader loader = SchemeRouter.newBuilder()
 *         .route("https", httpLoader)
 *         .route("file", new FileLoader(parser))
 *         .fallback(StaticLoader.newBuilder().set(url, document).build())
 *         .build();
 * }</pre>
 *
 * @see DocumentLoader
 * @see HttpLoader
 * @see FileLoader
 */
public final class SchemeRouter implements DocumentLoader {

    private final Map<String, DocumentLoader> loaders;
    private final DocumentLoader fallback;

    private SchemeRouter(final Map<String, DocumentLoader> loaders, DocumentLoader fallback) {
        this.loaders = loaders;
        this.fallback = fallback;
    }

    /** Creates a new builder for configuring a {@code SchemeRouter}. */
    public static Builder newBuilder() {
        return new Builder();
    }

    /**
     * Delegates the request to the loader registered for the given URI scheme.
     *
     * @param uri     the target URI to load
     * @param options document loading options
     * @return the loaded {@link Document}
     * @throws LoaderException      if no loader is registered for the URI scheme
     *                              and no fallback is configured
     * @throws NullPointerException if {@code url} is {@code null}
     */
    @Override
    public Document loadDocument(URI uri, Options options) throws LoaderException {

        final DocumentLoader loader = loaders.getOrDefault(
                Objects.requireNonNull(uri, "The url must not be null.")
                        .getScheme().toLowerCase(),
                null);

        if (loader != null) {
            return loader.loadDocument(uri, options);
        }

        if (fallback != null) {
            return fallback.loadDocument(uri, options);
        }

        throw new LoaderException(uri, "The scheme [" + uri.getScheme() + "] is not supported.");

    }

    // ---------- Builder ----------

    /**
     * Fluent builder for configuring and creating a {@link SchemeRouter}.
     *
     * <p>
     * Routes can be defined by associating URI schemes with specific
     * {@link DocumentLoader} instances. A fallback loader may also be set to handle
     * all unregistered schemes.
     * </p>
     */
    public static final class Builder {

        private final Map<String, DocumentLoader> routes = new LinkedHashMap<>();
        private DocumentLoader fallback;

        /**
         * Associates a URI scheme with a {@link DocumentLoader}.
         *
         * @param scheme the URI scheme (case-insensitive)
         * @param loader the loader to handle that scheme
         * @return this builder instance
         */
        public Builder route(String scheme, DocumentLoader loader) {
            routes.put(scheme.toLowerCase(), loader);
            return this;
        }

        /**
         * Sets the fallback loader used when no scheme matches.
         *
         * @param loader the fallback loader
         * @return this builder instance
         */
        public Builder fallback(DocumentLoader loader) {
            this.fallback = loader;
            return this;
        }

        /**
         * Builds a new {@link SchemeRouter} instance with the configured routes and
         * optional fallback loader.
         *
         * @return a new {@link SchemeRouter}
         */
        public DocumentLoader build() {
            return new SchemeRouter(Map.copyOf(routes), fallback);
        }
    }
}
