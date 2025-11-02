package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.tree.io.PolyNode;

public final class StaticLoader implements DocumentLoader {

    private final Map<URI, Document> resources;
    private final DocumentLoader loader;

    private StaticLoader(Map<URI, Document> resources, DocumentLoader loader) {
        this.resources = resources;
        this.loader = loader;
    }

    public static final Builder newBuilder() {
        return newBuilder(Map.of());
    }

    public static final Builder newBuilder(StaticLoader loader) {
        return new Builder(loader.resources, loader.loader);
    }

    public static final Builder newBuilder(Map<URI, Document> resources) {
        return new Builder(Objects.requireNonNull(resources), null);
    }

    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {

        var document = resources.getOrDefault(
                Objects.requireNonNull(url, "The url must not be null."),
                null);

        if (document != null) {
            return document;
        }

        if (loader != null) {
            return loader.loadDocument(url, options);
        }

        throw new JsonLdException(
                JsonLdErrorCode.LOADING_DOCUMENT_FAILED,
                "URL [" + url + "] is not recogized and fallback loader is not set.");
    }

    public static final class Builder {

        private final Map<URI, Document> resources;
        private DocumentLoader loader;

        Builder(Map<URI, Document> resources, DocumentLoader loader) {
            this.resources = new LinkedHashMap<>(resources);
            this.loader = loader;
        }

        public Builder set(String url, Document document) {
            return set(URI.create(url), document);
        }

        public Builder set(URI url, Document document) {
            resources.put(url, document);
            return this;
        }

        public Builder set(String url, PolyNode node) {
            return set(URI.create(url), node);
        }

        public Builder set(URI url, PolyNode node) {
            resources.put(url, RemoteDocument.of(node, url));
            return this;
        }

        public Builder fallback(DocumentLoader loader) {
            this.loader = loader;
            return this;
        }

        public StaticLoader build() {
            return new StaticLoader(Map.copyOf(resources), loader);
        }
    }
}
