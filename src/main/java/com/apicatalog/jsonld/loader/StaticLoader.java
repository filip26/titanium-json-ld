package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;

public final class StaticLoader implements DocumentLoader {

    private final Map<String, Document> resources;
    private final DocumentLoader loader;

    private StaticLoader(Map<String, Document> resources, DocumentLoader loader) {
        this.resources = resources;
        this.loader = loader;
    }

    public static final Builder newBuilder() {
        return newBuilder(Map.of());
    }

    public static final Builder newBuilder(StaticLoader loader) {
        return new Builder(loader.resources, loader.loader);
    }

    public static final Builder newBuilder(Map<String, Document> resources) {
        return new Builder(resources, null);
    }

    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {
        if (url == null) {
            throw new IllegalArgumentException("The url must not be null.");
        }

        var document = resources.getOrDefault(url, null);

        if (document != null) {
            return document;
        }

        if (loader != null) {
            return loader.loadDocument(url, options);
        }

        throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "URL [" + url + "] is not present and fallback loader is not set.");
    }

    public static final class Builder {

        private final Map<String, Document> resources;
        private DocumentLoader loader;

        Builder(Map<String, Document> resources, DocumentLoader loader) {
            this.resources = new LinkedHashMap<>(resources);
            this.loader = loader;
        }

        public Builder set(String url, Document document) {
            resources.put(url, document);
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
