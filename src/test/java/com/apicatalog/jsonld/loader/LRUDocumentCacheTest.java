package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import jakarta.json.JsonValue;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

public class LRUDocumentCacheTest {

    static class Request {
        final URI url;

        final DocumentLoaderOptions options;

        public Request(URI url, DocumentLoaderOptions options) {
            this.url = url;
            this.options = options;
        }

    }

    static class DummyLoader implements DocumentLoader {

        public List<Request> requests = new ArrayList<>();

        @Override
        public Document loadDocument(URI url, DocumentLoaderOptions options) {
            requests.add(new Request(url, options));
            // Return empty document.
            return JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        }

    }

    @Test
    void testLoadDocument() throws JsonLdError {
        DummyLoader dummyLoader = new DummyLoader();
        LRUDocumentCache cachedLoader = new LRUDocumentCache(dummyLoader, 2);

        DocumentLoaderOptions options = new DocumentLoaderOptions();
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        // There should be only one call as all other should be cached.
        Assertions.assertEquals(1, dummyLoader.requests.size());
        // Make sure valid arguments were passed.
        Request request = dummyLoader.requests.get(0);
        Assertions.assertEquals("http://localhost/1",request.url.toString());
        Assertions.assertSame(options,request.options);
    }

    @Test
    void testCacheSize() throws JsonLdError {
        DummyLoader dummyLoader = new DummyLoader();
        LRUDocumentCache cachedLoader = new LRUDocumentCache(dummyLoader, 2);

        DocumentLoaderOptions options = new DocumentLoaderOptions();
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        cachedLoader.loadDocument(URI.create("http://localhost/2"), options);
        cachedLoader.loadDocument(URI.create("http://localhost/2"), options);

        // There should be only one call as all other should be cached.
        Assertions.assertEquals(2, dummyLoader.requests.size());

        // Request of new resource.
        cachedLoader.loadDocument(URI.create("http://localhost/3"), options);
        Assertions.assertEquals(3, dummyLoader.requests.size());

        // Using LRU the first resources should not be in cache anymore.
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        Assertions.assertEquals(4, dummyLoader.requests.size());
    }

    @Test
    void testLoadDocumentsWithDifferentOptions() throws JsonLdError {
        DummyLoader dummyLoader = new DummyLoader();
        LRUDocumentCache cachedLoader = new LRUDocumentCache(dummyLoader, 2);

        // Using options with same inside should lead to cache hit.
        DocumentLoaderOptions options = new DocumentLoaderOptions();
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        DocumentLoaderOptions sameOptions = new DocumentLoaderOptions();
        cachedLoader.loadDocument(URI.create("http://localhost/1"), sameOptions);
        Assertions.assertEquals(1, dummyLoader.requests.size());

        // Use of different options should cause cache miss.
        DocumentLoaderOptions differentOptions = new DocumentLoaderOptions();
        differentOptions.setProfile("profile");
        cachedLoader.loadDocument(URI.create("http://localhost/1"), differentOptions);
        Assertions.assertEquals(2, dummyLoader.requests.size());
    }

}
