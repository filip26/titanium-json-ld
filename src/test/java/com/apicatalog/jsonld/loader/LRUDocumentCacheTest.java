package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.DocumentLoader.Options;

import jakarta.json.JsonValue;

public class LRUDocumentCacheTest {

    static class Request {

        final URI url;

        final Options options;

        public Request(URI url, Options options) {
            this.url = url;
            this.options = options;
        }
    }

    static class RecordRequestLoader implements DocumentLoader {

        final List<Request> requests = new ArrayList<>();

        @Override
        public Document loadDocument(URI url, Options options) {
            requests.add(new Request(url, options));
            // Return empty document.
            return JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        }

    }

    @Test
    void testLoadDocument() throws JsonLdException {
        RecordRequestLoader loader = new RecordRequestLoader();
        CacheLoader cachedLoader = new CacheLoader(loader, 2);

        Options options = Options.DEFAULT;
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        // There should be only one call as all other should be cached.
        Assertions.assertEquals(1, loader.requests.size());

        // Make sure valid arguments were passed.
        Request request = loader.requests.get(0);
        Assertions.assertEquals("http://localhost/1",request.url.toString());
        Assertions.assertSame(options,request.options);
    }

    @Test
    void testCacheSize() throws JsonLdException {
        RecordRequestLoader loader = new RecordRequestLoader();
        CacheLoader cachedLoader = new CacheLoader(loader, 2);

        Options options = Options.DEFAULT;
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        cachedLoader.loadDocument(URI.create("http://localhost/2"), options);
        cachedLoader.loadDocument(URI.create("http://localhost/2"), options);

        // There should be only one call as all other should be cached.
        Assertions.assertEquals(2, loader.requests.size());

        // Request of new resource.
        cachedLoader.loadDocument(URI.create("http://localhost/3"), options);
        Assertions.assertEquals(3, loader.requests.size());

        // Using LRU the first resources should not be in cache anymore.
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        Assertions.assertEquals(4, loader.requests.size());
    }

    @Test
    void testLoadDocumentsWithDifferentOptions() throws JsonLdException {
        RecordRequestLoader loader = new RecordRequestLoader();
        CacheLoader cachedLoader = new CacheLoader(loader, 2);

        // Using options with same inside should lead to cache hit.
        Options options = Options.DEFAULT;
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        Options sameOptions = Options.DEFAULT;
        cachedLoader.loadDocument(URI.create("http://localhost/1"), sameOptions);
        Assertions.assertEquals(1, loader.requests.size());

        // Use of different options should cause cache miss.
        Options differentOptions = new Options(false, "profile", null);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), differentOptions);
        Assertions.assertEquals(2, loader.requests.size());
    }

    @Test
    void testCachingEqualOptions() throws JsonLdException {
        RecordRequestLoader loader = new RecordRequestLoader();
        CacheLoader cachedLoader = new CacheLoader(loader, 2);
        
        Options options = new Options(true, "profile", List.of("first", "second"));
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        options = new Options(true, "profile", List.of("first", "second"));
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        options = new Options(true, "profile", List.of("first", "second"));
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        Assertions.assertEquals(1, loader.requests.size());
    }

    @Test
    void testCachingProfilesOrderMatter() throws JsonLdException {
        RecordRequestLoader loader = new RecordRequestLoader();
        CacheLoader cachedLoader = new CacheLoader(loader, 2);
        Options options = null;

        options = new Options(true, "profile", List.of("first", "second"));
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        options = new Options(true, "profile", List.of("second", "first"));
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        Assertions.assertEquals(2, loader.requests.size());
    }


}
