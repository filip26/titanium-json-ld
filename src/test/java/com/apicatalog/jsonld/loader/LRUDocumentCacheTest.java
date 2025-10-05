package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;

import jakarta.json.JsonValue;

public class LRUDocumentCacheTest {

    static class Request {

        final URI url;

        final LoaderOptions options;

        public Request(URI url, LoaderOptions options) {
            this.url = url;
            this.options = options;
        }

    }

    static class RecordRequestLoader implements JsonLdLoader {

        final List<Request> requests = new ArrayList<>();

        @Override
        public Document loadDocument(URI url, LoaderOptions options) {
            requests.add(new Request(url, options));
            // Return empty document.
            return JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        }

    }

    @Test
    void testLoadDocument() throws JsonLdError {
        RecordRequestLoader loader = new RecordRequestLoader();
        LRUDocumentCache cachedLoader = new LRUDocumentCache(loader, 2);

        LoaderOptions options = new LoaderOptions();
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
    void testCacheSize() throws JsonLdError {
        RecordRequestLoader loader = new RecordRequestLoader();
        LRUDocumentCache cachedLoader = new LRUDocumentCache(loader, 2);

        LoaderOptions options = new LoaderOptions();
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
    void testLoadDocumentsWithDifferentOptions() throws JsonLdError {
        RecordRequestLoader loader = new RecordRequestLoader();
        LRUDocumentCache cachedLoader = new LRUDocumentCache(loader, 2);

        // Using options with same inside should lead to cache hit.
        LoaderOptions options = new LoaderOptions();
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);
        LoaderOptions sameOptions = new LoaderOptions();
        cachedLoader.loadDocument(URI.create("http://localhost/1"), sameOptions);
        Assertions.assertEquals(1, loader.requests.size());

        // Use of different options should cause cache miss.
        LoaderOptions differentOptions = new LoaderOptions();
        differentOptions.setProfile("profile");
        cachedLoader.loadDocument(URI.create("http://localhost/1"), differentOptions);
        Assertions.assertEquals(2, loader.requests.size());
    }

    @Test
    void testCachingEqualOptions() throws JsonLdError {
        RecordRequestLoader loader = new RecordRequestLoader();
        LRUDocumentCache cachedLoader = new LRUDocumentCache(loader, 2);
        LoaderOptions options = null;

        options = new LoaderOptions();
        options.setProfile("profile");
        options.setExtractAllScripts(true);
        List<String> firstList = new ArrayList<>();
        firstList.add("first");
        firstList.add("second");
        options.setRequestProfile(firstList);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        options = new LoaderOptions();
        options.setProfile("profile");
        options.setExtractAllScripts(true);
        List<String> secondList = new ArrayList<>();
        secondList.add("first");
        secondList.add("second");
        options.setRequestProfile(secondList);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        options = new LoaderOptions();
        options.setProfile("profile");
        options.setExtractAllScripts(true);
        List<String> thirdList = new LinkedList<>();
        thirdList.add("first");
        thirdList.add("second");
        options.setRequestProfile(thirdList);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        Assertions.assertEquals(1, loader.requests.size());
    }

    @Test
    void testCachingProfilesOrderMatter() throws JsonLdError {
        RecordRequestLoader loader = new RecordRequestLoader();
        LRUDocumentCache cachedLoader = new LRUDocumentCache(loader, 2);
        LoaderOptions options = null;

        options = new LoaderOptions();
        options.setProfile("profile");
        options.setExtractAllScripts(true);
        List<String> firstList = new ArrayList<>();
        firstList.add("first");
        firstList.add("second");
        options.setRequestProfile(firstList);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        options = new LoaderOptions();
        options.setProfile("profile");
        options.setExtractAllScripts(true);
        List<String> secondList = new ArrayList<>();
        secondList.add("second");
        secondList.add("first");
        options.setRequestProfile(secondList);
        cachedLoader.loadDocument(URI.create("http://localhost/1"), options);

        Assertions.assertEquals(2, loader.requests.size());
    }


}
