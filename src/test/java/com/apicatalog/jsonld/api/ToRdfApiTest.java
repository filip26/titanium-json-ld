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

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfNQuad;
import jakarta.json.JsonValue;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.*;

class ToRdfApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(JsonValue.EMPTY_JSON_ARRAY);

    @Test
    void test1() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test2() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes()))).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test3() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("https://example.com").loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test4() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(URI.create("https://example.com")).loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test5() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com  ").loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test6() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com  ").context(JsonValue.EMPTY_JSON_OBJECT).loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test7() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com").context(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).loader(MOCK_LOADER).ordered().get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test8() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com").context(JsonDocument.of(MediaType.JSON, new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes())))).loader(MOCK_LOADER).ordered().get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test9() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).context(JsonValue.EMPTY_JSON_OBJECT).loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test10() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).context(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).loader(MOCK_LOADER).ordered().get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test11() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).context(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes()))).loader(MOCK_LOADER).ordered().get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    private final String invalidUri = " http://example.com/invalid";
    private final String expandedInvalidSubject = "/com/apicatalog/jsonld/test/issue383-expanded-subject.json";

    @Test
    void test12() throws JsonLdError, IOException {
        RdfDataset result = readRdfDataset(expandedInvalidSubject, UriValidationPolicy.None);

        boolean includeInvalidSubject = anyInDataset(result, rdfNQuad -> rdfNQuad.getSubject().getValue().equals(invalidUri));

        assertTrue(includeInvalidSubject, "The resulting dataset without uri validation should include :" + invalidUri);
    }

    @Test
    void test13() throws JsonLdError, IOException {
        RdfDataset result = readRdfDataset(expandedInvalidSubject, UriValidationPolicy.Full);

        boolean includeInvalidSubject = anyInDataset(result, rdfNQuad -> rdfNQuad.getSubject().getValue().equals(invalidUri));

        assertFalse(includeInvalidSubject, "The resulting dataset with uri validation should NOT include :" + invalidUri);
    }

    private final String expandedInvalidPredicate = "/com/apicatalog/jsonld/test/issue383-expanded-predicate.json";

    @Test
    void test14() throws JsonLdError, IOException {
        RdfDataset result = readRdfDataset(expandedInvalidPredicate, UriValidationPolicy.None);

        boolean includeInvalidPredicate = anyInDataset(result, rdfNQuad -> rdfNQuad.getPredicate().getValue().equals(invalidUri));

        assertTrue(includeInvalidPredicate, "The resulting dataset without uri validation should include :" + invalidUri);
    }

    @Test
    void test15() throws JsonLdError, IOException {
        RdfDataset result = readRdfDataset(expandedInvalidPredicate, UriValidationPolicy.Full);

        boolean includeInvalidPredicate = anyInDataset(result, rdfNQuad -> rdfNQuad.getPredicate().getValue().equals(invalidUri));

        assertFalse(includeInvalidPredicate, "The resulting dataset with uri validation should NOT include :" + invalidUri);
    }

    private final String expandedInvalidObject = "/com/apicatalog/jsonld/test/issue383-expanded-object.json";

    @Test
    void test16() throws JsonLdError, IOException {
        RdfDataset result = readRdfDataset(expandedInvalidObject, UriValidationPolicy.None);

        boolean includeInvalidObject = anyInDataset(result, rdfNQuad -> rdfNQuad.getObject().getValue().equals(invalidUri));

        assertTrue(includeInvalidObject, "The resulting dataset without uri validation should include :" + invalidUri);
    }

    @Test
    void test17() throws JsonLdError, IOException {
        RdfDataset result = readRdfDataset(expandedInvalidObject, UriValidationPolicy.Full);

        boolean includeInvalidObject = anyInDataset(result, rdfNQuad -> rdfNQuad.getObject().getValue().equals(invalidUri));

        assertFalse(includeInvalidObject, "The resulting dataset with uri validation should NOT include :" + invalidUri);
    }

    private boolean anyInDataset(final RdfDataset dataset, Predicate<RdfNQuad> predicate) {
        return dataset.toList().stream().anyMatch(predicate);
    }

    private RdfDataset readRdfDataset(final String name, final UriValidationPolicy uriValidation) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            JsonLdOptions options = new JsonLdOptions();
            options.setUriValidation(uriValidation);
            Document document = JsonDocument.of(is);

            return JsonLd.toRdf(document)
                    .options(options).get();
        }
    }
}
