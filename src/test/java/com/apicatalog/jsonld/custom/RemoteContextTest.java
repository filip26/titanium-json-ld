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
package com.apicatalog.jsonld.custom;

import static java.time.Duration.ofMinutes;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTimeout;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.time.Duration;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.lang.JsonLdComparison;
import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;
import com.apicatalog.rdf.nquads.NQuadsReader;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.set.OrderedQuadDataset;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;

import jakarta.json.JsonArray;
import jakarta.json.JsonValue;

class RemoteContextTest {

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/61">Issue
     *      #61</a>
     */
    @Test
    void testToRdfMissingTriples1() throws JsonLdError, IOException, NQuadsReaderException, RdfConsumerException {

        final Document document = readDocument("/com/apicatalog/jsonld/test/issue61-in.json");
        final Document context = readDocument("/com/apicatalog/jsonld/test/issue61-context.json");

        final OrderedQuadDataset result = new OrderedQuadDataset();

        JsonLd.toRdf(document).context(context).provide(new QuadAcceptor(result));

        try (final InputStream is = getClass().getResourceAsStream("/com/apicatalog/jsonld/test/issue61-out.nq")) {

            assertNotNull(is);

            RdfQuadSet expected = new OrderedQuadDataset();

            new NQuadsReader(new InputStreamReader(is)).provide(new QuadAcceptor(expected));

            boolean match = RdfComparison.equals(result, expected);

            assertTrue(match);
        }
    }

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/61">Issue
     *      #61</a>
     */
    @Test
    void testToRdfMissingTriples2() throws JsonLdError, IOException, NQuadsReaderException, RdfConsumerException {

        final Document document = readDocument("/com/apicatalog/jsonld/test/issue61-in.json");
        final Document context = readDocument("/com/apicatalog/jsonld/test/issue61-context.json");

        final JsonLdOptions options = new JsonLdOptions();
        options.setExpandContext(context);

        final OrderedQuadSet result = new OrderedQuadSet();

        JsonLd.toRdf(document).options(options).provide(new QuadAcceptor(result));

        try (final InputStream is = getClass().getResourceAsStream("/com/apicatalog/jsonld/test/issue61-out.nq")) {

            assertNotNull(is);

            final OrderedQuadSet expected = new OrderedQuadSet();

            new NQuadsReader(new InputStreamReader(is)).provide(new QuadAcceptor(expected));

            boolean match = RdfComparison.equals(result, expected);

            assertTrue(match);
        }
    }

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/63">Issue
     *      #63</a>
     */
    @Test
    void testRemoteContext() throws JsonLdError, IOException, NQuadsReaderException, RdfConsumerException {

        final Document document = readDocument("/com/apicatalog/jsonld/test/issue63-in.json");

        final OrderedQuadSet result = new OrderedQuadSet();

        JsonLd.toRdf(document).loader(new ClasspathLoader()).provide(new QuadAcceptor(result));

        assertNotNull(result);

        try (final InputStream is = getClass().getResourceAsStream("/com/apicatalog/jsonld/test/issue63-out.nq")) {

            assertNotNull(is);

            final OrderedQuadSet expected = new OrderedQuadSet();

            new NQuadsReader(new InputStreamReader(is)).provide(new QuadAcceptor(expected));

            boolean match = RdfComparison.equals(result, expected);

            assertTrue(match);
        }
    }

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/62">Issue
     *      #62</a>
     *
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    @Disabled("Run manually")
    void testPerformance() throws JsonLdError, IOException {

        final Document document = readDocument("/com/apicatalog/jsonld/test/issue62-in.json");
        assertNotNull(document);

        final Document expected = readDocument("/com/apicatalog/jsonld/test/issue62-out.json");
        assertNotNull(expected);

        assertTimeout(ofMinutes(1), () -> {

            long start = System.nanoTime();

//FIXME            final JsonArray result = JsonLd.expand(document).get();
            JsonArray result = JsonValue.EMPTY_JSON_ARRAY;

            System.out.println("Time elapsed: " + Duration.ofNanos(System.nanoTime() - start));

            assertNotNull(result);

            boolean match = JsonLdComparison.equals(result, JakartaAdapter.instance(),
                    (JsonValue) expected.getJsonContent().orElse(null), JakartaAdapter.instance()

            );

            assertTrue(match);
        });

    }

    private final Document readDocument(final String name) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            return JsonDocument.of(is);
        }
    }
}
