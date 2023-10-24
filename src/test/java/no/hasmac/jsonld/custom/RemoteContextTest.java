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
package no.hasmac.jsonld.custom;

import static java.time.Duration.ofMinutes;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTimeout;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.time.Duration;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLd;
import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.document.RdfDocument;
import no.hasmac.jsonld.json.JsonLdComparison;
import no.hasmac.jsonld.loader.ClasspathLoader;
import no.hasmac.rdf.RdfComparison;
import no.hasmac.rdf.RdfDataset;

import jakarta.json.JsonArray;

class RemoteContextTest {

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/61">Issue #61</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    void testToRdfMissingTriples1() throws JsonLdError, IOException {

        final Document document = readDocument("/no/hasmac/jsonld/test/issue61-in.json");
        final Document context = readDocument("/no/hasmac/jsonld/test/issue61-context.json");

        final RdfDataset result = JsonLd.toRdf(document).context(context).get();

        assertNotNull(result);

        try (final InputStream is = getClass().getResourceAsStream("/no/hasmac/jsonld/test/issue61-out.nq")) {

            assertNotNull(is);

            boolean match = RdfComparison.equals(result, RdfDocument.of(is).getRdfContent().orElse(null));

            assertTrue(match);
        }
    }

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/61">Issue #61</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    void testToRdfMissingTriples2() throws JsonLdError, IOException {

        final Document document = readDocument("/no/hasmac/jsonld/test/issue61-in.json");
        final Document context = readDocument("/no/hasmac/jsonld/test/issue61-context.json");

        final JsonLdOptions options = new JsonLdOptions();
        options.setExpandContext(context);

        final RdfDataset result = JsonLd.toRdf(document).options(options).get();

        assertNotNull(result);

        try (final InputStream is = getClass().getResourceAsStream("/no/hasmac/jsonld/test/issue61-out.nq")) {

            assertNotNull(is);

            boolean match = RdfComparison.equals(result, RdfDocument.of(is).getRdfContent().orElse(null));

            assertTrue(match);
        }
    }

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/63">Issue #63</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    void testRemoteContext() throws JsonLdError, IOException {

        final Document document = readDocument("/no/hasmac/jsonld/test/issue63-in.json");

        final RdfDataset result = JsonLd.toRdf(document).loader(new ClasspathLoader()).get();

        assertNotNull(result);

        try (final InputStream is = getClass().getResourceAsStream("/no/hasmac/jsonld/test/issue63-out.nq")) {

            assertNotNull(is);

            boolean match = RdfComparison.equals(result, RdfDocument.of(is).getRdfContent().orElse(null));

            assertTrue(match);
        }
    }

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/62">Issue #62</a>
     *
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    @Disabled("Run manually")
    void testPerformance() throws JsonLdError, IOException {

        final Document document = readDocument("/no/hasmac/jsonld/test/issue62-in.json");
        assertNotNull(document);

        final Document expected = readDocument("/no/hasmac/jsonld/test/issue62-out.json");
        assertNotNull(expected);

        assertTimeout(ofMinutes(1), () -> {

            long start = System.nanoTime();

            final JsonArray result = JsonLd.expand(document).get();

            System.out.println("Time elapsed: " + Duration.ofNanos(System.nanoTime() - start));

            assertNotNull(result);

            boolean match = JsonLdComparison.equals(result, expected.getJsonContent().orElse(null));

            assertTrue(match);
        });

    }

    private Document readDocument(final String name) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            return JsonDocument.of(is);
        }
    }
}
