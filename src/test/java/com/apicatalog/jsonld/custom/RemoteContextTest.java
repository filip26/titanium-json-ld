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

import com.apicatalog.jsonld.Comparison;
import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.loader.CacheLoader;
import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;
import com.apicatalog.rdf.nquads.NQuadsReader;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.set.OrderedQuadDataset;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;
import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.java.JavaAdapter;

class RemoteContextTest {

    static ClasspathLoader LOADER = ClasspathLoader
            .newBuilder()
            .defaultParser(JakartaTestSuite.PARSER)
            .build();

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/61">Issue
     *      #61</a>
     */
    @Test
    void testToRdfMissingTriples1() throws JsonLdException, IOException, NQuadsReaderException, RdfConsumerException, TreeIOException {

        final var document = read("/com/apicatalog/jsonld/test/issue61-in.json");
        final var context = read("/com/apicatalog/jsonld/test/issue61-context.json");

        final OrderedQuadDataset result = new OrderedQuadDataset();

        JsonLd.toRdf(
                document,
                new QuadAcceptor(result),
                Options.newOptions()
                        .expandContext(context));

        try (final var is = getClass().getResourceAsStream("/com/apicatalog/jsonld/test/issue61-out.nq")) {

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
    void testToRdfMissingTriples2() throws JsonLdException, IOException, NQuadsReaderException, RdfConsumerException, TreeIOException {

        final var document = read("/com/apicatalog/jsonld/test/issue61-in.json");
        final var context = read("/com/apicatalog/jsonld/test/issue61-context.json");

        final OrderedQuadSet result = new OrderedQuadSet();

        JsonLd.toRdf(
                document,
                new QuadAcceptor(result),
                Options.newOptions().expandContext(context));

        try (final var is = getClass().getResourceAsStream("/com/apicatalog/jsonld/test/issue61-out.nq")) {

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
    void testRemoteContext() throws JsonLdException, IOException, NQuadsReaderException, RdfConsumerException, TreeIOException {

        final var document = read("/com/apicatalog/jsonld/test/issue63-in.json");

        final OrderedQuadSet result = new OrderedQuadSet();

        JsonLd.toRdf(
                document,
                new QuadAcceptor(result),
                Options.with(LOADER));

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
     * @throws JsonLdException
     * @throws TreeIOException
     * @throws IOException
     */
    @Test
    @Disabled
    void testPerformance() throws JsonLdException, TreeIOException, IOException {

        final var document = read("/com/apicatalog/jsonld/test/issue62-in.json");
        assertNotNull(document);

        final var expected = read("/com/apicatalog/jsonld/test/issue62-out.json");
        assertNotNull(expected);

        assertTimeout(ofMinutes(1), () -> {

            long start = System.nanoTime();

            final var loader = CacheLoader.of(JakartaTestSuite.HTTP_LOADER, 256);

            final var result = JsonLd.expand(document, Options.with(loader));

            System.out.println("Time elapsed: " + Duration.ofNanos(System.nanoTime() - start));
            System.out.println("Cached documents: " + loader.cache().size());

            assertNotNull(result);

            boolean match = Comparison.equals(
                    result, JavaAdapter.instance(),
                    expected.node(), expected.adapter());

            assertTrue(match);
        });
    }

    private final Tree read(final String name) throws JsonLdException, TreeIOException, IOException {
        try (final var is = getClass().getResourceAsStream(name)) {
            return JakartaTestSuite.PARSER.parse(is);
        }
    }
}
