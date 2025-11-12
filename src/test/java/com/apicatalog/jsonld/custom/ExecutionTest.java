/*
 * Copyright 2025 the original author or authors.
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.jsonld.processor.Execution;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.processor.TypeMapper;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.java.NativeAdapter;

class ExecutionTest {

    @Test
    void testExpandTimeout() {
        var ex = assertThrows(JsonLdException.class, () -> JsonLd.expand(
                Map.of(),
                Options.newOptions().timeout(Duration.ofNanos(0))));
        assertEquals(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED, ex.code());
    }

    @Test
    void testContextKeyCollector() throws JsonLdException, TreeIOException, IOException {

        var document = read("/com/apicatalog/jsonld/test/vc-utopia.jsonld");

        var loader = StaticLoader.newBuilder()
                .set("https://www.w3.org/ns/credentials/v2", read("/com/apicatalog/jsonld/loader/credentials-v2.jsonld"))
                .set("https://w3id.org/vc-barcodes/v1", read("/com/apicatalog/jsonld/loader/vc-barcodes-v1.jsonld"))
                .set("https://w3id.org/utopia/v2", read("/com/apicatalog/jsonld/loader/utopia-v2-context.jsonld"))
                .build();

        var options = Options.with(loader);

        var keys = new ArrayList<Collection<String>>();

        var runtime = Execution.of(options);
        runtime.contextKeyCollector(keys::add);

        var expanded = Expander.expand(document, options, runtime);

        assertNotNull(expanded);

//        System.out.println(keys
//                .stream()
//                .map(c -> c.stream().filter(Predicate.not(Keywords::contains)).sorted())
//                .flatMap(Function.identity())
//                .collect(Collectors.toCollection(LinkedHashSet::new)));

        var expected = read("/com/apicatalog/jsonld/test/vc-utopia-terms.json");

        var match = TreeIO.deepEquals(Map.of("contextKeys", keys), NativeAdapter.instance(), expected.node(), expected.adapter());

        assertTrue(match);
    }

    @Test
    void testTypeMapper() throws JsonLdException, TreeIOException, IOException {

        var document = read("/com/apicatalog/jsonld/test/vc-utopia.jsonld");

        var loader = StaticLoader.newBuilder()
                .set("https://www.w3.org/ns/credentials/v2", read("/com/apicatalog/jsonld/loader/credentials-v2.jsonld"))
                .set("https://w3id.org/vc-barcodes/v1", read("/com/apicatalog/jsonld/loader/vc-barcodes-v1.jsonld"))
                .set("https://w3id.org/utopia/v2", read("/com/apicatalog/jsonld/loader/utopia-v2-context.jsonld"))
                .build();

        var options = Options.with(loader);

        var typeMap = new LinkedHashMap<String, Object>();

        var typeMapper = new TypeMapper() {

            Deque<Map<String, Object>> stack = new ArrayDeque<>();

            {
                stack.push(typeMap);
            }

            @Override
            public void beginMap(String key) {
                var map = new LinkedHashMap<String, Object>();
                stack.peek().put(key, map);
                stack.push(map);
            }

            @Override
            public void endMap() {
                stack.pop();
            }

            @Override
            public void mapType(String key, String type) {
                stack.peek().put(key, type);
            }

            @Override
            public void mapType(String key, String type, String value) {

                switch (type) {
                case Keywords.ID:
                    stack.peek().put(key, Map.of(
                            Keywords.ID, value,
                            Keywords.TYPE, Keywords.ID));
                    break;
                case Keywords.TYPE:
                    stack.peek().put(key, Map.of(
                            Keywords.TYPE, value));
                    break;
                case Keywords.VOCAB:
                    stack.peek().put(key, Map.of(
                            Keywords.ID, value,
                            Keywords.TYPE, Keywords.VOCAB));
                    break;

                default:
                    throw new IllegalStateException();
                }
            }
        };

        var runtime = Execution.of(options);
        runtime.typeMapper(typeMapper);

        var expanded = Expander.expand(document, options, runtime);
        assertNotNull(expanded);

//        System.out.println(typeMap);
        var expected = read("/com/apicatalog/jsonld/test/vc-utopia-typemap.json");

        var match = TreeIO.deepEquals(typeMap, NativeAdapter.instance(), expected.node(), expected.adapter());

        assertTrue(match);
    }

    private final TreeIO read(final String name) throws JsonLdException, TreeIOException, IOException {
        try (final var is = getClass().getResourceAsStream(name)) {
            return JakartaTestSuite.PARSER.parse(is);
        }
    }
}
