/*
 * Copyright 2026 the original author or authors.
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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTimeout;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;

class FramingReferenceArrayPerformanceTest {

    @ParameterizedTest(name = "{0} items, {1}")
    @MethodSource("data")
    void testFramingReferenceArrays(int itemCount, Duration timeout) throws JsonLdException {

        var data = new TestData(buildTestDocument(itemCount), buildTestFrame());

        assertTimeout(
                timeout,
                () -> {
                    long executionTime = measureFramingPerformance(data);
                    System.out.println("Framing %d shared-reference items took: %d ms.".formatted(itemCount, executionTime));
                });
    }

    static Collection<Object[]> data() {
        return List.of(
                new Object[] { 1_000, Duration.ofSeconds(2) },
                new Object[] { 10_000, Duration.ofSeconds(5) });
    }

    private long measureFramingPerformance(TestData data) throws JsonLdException {

        long startTime = System.currentTimeMillis();

        var framed = JsonLd.frame(data.document(), data.frame(), Options.newOptions());

        long endTime = System.currentTimeMillis();

        assertNotNull(framed);

        return endTime - startTime;
    }

    private Map<String, Object> buildTestDocument(int itemCount) {

        List<Object> graph = new ArrayList<>(itemCount + 100);
        int sharedTagCount = 100;
        int tagsPerItem = 12;

        for (int tagIndex = 0; tagIndex < sharedTagCount; tagIndex++) {
            graph.add(node(
                    "@id", "http://example.com/tag" + tagIndex,
                    "@type", "Tag",
                    "name", "Tag " + tagIndex));
        }

        for (int itemIndex = 0; itemIndex < itemCount; itemIndex++) {

            List<Object> tags = new ArrayList<>(tagsPerItem);

            for (int tagOffset = 0; tagOffset < tagsPerItem; tagOffset++) {
                tags.add("http://example.com/tag" + ((itemIndex + tagOffset) % sharedTagCount));
            }

            Map<String, Object> item = node(
                    "@id", "http://example.com/item" + itemIndex,
                    "@type", "Item",
                    "name", "Item " + itemIndex,
                    "tags", tags);

            if (itemIndex > 0) {
                item.put("related", "http://example.com/item" + (itemIndex - 1));
            }

            if (itemIndex + 1 < itemCount) {
                item.put("next", "http://example.com/item" + (itemIndex + 1));
            }

            graph.add(item);
        }

        return node(
                "@context", node("@vocab", "http://example.com/vocab#"),
                "@graph", graph);
    }

    private Map<String, Object> buildTestFrame() {
        return node(
                "@context", node("@vocab", "http://example.com/vocab#"),
                "@type", "Item",
                "tags", node(
                        "@embed", "@once",
                        "@type", "Tag"),
                "related", node(
                        "@embed", "@once",
                        "@type", "Item"),
                "next", node(
                        "@embed", "@once",
                        "@type", "Item"));
    }

    private static Map<String, Object> node(Object... entries) {
        Map<String, Object> map = new LinkedHashMap<>(entries.length / 2);

        for (int index = 0; index < entries.length; index += 2) {
            map.put((String) entries[index], entries[index + 1]);
        }

        return map;
    }

    private record TestData(Map<String, Object> document, Map<String, Object> frame) {
    }
}
