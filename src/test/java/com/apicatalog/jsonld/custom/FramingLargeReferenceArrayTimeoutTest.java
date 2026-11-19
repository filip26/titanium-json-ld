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
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;

import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;

@EnabledIfSystemProperty(named = "titanium.perf.large", matches = "true")
class FramingLargeReferenceArrayTimeoutTest {

    private static final int ITEM_COUNT = 1_000_000;
    private static final Duration TIMEOUT = Duration.ofMinutes(1);

    @Test
    void testLargeReferenceArrayFinishesWithinOneMinute() {

        final var document = buildTestDocument(ITEM_COUNT);
        final var frame = buildTestFrame();
        final var elapsedMillis = new AtomicLong();

        assertTimeoutPreemptively(
                TIMEOUT,
                () -> {
                    final long startedAt = System.nanoTime();
                    final var framed = JsonLd.frame(document, frame, Options.newOptions());
                    elapsedMillis.set((System.nanoTime() - startedAt) / 1_000_000L);
                    assertNotNull(framed);
                });

        System.out.println("Framing %d hub-array items took: %d ms.".formatted(ITEM_COUNT, elapsedMillis.get()));
    }

    private Map<String, Object> buildTestDocument(final int itemCount) {

        final List<Object> graph = new ArrayList<>(itemCount + 1);
        final List<Object> contains = new ArrayList<>(itemCount);

        for (int itemIndex = 0; itemIndex < itemCount; itemIndex++) {
            final String itemId = "http://example.com/item" + itemIndex;
            contains.add(itemId);
            graph.add(node(
                    "@id", itemId,
                    "@type", "Item"));
        }

        graph.add(node(
                "@id", "http://example.com/root",
                "@type", "Collection",
                "contains", contains));

        final Map<String, Object> context = node(
                "@vocab", "http://example.com/vocab#",
                "contains", node("@type", "@id"));

        return node(
                "@context", context,
                "@graph", graph);
    }

    private Map<String, Object> buildTestFrame() {
        final Map<String, Object> context = node(
                "@vocab", "http://example.com/vocab#",
                "contains", node("@type", "@id"));

        return node(
                "@context", context,
                "@type", "Collection",
                "contains", node(
                        "@embed", "@once",
                        "@type", "Item"));
    }

    private static Map<String, Object> node(Object... entries) {
        final Map<String, Object> map = new LinkedHashMap<>(entries.length / 2);

        for (int index = 0; index < entries.length; index += 2) {
            map.put((String) entries[index], entries[index + 1]);
        }

        return map;
    }
}
