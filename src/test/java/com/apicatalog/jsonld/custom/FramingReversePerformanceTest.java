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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTimeout;

import java.io.IOException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;

class FramingReversePerformanceTest {

    @ParameterizedTest(name = "{0} elemets, {1}")
    @MethodSource("data")
    void testFramingPerformance(int elements, Duration timeout) throws JsonLdException, IOException {

        final var document = buildTestDocument(elements);
        final var frame = buildTestFrame();

        assertTimeout(
                timeout,
                () -> {
                    final long executionTime = measureFramingPerformance(document, frame);
                    System.out.println("Framing %d elements took: %d ms.".formatted(elements, executionTime));
                });
    }

    static final Collection<Object[]> data() {
        return List.of(
                new Object[] { 1_000, Duration.ofSeconds(1) },
                new Object[] { 10_000, Duration.ofSeconds(2) });
    }

    private long measureFramingPerformance(Map<String, ?> document, Map<String, ?> frame) throws JsonLdException, IOException {

        final long startTime = System.currentTimeMillis();

        final var framed = JsonLd.frame(document, frame, Options.newOptions());

        final long endTime = System.currentTimeMillis();

        assertNotNull(framed);

        return endTime - startTime;
    }

    private Map<String, Object> buildTestDocument(final int elementCount) {

        final var data = new ArrayList<>();

        final int departmentCount = Math.max(5, elementCount / 100);
        final int personsPerDepartment = elementCount / departmentCount;

        for (int departmentIndex = 0; departmentIndex < departmentCount; departmentIndex++) {

            final String departmentId = "http://example.com/department" + departmentIndex;

            data.add(Map.of(
                    "@id", departmentId,
                    "@type", "Department",
                    "name", "Department " + departmentIndex));

            for (int personOffset = 0; personOffset < personsPerDepartment; personOffset++) {

                final int personIndex = departmentIndex * personsPerDepartment + personOffset;
                final var personBuilder = new HashMap<String, Object>(Map.of(
                        "@id", "http://example.com/person" + personIndex,
                        "@type", "Person",
                        "name", "Person " + personIndex,
                        "memberOf", departmentId));

                if (personOffset == 0 && departmentIndex > 0) {
                    personBuilder.put("manages", departmentId);
                }

                if (personIndex > 0 && personIndex % 3 == 0) {
                    final var relatedBuilder = new ArrayList<>();
                    relatedBuilder.add("http://example.com/person" + (personIndex - 1));

                    if (personIndex < elementCount - 1) {
                        relatedBuilder.add("http://example.com/person" + (personIndex + 1));
                    }

                    if (personIndex >= 5 && personIndex % 5 == 0) {
                        relatedBuilder.add("http://example.com/person" + (personIndex - 5));
                    }

                    personBuilder.put("relatedTo", relatedBuilder);
                }

                final int itemsPerPerson = 3;
                for (int itemOffset = 0; itemOffset < itemsPerPerson; itemOffset++) {

                    final int itemIndex = personIndex * itemsPerPerson + itemOffset;
                    final var itemBuilder = new HashMap<String, Object>(Map.of(
                            "@id", "http://example.com/item" + itemIndex,
                            "@type", "Item",
                            "name", "Item " + itemIndex,
                            "parent", "http://example.com/person" + personIndex));

                    if (itemOffset > 0) {
                        itemBuilder.put("child", "http://example.com/item" + (itemIndex - 1));
                    }

                    data.add(itemBuilder);
                }

                data.add(personBuilder);
            }
        }

        return Map.of(
                "@context", Map.of("@vocab", "http://example.com/vocab#"),
                "@graph", data);
    }

    private Map<String, Object> buildTestFrame() {
        return Map.of(
                "@context", Map.of("@vocab", "http://example.com/vocab#"),
                "@type", "Person",
                "@reverse", Map.of("memberOf", Map.of("@type", "Department")),
                "parent", Map.of("@type", "Item"),
                "relatedTo", Map.of("@type", "Person"));

    }
}