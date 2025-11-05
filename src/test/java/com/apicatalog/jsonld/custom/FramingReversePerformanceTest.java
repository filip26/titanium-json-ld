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
import java.util.Collection;
import java.util.List;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;

class FramingReversePerformanceTest {

    @ParameterizedTest(name = "{0} elemets, {1}")
    @MethodSource("data")
    void testFramingPerformance(int elements, Duration timeout) throws JsonLdException, IOException {

        var data = buildTestData(elements);

        assertTimeout(
                timeout,
                () -> {
                    final long executionTime = measureFramingPerformance(data);
                    System.out.println("Framing %d elements took: %d ms.".formatted(elements, executionTime));
                });
    }

    static final Collection<Object[]> data() {
        return List.of(
                new Object[] { 1_000, Duration.ofSeconds(1) },
                new Object[] { 10_000, Duration.ofSeconds(2) });
    }

    private PolyNode[] buildTestData(final int elementCount) throws JsonLdException, IOException {

        final var document = new PolyNode(buildTestDocument(elementCount), JakartaAdapter.instance());
        final var frame = new PolyNode(buildTestFrame(), JakartaAdapter.instance());

        return new PolyNode[] { document, frame };
    }

    private long measureFramingPerformance(PolyNode[] data) throws JsonLdException, IOException {

        final long startTime = System.currentTimeMillis();

        final var framed = JsonLd.frame(data[0], data[1], Options.newOptions());

        final long endTime = System.currentTimeMillis();

        assertNotNull(framed);

        return endTime - startTime;
    }

    private JsonObject buildTestDocument(final int elementCount) {

        final JsonObjectBuilder contextBuilder = Json.createObjectBuilder()
                .add("@vocab", "http://example.com/vocab#");

        final JsonArrayBuilder dataBuilder = Json.createArrayBuilder();

        final int departmentCount = Math.max(5, elementCount / 100);
        final int personsPerDepartment = elementCount / departmentCount;

        for (int departmentIndex = 0; departmentIndex < departmentCount; departmentIndex++) {

            final String departmentId = "http://example.com/department" + departmentIndex;

            dataBuilder.add(Json.createObjectBuilder()
                    .add("@id", departmentId)
                    .add("@type", "Department")
                    .add("name", "Department " + departmentIndex));

            for (int personOffset = 0; personOffset < personsPerDepartment; personOffset++) {

                final int personIndex = departmentIndex * personsPerDepartment + personOffset;
                final JsonObjectBuilder personBuilder = Json.createObjectBuilder()
                        .add("@id", "http://example.com/person" + personIndex)
                        .add("@type", "Person")
                        .add("name", "Person " + personIndex)
                        .add("memberOf", departmentId);

                if (personOffset == 0 && departmentIndex > 0) {
                    personBuilder.add("manages", departmentId);
                }

                if (personIndex > 0 && personIndex % 3 == 0) {
                    final JsonArrayBuilder relatedBuilder = Json.createArrayBuilder();
                    relatedBuilder.add("http://example.com/person" + (personIndex - 1));

                    if (personIndex < elementCount - 1) {
                        relatedBuilder.add("http://example.com/person" + (personIndex + 1));
                    }

                    if (personIndex >= 5 && personIndex % 5 == 0) {
                        relatedBuilder.add("http://example.com/person" + (personIndex - 5));
                    }

                    personBuilder.add("relatedTo", relatedBuilder);
                }

                final int itemsPerPerson = 3;
                for (int itemOffset = 0; itemOffset < itemsPerPerson; itemOffset++) {

                    final int itemIndex = personIndex * itemsPerPerson + itemOffset;
                    final JsonObjectBuilder itemBuilder = Json.createObjectBuilder()
                            .add("@id", "http://example.com/item" + itemIndex)
                            .add("@type", "Item")
                            .add("name", "Item " + itemIndex)
                            .add("parent", "http://example.com/person" + personIndex);

                    if (itemOffset > 0) {
                        itemBuilder.add("child", "http://example.com/item" + (itemIndex - 1));
                    }

                    dataBuilder.add(itemBuilder);
                }

                dataBuilder.add(personBuilder);
            }
        }

        return Json.createObjectBuilder()
                .add("@context", contextBuilder)
                .add("@graph", dataBuilder)
                .build();
    }

    private JsonObject buildTestFrame() {
        return Json.createObjectBuilder()
                .add("@context", Json.createObjectBuilder()
                        .add("@vocab", "http://example.com/vocab#"))
                .add("@type", "Person")
                .add("@reverse", Json.createObjectBuilder()
                        .add("memberOf", Json.createObjectBuilder()
                                .add("@type", "Department"))
                        .add("parent", Json.createObjectBuilder()
                                .add("@type", "Item"))
                        .add("relatedTo", Json.createObjectBuilder()
                                .add("@type", "Person")))
                .build();
    }
}