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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.JsonDocument;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;

class FramingReversePerformanceTest {

    @Test
    void testFramingPerformance1000Elements() throws JsonLdError {

        final long executionTime = measureFramingPerformance(1_000);

        System.out.println("Framing 1_000 elements took: " + executionTime + " ms");

        assertTrue(executionTime < 1_000L,
                "Framing 1_000 elements took too long: " + executionTime + " ms");
    }

    @Test
    void testFramingPerformance10000Elements() throws JsonLdError {

        final long executionTime = measureFramingPerformance(10_000);

        System.out.println("Framing 10_000 elements took: " + executionTime + " ms");

        assertTrue(executionTime < 5_000L,
                "Framing 10_000 elements took too long: " + executionTime + " ms");
    }

    private long measureFramingPerformance(final int elementCount) throws JsonLdError {

        final JsonObject document = buildTestDocument(elementCount);
        final JsonObject frame = buildTestFrame();

        final long startTime = System.currentTimeMillis();

        final JsonObject framed = JsonLd.frame(JsonDocument.of(document), JsonDocument.of(frame)).get();

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