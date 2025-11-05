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

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdTestSuite;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.test.JsonLdTestManifest;
import com.apicatalog.jsonld.test.JsonLdTestRunnerJunit;

class CompactBaseTest {

    @Test
    void testCompactBase() {

        final var testCase = JsonLdTestManifest
                .load(
                        JsonLdTestManifest.JSON_LD_API_BASE,
                        "compact-manifest.jsonld",
                        JsonLdTestSuite.ZIP_RESOURCE_LOADER)
                .stream()
                .filter(o -> "#t0047".equals(o.id))
                .findFirst().orElseThrow(() -> new IllegalStateException());

        assertTrue(new JsonLdTestRunnerJunit(testCase)
                .execute(options -> JsonLd.compact(
                        testCase.input,
                        testCase.context,
                        Options.copyOf(options)
                                .base(URI.create("http://fake.com")))));
    }
}
