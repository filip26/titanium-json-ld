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

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.NoSuchElementException;

import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLd;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.loader.ZipResourceLoader;
import no.hasmac.jsonld.test.JsonLdManifestLoader;
import no.hasmac.jsonld.test.JsonLdTestCase;
import no.hasmac.jsonld.test.JsonLdTestRunnerJunit;

class CompactBaseTest {

    @Test
    void testCompactBase() {

        final JsonLdTestCase testCase = JsonLdManifestLoader
                .load(JsonLdManifestLoader.JSON_LD_API_BASE, "compact-manifest.jsonld", new ZipResourceLoader())
                .stream()
                .filter(o -> "#t0047".equals(o.id))
                .findFirst().orElseThrow(() -> new NoSuchElementException());

        assertTrue(new JsonLdTestRunnerJunit(testCase).execute(options ->
                        JsonDocument.of(
                                JsonLd.compact(testCase.input, testCase.context).options(options).base("http://fake.com").get()
        )));
    }
}
