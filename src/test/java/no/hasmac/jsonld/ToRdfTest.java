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
package no.hasmac.jsonld;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.io.IOException;
import java.util.Locale;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import no.hasmac.jsonld.loader.ZipResourceLoader;
import no.hasmac.jsonld.test.JsonLdManifestLoader;
import no.hasmac.jsonld.test.JsonLdTestCase;
import no.hasmac.jsonld.test.JsonLdTestRunnerJunit;

class ToRdfTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource({"jsonLdApi"})
    void testToRdf(final JsonLdTestCase testCase) throws IOException {

        // Force a locale to something different than US to be aware of DecimalFormat errors
        Locale.setDefault(Locale.GERMAN);

        // blank nodes as predicates are not supported - wont'fix
        assumeFalse("#te075".equals(testCase.id));
        // invalid IRI/URI are not accepted - wont'fix
        assumeFalse("#tli12".equals(testCase.id));

        assertTrue(new JsonLdTestRunnerJunit(testCase).execute());
    }

    static Stream<JsonLdTestCase> jsonLdApi() throws JsonLdError {
        return JsonLdManifestLoader
                    .load(JsonLdManifestLoader.JSON_LD_API_BASE, "toRdf-manifest.jsonld", new ZipResourceLoader())
                    .stream()
                    .filter(JsonLdTestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                    ;
    }
}
