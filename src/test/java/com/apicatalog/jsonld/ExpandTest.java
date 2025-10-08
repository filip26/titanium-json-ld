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
package com.apicatalog.jsonld;


import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.jsonld.test.JsonLdManifestLoader;
import com.apicatalog.jsonld.test.JsonLdTestCase;
import com.apicatalog.jsonld.test.JsonLdTestRunnerJunit;

@DisplayName(value = "XY")
class ExpandTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource({"jsonLdApi", "jsonLdStar"})
    void testExpand(final JsonLdTestCase testCase) {

        // Skip JSON-LD-STAR (Experimental) negative test (an invalid annotation inside @list)
        assumeFalse("#tst26".equals(testCase.id));

        assertTrue(new JsonLdTestRunnerJunit(testCase).execute());
    }

    static final Stream<JsonLdTestCase> jsonLdApi() throws JsonLdError {
        return JsonLdManifestLoader
                    .load(JsonLdManifestLoader.JSON_LD_API_BASE, "expand-manifest.jsonld", new ZipResourceLoader())
                    .stream()
                    .filter(JsonLdTestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                    ;
    }

    static final Stream<JsonLdTestCase> jsonLdStar() throws JsonLdError {
        return JsonLdManifestLoader
                    .load(JsonLdManifestLoader.JSON_LD_STAR_BASE, "expand-manifest.jsonld", new ZipResourceLoader())
                    .stream()
                    ;
    }

}
