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

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.test.JsonLdManifestLoader;
import com.apicatalog.jsonld.test.JsonLdTestCase;
import com.apicatalog.jsonld.test.JsonLdTestRunnerJunit;
import com.apicatalog.jsonld.test.loader.ZipResourceLoader;

class CompactTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testCompact(JsonLdTestCase testCase) {
        assertTrue(new JsonLdTestRunnerJunit(testCase).execute());
    }

    static final Stream<JsonLdTestCase> data() throws JsonLdError {        
        return JsonLdManifestLoader
                .load(JsonLdManifestLoader.JSON_LD_API_BASE, "compact-manifest.jsonld", new ZipResourceLoader())
                .stream()
                .filter(test -> !Version.V1_0.equals(test.options.specVersion)) // skip specVersion == 1.0
                ;
        
    }
}
