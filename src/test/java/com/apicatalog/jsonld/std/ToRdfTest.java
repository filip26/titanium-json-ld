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
package com.apicatalog.jsonld.std;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeFalse;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.util.Locale;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.SuiteEvironment;
import com.apicatalog.jsonld.test.JunitRunner;
import com.apicatalog.jsonld.test.TestCase;
import com.apicatalog.jsonld.test.TestManifest;

@DisplayName(value = "ToRDF")
public class ToRdfTest {

    @BeforeAll
    public static void beforeAll() {
        assumeTrue(SuiteEvironment.isRunning);
    }
    
    @ParameterizedTest(name = "{0}")
    @MethodSource({"jsonLdApi"})
    void testToRdf(final TestCase testCase) {

        // Force a locale to something different than US to be aware of DecimalFormat errors
        Locale.setDefault(Locale.GERMAN);

        // blank nodes as predicates are not supported - wont'fix
        assumeFalse("#te075".equals(testCase.id));
        // invalid IRI/URI are not accepted - wont'fix
        assumeFalse("#tli12".equals(testCase.id));

        assertTrue(new JunitRunner(testCase).execute());
    }

    static final Stream<TestCase> jsonLdApi() throws JsonLdException {
        return TestManifest
                    .load(
                            TestManifest.JSON_LD_API_BASE, 
                            "toRdf-manifest.jsonld", 
                            SuiteEvironment.LOADER)
                    .stream()
                    .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                    ;
    }
}