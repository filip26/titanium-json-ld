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

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.SuiteEvironment;
import com.apicatalog.jsonld.test.JunitRunner;
import com.apicatalog.jsonld.test.TestCase;
import com.apicatalog.jsonld.test.TestManifest;

public class CustomTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testCustom(TestCase testCase) {
        assertTrue(new JunitRunner(testCase).execute());
    }

    static final Stream<TestCase> data() throws JsonLdException {
        return TestManifest
                .load(
                        "classpath:/com/apicatalog/jsonld/test/", 
                        "manifest.json", 
                        SuiteEvironment.CLASSPATH_LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                .filter(test -> !"#t0008".equals(test.id)) // requires mock server
        ;
    }
}
