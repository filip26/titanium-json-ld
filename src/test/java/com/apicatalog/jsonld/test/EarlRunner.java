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
package com.apicatalog.jsonld.test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.util.Objects;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.test.TestCase.Type;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;
import com.apicatalog.tree.io.java.JavaAdapter;

import jakarta.json.JsonStructure;

public class EarlRunner {

    private final TestCase testCase;

    public EarlRunner(TestCase testCase) {
        this.testCase = testCase;
    }

    public boolean execute(TestMethod method) {

        assertNotNull(testCase.baseUri);
        assertNotNull(testCase.input);

        final Options options = testCase.getOptions();

        assertNotNull(options);
        assertNotNull(options.loader());

        Object result = null;

        try {

            result = method.invoke(options);

            if (result == null || testCase.expectErrorCode != null) {
                return false;
            }

            if ((result instanceof RdfQuadSet) && testCase.expect == null && testCase.type.contains(Type.POSITIVE_SYNTAX_TEST)) {
                return true;

            } else if (testCase.expect == null) {
                return false;
            }

            if (result instanceof RdfQuadSet quads) {
                return JunitRunner.validateQuads(testCase, options, quads, false);
            }

            if (result instanceof JsonStructure json) {
                return JunitRunner.validateJsonLd(
                        testCase, 
                        options, 
                        json, 
                        JakartaAdapter.instance(),
                        false);
            }

            return JunitRunner.validateJsonLd(
                    testCase,
                    options,
                    result,
                    JavaAdapter.instance(),
                    false);


        } catch (IOException | NQuadsReaderException e) {
            throw new IllegalStateException(e);

        } catch (JsonLdException e) {
            return Objects.equals(e.code(), testCase.expectErrorCode);

        } catch (RdfConsumerException e) {
            if (e.getCause() instanceof JsonLdException) {
                return Objects.equals(((JsonLdException) e.getCause()).code(), testCase.expectErrorCode);
            }
        }
        return false;
    }
}
