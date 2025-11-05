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
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.test.JsonLdTestCase.Type;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;

public class JsonLdTestRunnerEarl {

    private final JsonLdTestCase testCase;

    public JsonLdTestRunnerEarl(JsonLdTestCase testCase) {
        this.testCase = testCase;
    }

    public boolean execute(JsonLdTestMethod method) {

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

            Document expectedDocument = options.loader().loadDocument(testCase.expect, DocumentLoader.defaultOptions());

            if (expectedDocument == null) {
                return false;
            }

            // compare expected with the result
    //FIXMe
//            if (expectedDocument.getJsonContent().isPresent()) {
//
//                return (result instanceof JsonStructure)
//                        && JsonLdComparison.equals(
//                                (JsonValue) expectedDocument.getJsonContent().get(),
//                                JakartaAdapter.instance(),
//                                (JsonStructure) result,
//                                JakartaAdapter.instance());
//
//            } else if (expectedDocument instanceof QuadSetDocument) {
//                return (result instanceof RdfQuadSet)
//                        && RdfComparison.equals(
//                                ((QuadSetDocument) expectedDocument).contentX(),
//                                (RdfQuadSet) result);
//            }

        } catch (IOException e) {
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
