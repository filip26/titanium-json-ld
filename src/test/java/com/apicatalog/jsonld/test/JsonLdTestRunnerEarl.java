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

import java.util.Objects;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonLdComparison;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.loader.QuadSetDocument;
import com.apicatalog.jsonld.test.JsonLdTestCase.Type;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;

public class JsonLdTestRunnerEarl {

    private final JsonLdTestCase testCase;

    public JsonLdTestRunnerEarl(JsonLdTestCase testCase) {
        this.testCase = testCase;
    }

    public boolean execute(JsonLdTestCaseMethod method) {

        JsonLdOptions options = testCase.getOptions();

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

            Document expectedDocument = options.getDocumentLoader().loadDocument(testCase.expect, new DocumentLoaderOptions());

            if (expectedDocument == null) {
                return false;
            }

            // compare expected with the result
            if (expectedDocument.getJsonContent().isPresent()) {

                return (result instanceof Document)
                        && ((Document) result).getJsonContent().isPresent()
                        && JsonLdComparison.equals(
                                expectedDocument.getJsonContent().get(),
                                ((Document) result).getJsonContent().get());

            } else if (expectedDocument instanceof QuadSetDocument) {

                return (result instanceof RdfQuadSet)
                        && RdfComparison.equals(
                                ((QuadSetDocument) expectedDocument).getContent(),
                                (RdfQuadSet) result);
            }

        } catch (JsonLdError e) {
            return Objects.equals(e.getCode(), testCase.expectErrorCode);

        } catch (RdfConsumerException e) {
            if (e.getCause() instanceof JsonLdError) {
                return Objects.equals(((JsonLdError) e.getCause()).getCode(), testCase.expectErrorCode);
            }
        }
        return false;
    }
}
