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

import static org.junit.jupiter.api.Assertions.fail;

import java.util.Objects;
import java.util.concurrent.ExecutionException;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonLdComparison;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.test.JsonLdTestCase.Type;
import com.apicatalog.rdf.RdfComparison;

public class JsonLdTestRunnerEarl {

    private final JsonLdTestCase testCase;

    public JsonLdTestRunnerEarl(JsonLdTestCase testCase) {
        this.testCase = testCase;
    }

    public boolean execute(JsonLdTestCaseMethod method) {

        JsonLdOptions options = testCase.getOptions();

        Document result = null;

        try {

            result = method.invoke(options);

            if (result == null || testCase.expectErrorCode != null) {
                return false;
            }

            if (result.getRdfContent().isPresent() && testCase.expect == null && testCase.type.contains(Type.POSITIVE_SYNTAX_TEST)) {
                return true;

            } else if (testCase.expect == null) {
                return false;
            }

            Document expectedDocument = options.getDocumentLoader().loadDocument(testCase.expect, new DocumentLoaderOptions()).get();

            if (expectedDocument == null) {
                return false;
            }


            // compare expected with the result
            if (expectedDocument.getJsonContent().isPresent()) {

                return result.getJsonContent().isPresent()
                       && JsonLdComparison.equals(
                                   expectedDocument.getJsonContent().get(),
                                   result.getJsonContent().get()
                                   );

            } else if (expectedDocument.getRdfContent().isPresent()) {

                return result.getRdfContent().isPresent()
                        && RdfComparison.equals(
                                    expectedDocument.getRdfContent().get(),
                                    result.getRdfContent().get()
                                    );


            }

            return false;

        } catch (JsonLdError e) {
            return Objects.equals(e.getCode(), testCase.expectErrorCode);

        } catch (ExecutionException | InterruptedException e) {
            fail(e);
            return false;
        }
    }
}
