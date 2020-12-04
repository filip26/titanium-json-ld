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
package com.apicatalog.jsonld.issue;

import static org.junit.Assume.assumeTrue;

import java.util.Collection;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdTestCase;
import com.apicatalog.jsonld.suite.JsonLdTestRunnerJunit;

@RunWith(Parameterized.class)
public class CompactBaseTest {

    @Parameterized.Parameter(0)
    public JsonLdTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;
        
    @Parameterized.Parameter(3)
    public String baseUri;
    
    @Test
    public void testCompactBase() {

        // skip other tests
        assumeTrue(testId.equals("#t0047"));
        Assert.assertTrue(new JsonLdTestRunnerJunit(testCase).execute(options ->

                JsonDocument.of(
                        JsonLd.compact(testCase.input, testCase.context).options(options).base("http://fake.com").get()
                )

        ));
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws JsonLdError {        
        return JsonLdManifestLoader
                .load(JsonLdManifestLoader.JSON_LD_API_BASE, "compact-manifest.jsonld")
                .stream()            
                .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                .collect(Collectors.toList());
    }
}
