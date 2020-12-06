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

import static org.junit.Assume.assumeFalse;

import java.io.IOException;
import java.util.Collection;
import java.util.Locale;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.test.JsonLdManifestLoader;
import com.apicatalog.jsonld.test.JsonLdTestCase;
import com.apicatalog.jsonld.test.JsonLdTestRunnerJunit;
import com.apicatalog.jsonld.test.loader.ZipResourceLoader;

@RunWith(Parameterized.class)
public class ToRdfTest {
    
    @Parameterized.Parameter(0)
    public JsonLdTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;
        
    @Parameterized.Parameter(3)
    public String baseUri;
    
    @Test
    public void testToRdf() throws IOException {
        // Force a locale to something different than US to be aware of DecimalFormat errors
        Locale.setDefault(Locale.FRANCE);

        // skip specVersion == 1.0
        assumeFalse(Version.V1_0.equals(testCase.options.specVersion));

        // blank nodes as predicates are not supported - wont'fix
        assumeFalse("#te075".equals(testCase.id));
        // invalid IRI/URI are not accepted - wont'fix
        assumeFalse("#tli12".equals(testCase.id));

        Assert.assertTrue(new JsonLdTestRunnerJunit(testCase).execute());
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws JsonLdError {
        return JsonLdManifestLoader
                    .load(JsonLdManifestLoader.JSON_LD_API_BASE, "toRdf-manifest.jsonld", new ZipResourceLoader())
                    .stream()            
                    .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                    .collect(Collectors.toList());
    }
}
