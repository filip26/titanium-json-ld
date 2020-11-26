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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;

import jakarta.json.JsonArray;
import jakarta.json.JsonValue;

public class NativeTypesIssue64Test {

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/64">Issue #64</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    public void testFromRdfNativeTypes() throws JsonLdError, IOException {

        final JsonArray result;
        
        try (final InputStream is = getClass().getResourceAsStream("issue64-in.nq")) {
            assertNotNull(is);
            
            result = JsonLd.fromRdf(RdfDocument.of(is)).nativeTypes().get();

            assertNotNull(result);
        }
        
        final JsonValue expected;
        
        try (final InputStream is = getClass().getResourceAsStream("issue64-out.json")) {
            assertNotNull(is);

            expected = JsonDocument.of(is).getJsonContent().orElse(null);
            
            assertNotNull(expected);
        }

        assertEquals(expected, result);
    }
    
}
