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
package com.apicatalog.rdf.io.nquad.writer;

import javax.json.JsonObject;

import com.apicatalog.jsonld.lang.Keywords;

public final class NQuadsWriterTestCase {

    private String id;
    private String name;
    private String input;
    private String expected;

    public static final NQuadsWriterTestCase of(final JsonObject json) {

        final NQuadsWriterTestCase testCase = new NQuadsWriterTestCase();

        testCase.id = json.getString(Keywords.ID);
        testCase.name = json.getString("name");
        testCase.input = json.getString("input");
        testCase.expected = json.getString("expected");
        
        return testCase;
    }
    
    public String getId() {
        return id;
    }
    
    public String getName() {
        return name;
    }
    
    public String getInput() {
        return input;
    }
    
    public String getExpected() {
        return expected;
    }    
}
