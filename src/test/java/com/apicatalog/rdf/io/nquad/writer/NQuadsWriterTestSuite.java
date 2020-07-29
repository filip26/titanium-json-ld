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

import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.zip.ZipException;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.stream.JsonParser;

import org.junit.Assert;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.rdf.io.nquad.NQuadsWriterTest;

public final class NQuadsWriterTestSuite {

    public static final Collection<NQuadsWriterTestCase> load() throws ZipException, IOException, URISyntaxException {
        
        try (final InputStream is = (new NQuadsWriterTest()).getClass().getResourceAsStream("manifest.json")) {

            Assert.assertNotNull(is);

            final JsonParser parser = Json.createParser(is);
            
            parser.next();
            
            return parser
                        .getArray()
                        .stream()
                        .filter(JsonUtils::isObject)
                        .map(JsonObject.class::cast)
                        .map(NQuadsWriterTestCase::of).collect(Collectors.toList());
        }
    }
}
