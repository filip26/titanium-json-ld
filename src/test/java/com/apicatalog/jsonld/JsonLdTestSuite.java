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

import java.net.http.HttpClient;
import java.net.http.HttpClient.Redirect;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;
import org.junit.platform.suite.api.SuiteDisplayName;

import com.apicatalog.jsonld.loader.DefaultHttpClient;
import com.apicatalog.jsonld.loader.FileLoader;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.tree.io.NodeParser;
import com.apicatalog.tree.io.jakarta.JakartaParser;
import com.apicatalog.tree.io.jakcson.Jackson2Parser;
import com.fasterxml.jackson.databind.ObjectMapper;

@Suite(failIfNoTests = true)
@SuiteDisplayName("JsonLd Suite")
@SelectClasses({
        ExpanderTest.class,
        FlattenTest.class,
        CompactorTest.class,
        FramerTest.class,
        FromRdfTest.class,
        ToRdfTest.class,
        RemoteTest.class
})
public class JsonLdTestSuite {

    public static final NodeParser JAKARTA_READER = new Jackson2Parser(new ObjectMapper());
//            new JakartaParser(Json.createReaderFactory(Map.of()));

    public static final HttpLoader HTTP_LOADER = new HttpLoader(
            new DefaultHttpClient(HttpClient
                    .newBuilder()
                    .followRedirects(Redirect.NEVER)
                    .build()),
            JAKARTA_READER);

    public static final ZipResourceLoader ZIP_RESOURCE_LOADER = new ZipResourceLoader(JAKARTA_READER);

    public static final FileLoader FILE_LOADER = new FileLoader(JAKARTA_READER);

//    public static final DocumentLoader LOADER = SchemeRouter.newBuilder()
//          .route("http", HTTP_LOADER)
////          .route("https", HttpLoader.defaultInstance())
////          .route("file", new FileLoader())
//          .build();

}
