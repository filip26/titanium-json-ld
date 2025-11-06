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
import java.time.Duration;
import java.util.Map;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;
import org.junit.platform.suite.api.SuiteDisplayName;

import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.FileLoader;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.loader.SchemeRouter;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeRenderer;
import com.apicatalog.tree.io.jakarta.JakartaParser;
import com.apicatalog.tree.io.jakarta.JakartaRenderer;

import jakarta.json.Json;

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

//    public static final ObjectMapper om  = new ObjectMapper();
//    
//    static {
//    om.configure(DeserializationFeature.NUL SerializationFeature.NUL,false);
//    om.configure(SerializationFeature.INDENT_OUTPUT,true);
//    om.setSerializationInclusion(Include.ALWAYS);
//    }

    public static final TreeParser PARSER =
//            new Jackson2Parser(om);
            new JakartaParser(Json.createReaderFactory(Map.of()));

    public static final TreeRenderer RENDERER = new JakartaRenderer(Json.createGeneratorFactory(Map.of()));

    public static final HttpLoader HTTP_LOADER = HttpLoader
            .newLoader(
                    HttpClient
                            .newBuilder()
                            .followRedirects(Redirect.NEVER)
                            .connectTimeout(Duration.ofSeconds(5))
                            .build(),
                    PARSER)
            .timeout(Duration.ofSeconds(5));

    public static final ZipResourceLoader ZIP_RESOURCE_LOADER = new ZipResourceLoader(PARSER);

    public static final FileLoader FILE_LOADER = new FileLoader(PARSER);

    public static final ClasspathLoader CLASSPATH_LOADER = new ClasspathLoader(PARSER);

    public static final DocumentLoader RESOURCE_LOADER = SchemeRouter.newBuilder()
            .route("zip", ZIP_RESOURCE_LOADER)
            .route("classpath", CLASSPATH_LOADER)
            .build();

}
