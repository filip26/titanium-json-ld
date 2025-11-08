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

import org.junit.platform.suite.api.BeforeSuite;
import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;
import org.junit.platform.suite.api.SuiteDisplayName;

import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.suite.CompactorTest;
import com.apicatalog.jsonld.suite.ExpanderTest;
import com.apicatalog.jsonld.suite.FlattenTest;
import com.apicatalog.jsonld.suite.FramerTest;
import com.apicatalog.jsonld.suite.FromRdfTest;
import com.apicatalog.jsonld.suite.RemoteTest;
import com.apicatalog.jsonld.suite.ToRdfTest;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeRenderer;
import com.apicatalog.tree.io.jakarta.JakartaParser;
import com.apicatalog.tree.io.jakarta.JakartaRenderer;

import jakarta.json.Json;

@Suite(failIfNoTests = true)
@SuiteDisplayName("JSON-LD Suite (Jakarta)")
@SelectClasses({
        ExpanderTest.class,
        FlattenTest.class,
        CompactorTest.class,
        FramerTest.class,
        FromRdfTest.class,
        ToRdfTest.class,
        RemoteTest.class
})
public class JakartaTestSuite extends SuiteEvironment {

    public static final TreeParser PARSER = new JakartaParser(Json.createReaderFactory(Map.of()));

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

    @BeforeSuite
    public static void beforeSuite() {
        start(PARSER, RENDERER);
    }

    public static void afterSuite() {
        stop();
    }

}
