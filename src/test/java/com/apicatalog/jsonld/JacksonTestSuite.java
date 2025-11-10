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

import org.junit.platform.suite.api.AfterSuite;
import org.junit.platform.suite.api.BeforeSuite;
import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;
import org.junit.platform.suite.api.SuiteDisplayName;

import com.apicatalog.jsonld.std.CompactorTest;
import com.apicatalog.jsonld.std.ExpanderTest;
import com.apicatalog.jsonld.std.FlattenTest;
import com.apicatalog.jsonld.std.FramerTest;
import com.apicatalog.jsonld.std.FromRdfTest;
import com.apicatalog.jsonld.std.RemoteTest;
import com.apicatalog.jsonld.std.ToRdfTest;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeRenderer;
import com.apicatalog.tree.io.jakcson.Jackson2Parser;
import com.apicatalog.tree.io.jakcson.Jackson2Renderer;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;

@Suite(failIfNoTests = true)
@SuiteDisplayName("JSON-LD Suite (Jackson)")
@SelectClasses({
        ExpanderTest.class,
        FlattenTest.class,
        CompactorTest.class,
        FramerTest.class,
        FromRdfTest.class,
        ToRdfTest.class,
        RemoteTest.class
})
public class JacksonTestSuite extends SuiteEvironment {

    public static final ObjectMapper OM = new ObjectMapper();

    public static final TreeParser PARSER = new Jackson2Parser(OM);

    public static final TreeRenderer RENDERER = new Jackson2Renderer(new JsonFactory());

    @BeforeSuite
    public static void beforeSuite() {
        start(PARSER, RENDERER);
    }

    @AfterSuite
    public static void afterSuite() {
        stop();
    }
}
