/*
 * Copyright 2025 the original author or authors.
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

import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeRenderer;

public class SuiteEvironment {

    public static Boolean isRunning = false;

    public static TreeParser PARSER;
    public static DocumentLoader LOADER;

    static void start(TreeParser parser, TreeRenderer renderer) {
        SuiteEvironment.PARSER = parser;
        SuiteEvironment.LOADER = new ZipResourceLoader(parser);
        isRunning = true;
    }

    static void stop() {
        isRunning = false;
        SuiteEvironment.LOADER = null;
    }
}
