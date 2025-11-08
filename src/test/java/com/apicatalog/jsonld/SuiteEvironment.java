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
