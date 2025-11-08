package com.apicatalog.jsonld;

import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeRenderer;

public class SuiteEvironment {

    public static Boolean suiteRunning = false;

    public static ZipResourceLoader LOADER;

    static void start(TreeParser parser, TreeRenderer renderer) {
        SuiteEvironment.LOADER = new ZipResourceLoader(parser);
        suiteRunning = true;
    }
    
    static void stop() {
        suiteRunning = false;
    }
}
