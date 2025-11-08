package com.apicatalog.jsonld;

import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeRenderer;

public class SuiteEvironment {

    public static Boolean suiteRunning = false;

    public static TreeParser parser;

    public static TreeRenderer renderer;

    public static ZipResourceLoader ZIP_LOADER;

    public static ClasspathLoader CLASSPATH_LOADER;

    static void start(TreeParser parser, TreeRenderer renderer) {
        SuiteEvironment.parser = parser;
        SuiteEvironment.ZIP_LOADER = new ZipResourceLoader(parser);
        SuiteEvironment.CLASSPATH_LOADER = new ClasspathLoader(parser);
        suiteRunning = true;
    }
    
    static void stop() {
        suiteRunning = false;
    }
}
