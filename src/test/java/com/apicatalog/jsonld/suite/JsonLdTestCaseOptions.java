package com.apicatalog.jsonld.suite;

import java.net.URI;

import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.uri.UriResolver;

public class JsonLdTestCaseOptions {

    public Version specVersion;
    public String base;
    public String processingMode;
    public Boolean normative;
    public String expandContext;
    public Boolean compactArrays;
    public Boolean compactToRelative;
    
    public static final JsonLdTestCaseOptions of(JsonObject o, String baseUri) {
        
        final JsonLdTestCaseOptions options = new JsonLdTestCaseOptions();
        
        if (o.containsKey("specVersion")) {
            options.specVersion = Version.of(o.getString("specVersion"));
        }
        
        options.base = o.getString("base", null);
        options.processingMode = o.getString("processingMode", null);

        if (o.containsKey("normative")) {
            options.normative = o.getBoolean("normative");
        }

        if (o.containsKey("expandContext")) {
            options.expandContext = UriResolver.resolve(URI.create(baseUri), o.getString("expandContext"));
        }

        if (o.containsKey("compactArrays")) {
            options.compactArrays = o.getBoolean("compactArrays");
        }

        if (o.containsKey("compactToRelative")) {
            options.compactToRelative = o.getBoolean("compactToRelative");
        }
        
        return options;
    }

    public void setup(JsonLdOptions options) {

        if (processingMode != null) {
            options.setProcessingMode(Version.of(processingMode));
        }
                
        if (base != null) {
            options.setBase(URI.create(base));
        }
        
        if (expandContext != null) {
            options.setExpandContext(URI.create(expandContext));
        }
        
        if (compactArrays != null) {
            options.setCompactArrays(compactArrays);
        }
        
        if (compactToRelative != null) {
            options.setCompactToRelative(compactToRelative);
        }
    }
}
