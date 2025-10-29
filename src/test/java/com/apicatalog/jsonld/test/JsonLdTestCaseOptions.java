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
package com.apicatalog.jsonld.test;

import java.net.URI;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.tree.io.NodeAdapter;

public class JsonLdTestCaseOptions {

    public JsonLdVersion version;
    public String base;
    public String processingMode;
    public Boolean normative;
    public String expandContext;
    public Boolean compactArrays;
    public Boolean compactToRelative;
    public String rdfDirection;
    public Boolean produceGeneralizedRdf;
    public Boolean useNativeTypes;
    public Boolean useRdfType;
    public Boolean omitGraph;
    public Boolean numericId;
    public Boolean rdfStar;
    public MediaType contentType;    
    public JsonLdOptions.ProcessingPolicy undefinedTerms;
    
    public URI redirectTo;

    public Integer httpStatus;

    public Set<String> httpLink;

    public JsonLdOptions.ProcessingPolicy undefinedTermPolicy = JsonLdOptions.ProcessingPolicy.Ignore;

    public static final JsonLdTestCaseOptions of(String baseUri) {
        return new JsonLdTestCaseOptions();
        
    }
    public static final JsonLdTestCaseOptions of(Object node, NodeAdapter adapter, String baseUri1) {

        final JsonLdTestCaseOptions options = new JsonLdTestCaseOptions();
System.out.println(node);
        var version = adapter.property("specVersion", node);
        
        if (adapter.isString(version)) {
            options.version = JsonLdVersion.of(adapter.stringValue(version));
        }
        System.out.println(options.version);
//        options.base = node.getString("base", null);
//        options.processingMode = node.getString("processingMode", null);
//
//        if (node.containsKey("normative")) {
//            options.normative = node.getBoolean("normative");
//        }
//
//        if (node.containsKey("expandContext")) {
//            options.expandContext = UriResolver.resolve(URI.create(baseUri), node.getString("expandContext"));
//        }
//
//        if (node.containsKey("compactArrays")) {
//            options.compactArrays = node.getBoolean("compactArrays");
//        }
//
//        if (node.containsKey("compactToRelative")) {
//            options.compactToRelative = node.getBoolean("compactToRelative");
//        }
//
//        options.rdfDirection = node.getString("rdfDirection", null);
//
//        if (node.containsKey("produceGeneralizedRdf")) {
//            options.produceGeneralizedRdf = node.getBoolean("produceGeneralizedRdf");
//        }
//
//        if (node.containsKey("useNativeTypes")) {
//            options.useNativeTypes = node.getBoolean("useNativeTypes");
//        }
//
//        if (node.containsKey("useRdfType")) {
//            options.useRdfType = node.getBoolean("useRdfType");
//        }
//
//        if (node.containsKey("omitGraph")) {
//            options.omitGraph = node.getBoolean("omitGraph");
//        }
//
//        if (node.containsKey("useNumericId")) {
//            options.numericId = node.getBoolean("useNumericId");
//        }
//
//        if (node.containsKey("rdfstar")) {
//            options.rdfStar = node.getBoolean("rdfstar");
//        }
//        
//        if (node.containsKey("undefinedTermPolicy")) {
//            options.undefinedTerms = JsonLdOptions.ProcessingPolicy.valueOf(node.getString("undefinedTermPolicy"));            
//        }

        return options;
    }

    public void setup(JsonLdOptions options) {

        if (processingMode != null) {
            options.setProcessingMode(JsonLdVersion.of(processingMode));
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

        if (rdfDirection != null) {
            options.setRdfDirection(RdfDirection.valueOf(rdfDirection.toUpperCase().replace("-", "_")));
        }

        if (produceGeneralizedRdf != null) {
            options.setProduceGeneralizedRdf(produceGeneralizedRdf);
        }

        if (useNativeTypes != null) {
            options.setUseNativeTypes(useNativeTypes);
        }

        if (useRdfType != null) {
            options.setUseRdfType(useRdfType);
        }

        if (omitGraph != null) {
            options.setOmitGraph(omitGraph);
        }

        if (numericId != null) {
            options.setNumericId(numericId);
        }

        if (rdfStar != null) {
            options.setRdfStar(rdfStar);
        }
        
        if (undefinedTerms != null) {
            options.setUndefinedTermsPolicy(undefinedTerms);
        }
    }
}
