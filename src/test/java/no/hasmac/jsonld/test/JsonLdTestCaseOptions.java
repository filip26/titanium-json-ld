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
package no.hasmac.jsonld.test;

import java.net.URI;

import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.JsonLdOptions.RdfDirection;
import no.hasmac.jsonld.JsonLdVersion;
import no.hasmac.jsonld.uri.UriResolver;

import jakarta.json.JsonObject;

public class JsonLdTestCaseOptions {

    public JsonLdVersion specVersion;
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

    public static JsonLdTestCaseOptions of(JsonObject o, String baseUri) {

        final JsonLdTestCaseOptions options = new JsonLdTestCaseOptions();

        if (o.containsKey("specVersion")) {
            options.specVersion = JsonLdVersion.of(o.getString("specVersion"));
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

        options.rdfDirection = o.getString("rdfDirection", null);

        if (o.containsKey("produceGeneralizedRdf")) {
            options.produceGeneralizedRdf = o.getBoolean("produceGeneralizedRdf");
        }

        if (o.containsKey("useNativeTypes")) {
            options.useNativeTypes = o.getBoolean("useNativeTypes");
        }

        if (o.containsKey("useRdfType")) {
            options.useRdfType = o.getBoolean("useRdfType");
        }

        if (o.containsKey("omitGraph")) {
            options.omitGraph = o.getBoolean("omitGraph");
        }

        if (o.containsKey("useNumericId")) {
            options.numericId = o.getBoolean("useNumericId");
        }

        if (o.containsKey("rdfstar")) {
            options.rdfStar = o.getBoolean("rdfstar");
        }

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
    }
}
