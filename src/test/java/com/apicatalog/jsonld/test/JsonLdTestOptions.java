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
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.Options.ProcessingPolicy;
import com.apicatalog.jsonld.Options.RdfDirection;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.web.media.MediaType;
import com.apicatalog.web.uri.UriResolver;

public class JsonLdTestOptions {

    public Version version;
    public String base;
    public String processingMode;
    public Boolean normative;
    public String expandContext;
    public Boolean compactArrays;
    public Boolean compactToRelative;
    public Boolean ordered;
    public String rdfDirection;
    public Boolean produceGeneralizedRdf;
    public Boolean useNativeTypes;
    public Boolean useRdfType;
    public Boolean omitGraph;
    public Boolean useNumericId;
    public Boolean rdfStar;
    public MediaType contentType;
    public Options.ProcessingPolicy undefinedTerms;
    public URI redirectTo;
    public Integer httpStatus;
    public Set<String> httpLink;
    public Options.ProcessingPolicy undefinedTermPolicy = Options.ProcessingPolicy.Ignore;

    public static final JsonLdTestOptions newOptions() {
        return new JsonLdTestOptions();
    }

    public static final JsonLdTestOptions of(Object node, NodeAdapter adapter, String baseUri) {

        final JsonLdTestOptions options = new JsonLdTestOptions();

        for (final var entry : adapter.entries(node)) {

            final var key = adapter.stringValue(entry.getKey());

            switch (key) {
            case "specVersion":
                options.version = Version.of(adapter.stringValue(entry.getValue()));
                break;

            case "base":
                options.base = adapter.stringValue(entry.getValue());
                break;

            case "processingMode":
                options.processingMode = adapter.stringValue(entry.getValue());
                break;

            case "expandContext":
                options.expandContext = UriResolver.resolve(
                        URI.create(baseUri),
                        adapter.stringValue(entry.getValue()));
                break;

            case "normative":
                options.normative = adapter.isTrue(entry.getValue());
                break;

            case "compactArrays":
                options.compactArrays = adapter.isTrue(entry.getValue());
                break;

            case "compactToRelative":
                options.compactToRelative = adapter.isTrue(entry.getValue());
                break;

            case "contentType":
                options.contentType = MediaType.of(adapter.stringValue(entry.getValue()));
                break;

            case "redirectTo":
                options.redirectTo = URI.create(baseUri + adapter.stringValue(entry.getValue()));
                break;

            case "httpStatus":
                options.httpStatus = adapter.intValue(entry.getValue());
                break;

            case "httpLink":
                if (adapter.isCollection(entry.getValue())) {
                    options.httpLink = adapter.elementStream(entry.getValue())
                            .map(adapter::asString)
                            .collect(Collectors.toSet());
                } else {
                    options.httpLink = Set.of(adapter.stringValue(entry.getValue()));
                }
                break;

            case "ordered":
                options.ordered = adapter.isTrue(entry.getValue());
                break;

            case "omitGraph":
                options.omitGraph = adapter.isTrue(entry.getValue());
                break;

            case "produceGeneralizedRdf":
                options.produceGeneralizedRdf = adapter.isTrue(entry.getValue());
                break;

            case "rdfDirection":
                options.rdfDirection = adapter.stringValue(entry.getValue());
                break;

            case "useNativeTypes":
                options.useNativeTypes = adapter.isTrue(entry.getValue());
                break;

            case "useRdfType":
                options.useRdfType = adapter.isTrue(entry.getValue());
                break;

            case "useNumericId":
                options.useNumericId = adapter.isTrue(entry.getValue());
                break;
                
            case "undefinedTermPolicy":
                options.undefinedTermPolicy = ProcessingPolicy.valueOf(adapter.stringValue(entry.getValue()));
                System.out.println("## " + options.undefinedTermPolicy);
                break;
                
            default:
                System.err.println("An unknown test option " + key + " = " + entry.getValue() + ".");
            }
        }

        return options;
    }

    public Options setup(Options options) {

        if (processingMode != null) {
            options.mode(Version.of(processingMode));
        }

        if (base != null) {
            options.base(URI.create(base));
        }

        if (expandContext != null) {
            options.expandContext(URI.create(expandContext));
        }

        if (compactArrays != null) {
            options.compactArrays(compactArrays);
        }

        if (compactToRelative != null) {
            options.compactToRelative(compactToRelative);
        }

        if (rdfDirection != null) {
            options.rdfDirection(RdfDirection.valueOf(rdfDirection.toUpperCase().replace("-", "_")));
        }

        if (produceGeneralizedRdf != null) {
            options.generalizedRdf(produceGeneralizedRdf);
        }

        if (useNativeTypes != null) {
            options.useNativeTypes(useNativeTypes);
        }

        if (useRdfType != null) {
            options.useRdfType(useRdfType);
        }

        if (omitGraph != null) {
            options.omitGraph(omitGraph);
        }

        if (ordered != null) {
            options.ordered(ordered);
        }

        if (useNumericId != null) {
            options.useNumericId(useNumericId);
        }

        if (rdfStar != null) {
            options.rdfStar(rdfStar);
        }

        if (undefinedTerms != null) {
            options.undefinedTermsPolicy(undefinedTerms);
        }
        return options;
    }
}
