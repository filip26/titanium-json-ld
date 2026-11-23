/*
 * Copyright 2020-2026 the original author or authors.
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
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.apicatalog.jcs.Jcs;
import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.Options.ProcessingPolicy;
import com.apicatalog.jsonld.Options.RdfDirection;
import com.apicatalog.web.media.MediaType;
import com.apicatalog.web.uri.UriResolver;

public class TestOptions {

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
    public Boolean useJcs;
    public Boolean useInlineContexts;
    public Boolean omitGraph;
    public Boolean useNumericId;
    public Boolean rdfStar;
    public MediaType contentType;
    public URI redirectTo;
    public Integer httpStatus;
    public Set<String> httpLink;
    public Options.ProcessingPolicy undefinedTerms = Options.ProcessingPolicy.Ignore;
    public Options.ProcessingPolicy droppedNodes = Options.ProcessingPolicy.Ignore;

    public static final TestOptions newOptions() {
        return new TestOptions();
    }

    public static final TestOptions create(Map<String, ?> node, String baseUri) {

        final TestOptions options = new TestOptions();

        for (final var entry : node.entrySet()) {

            switch (entry.getKey()) {
            case "specVersion":
                options.version = Version.from((String) entry.getValue());
                break;

            case "base":
                options.base = (String) entry.getValue();
                break;

            case "processingMode":
                options.processingMode = (String) entry.getValue();
                break;

            case "expandContext":
                options.expandContext = UriResolver.resolve(
                        URI.create(baseUri),
                        (String) entry.getValue());
                break;

            case "normative":
                options.normative = Boolean.TRUE.equals(entry.getValue());
                break;

            case "compactArrays":
                options.compactArrays = Boolean.TRUE.equals(entry.getValue());
                break;

            case "compactToRelative":
                options.compactToRelative = Boolean.TRUE.equals(entry.getValue());
                break;

            case "contentType":
                options.contentType = MediaType.from((String) entry.getValue());
                break;

            case "redirectTo":
                options.redirectTo = URI.create(baseUri + (String) entry.getValue());
                break;

            case "httpStatus":
                options.httpStatus = ((Number) entry.getValue()).intValue();
                break;

            case "httpLink":
                if (entry.getValue() instanceof Collection<?> col) {
                    options.httpLink = col.stream()
                            .map(String.class::cast)
                            .collect(Collectors.toSet());
                } else {
                    options.httpLink = Set.of((String) entry.getValue());
                }
                break;

            case "ordered":
                options.ordered = Boolean.TRUE.equals(entry.getValue());
                break;

            case "omitGraph":
                options.omitGraph = Boolean.TRUE.equals(entry.getValue());
                break;

            case "produceGeneralizedRdf":
                options.produceGeneralizedRdf = Boolean.TRUE.equals(entry.getValue());
                break;

            case "rdfDirection":
                options.rdfDirection = (String)entry.getValue();
                break;

            case "useNativeTypes":
                options.useNativeTypes = Boolean.TRUE.equals(entry.getValue());
                break;

            case "useRdfType":
                options.useRdfType = Boolean.TRUE.equals(entry.getValue());
                break;

            case "useNumericId":
                options.useNumericId = Boolean.TRUE.equals(entry.getValue());
                break;

            case "undefinedTermPolicy":
                options.undefinedTerms = ProcessingPolicy.valueOf((String)entry.getValue());
                break;

            case "droppedNodePolicy":
                options.droppedNodes = ProcessingPolicy.valueOf((String)entry.getValue());
                break;

            case "useJCS":
                options.useJcs = Boolean.TRUE.equals(entry.getValue());
                break;

            case "rdfstar":
                options.rdfStar = Boolean.TRUE.equals(entry.getValue());
                break;

            case "useInlineContexts":
                options.useInlineContexts = Boolean.TRUE.equals(entry.getValue());

            case "processorFeature":
                // ignore feature declaration
                break;

            default:
                System.err.println("An unknown test option " + entry.getKey() + " = " + entry.getValue() + ".");
            }
        }

        return options;
    }

    public Options setup(Options options) {

        Optional.ofNullable(processingMode)
                .map(Version::from)
                .ifPresent(options::mode);

        Optional.ofNullable(base)
                .map(URI::create)
                .ifPresent(options::base);

        Optional.ofNullable(expandContext)
                .map(URI::create)
                .ifPresent(options::expandContext);

        Optional.ofNullable(compactArrays)
                .ifPresent(options::compactArrays);

        Optional.ofNullable(compactToRelative)
                .ifPresent(options::compactToRelative);

        Optional.ofNullable(rdfDirection)
                .map(String::toUpperCase)
                .map(s -> s.replace("-", "_"))
                .map(RdfDirection::valueOf)
                .ifPresent(options::rdfDirection);

        Optional.ofNullable(produceGeneralizedRdf)
                .ifPresent(options::generalizedRdf);

        Optional.ofNullable(useNativeTypes)
                .ifPresent(options::useNativeTypes);

        Optional.ofNullable(useRdfType)
                .ifPresent(options::useRdfType);

        if (useJcs != null) {
            options.rdfJsonCanonizer(Jcs::canonize);
        }

        Optional.ofNullable(omitGraph)
                .ifPresent(options::omitGraph);

        Optional.ofNullable(ordered)
                .ifPresent(options::ordered);

        Optional.ofNullable(useNumericId)
                .ifPresent(options::useNumericId);

        Optional.ofNullable(rdfStar)
                .ifPresent(options::rdfStar);

        Optional.ofNullable(undefinedTerms)
                .ifPresent(options::undefinedTerms);

        Optional.ofNullable(droppedNodes)
                .ifPresent(options::droppedNodes);

        Optional.ofNullable(useInlineContexts)
                .ifPresent(options::useInlineContexts);

        return options;
    }
}
