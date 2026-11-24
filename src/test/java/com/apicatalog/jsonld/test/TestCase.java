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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.UriRewriter;
import com.apicatalog.rdf.model.RdfQuad;
import com.apicatalog.rdf.nquads.NQuadsReader;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.provider.QuadSetProvider;
import com.apicatalog.web.media.MediaType;

public final class TestCase {

    public static final Predicate<TestCase> IS_NOT_V1_0 = test -> test.options != null
            && !Version.V1_0.equals(test.options.version);

    public String id;

    public String name;

    public URI input;

    public URI context;

    public URI expect;

    public URI frame;

    public ErrorCode expectErrorCode;

    public String baseUri;

    public String uri;

    public Set<Type> type;

    public TestOptions options;

    public String testsBase;

    private final DocumentLoader loader;

    private TestCase(String testsBase, DocumentLoader loader) {
        this.testsBase = testsBase;
        this.loader = loader;
    }

    @SuppressWarnings("unchecked")
    public static TestCase create(
            final Map<?, ?> node,
            final String manifestUri,
            final String manifestBase,
            final String baseUri,
            final DocumentLoader loader) {

        final var testCase = new TestCase(manifestBase, loader);
        testCase.baseUri = baseUri;

        for (final var entry : node.entrySet()) {

            if (entry.getValue() == null) {
                continue;
            }

            switch ((String) entry.getKey()) {
            case Keywords.ID:
                testCase.id = (String) entry.getValue();
                testCase.uri = baseUri + manifestUri.substring(0, manifestUri.length() - ".jsonld".length())
                        + testCase.id;
                break;

            case Keywords.TYPE:
                testCase.type = ((Collection<?>) entry.getValue())
                        .stream()
                        .map(String.class::cast)
                        .map(Type::from)
                        .collect(Collectors.toSet());
                break;

            case "name":
                testCase.name = (String) entry.getValue();
                break;

            case "input":
                testCase.input = URI.create(baseUri + (String) entry.getValue());
                break;

            case "context":
                testCase.context = URI.create(baseUri + (String) entry.getValue());
                break;

            case "expect":
                testCase.expect = URI.create(baseUri + (String) entry.getValue());
                break;

            case "expectErrorCode":
                testCase.expectErrorCode = errorCode((String) entry.getValue());
                break;

            case "frame":
                testCase.frame = URI.create(baseUri + (String) (entry.getValue()));
                break;

            case "option":
                testCase.options = TestOptions.create((Map<String, ?>) entry.getValue(), baseUri);
                break;

            case "purpose":
                // ignore textual justification
                break;

            case "requires":
                // ignore feature declaration
                break;

            default:
                throw new IllegalArgumentException(
                        "An unknown test case property " + entry.getKey() + " = " + entry.getValue() + ".");
            }
        }

        if (testCase.options == null) {
            testCase.options = new TestOptions();
        }

        if (testCase.options.contentType == null && testCase.input != null) {

            if (testCase.input.toString().endsWith(".jsonld")) {
                testCase.options.contentType = MediaType.JSON_LD;

            } else if (testCase.input.toString().endsWith(".json")) {
                testCase.options.contentType = MediaType.JSON;

            } else if (testCase.input.toString().endsWith(".html")) {
                testCase.options.contentType = MediaType.HTML;
            }
        }

        return testCase;
    }

    public Options options() {
        return options.setup(
                Options.with(
                        UriRewriter.newBuilder(loader)
                                .rebase(baseUri, testsBase)
                                .build())
                        .ordered(true));
    }

    public static ErrorCode errorCode(String errorCode) {

        if (errorCode == null || errorCode.isBlank()) {
            return null;
        }

        /*
         * Because scoped contexts can lead to contexts being reloaded, replace the
         * recursive context inclusion error with a context overflow error.
         *
         * @see <a href="https://www.w3.org/TR/json-ld11-api/#changes-from-cg">Changes
         * since JSON-LD Community Group Final Report</a>
         */
        if ("recursive context inclusion".equalsIgnoreCase(errorCode)) {
            return ErrorCode.CONTEXT_OVERFLOW;
        }
        if ("list of lists".equalsIgnoreCase(errorCode)) {
            return ErrorCode.UNSPECIFIED;
        }
        if ("compaction to list of lists".equalsIgnoreCase(errorCode)) {
            return ErrorCode.UNSPECIFIED;
        }

        return ErrorCode.valueOf(
                errorCode.strip().toUpperCase().replace(" ", "_").replace("-", "_").replaceAll("\\_\\@", "_KEYWORD_"));
    }

    public enum Type {

        EXPAND_TEST("jld:ExpandTest"),
        COMPACT_TEST("jld:CompactTest"),
        FLATTEN_TEST("jld:FlattenTest"),
        TO_RDF_TEST("jld:ToRDFTest"),
        FROM_RDF_TEST("jld:FromRDFTest"),
        FRAME_TEST("jld:FrameTest"),

        POSITIVE_EVALUATION_TEST("jld:PositiveEvaluationTest"),
        NEGATIVE_EVALUATION_TEST("jld:NegativeEvaluationTest"),
        POSITIVE_SYNTAX_TEST("jld:PositiveSyntaxTest");

        private String curi;

        Type(String curi) {
            this.curi = curi;
        }

        static Type from(final String value) {

            Objects.requireNonNull(value);

            for (final var type : values()) {
                if (type.curi.equals(value)) {
                    return type;
                }
            }

            throw new IllegalArgumentException("Unknown test @type '" + value + "'");
        }
    }

    @Override
    public String toString() {
        return id + ": " + name;
    }

    public Set<RdfQuad> readQuads(byte[] bytes) throws NQuadsReaderException, IOException {
        return readQuads(new ByteArrayInputStream(bytes));
    }

    public Set<RdfQuad> readQuads(InputStream is) throws NQuadsReaderException, IOException {

        var content = new LinkedHashSet<RdfQuad>();

        try (var reader = new NQuadsReader(new InputStreamReader(is))) {
            reader.read(new QuadSetProvider(content));
        }

        return content;
    }

    public URI rebase(URI url) {

        final var target = url.toString();

        final var relativePath = target.substring(baseUri.length());

        return URI.create(testsBase + relativePath);
    }
}
