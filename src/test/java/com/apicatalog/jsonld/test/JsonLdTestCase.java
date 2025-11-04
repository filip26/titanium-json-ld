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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.UriBaseRewriter;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;
import com.apicatalog.rdf.nquads.NQuadsReader;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.web.media.MediaType;

public final class JsonLdTestCase {

    public static final String TESTS_BASE = "https://w3c.github.io";

    public static final Predicate<JsonLdTestCase> IS_NOT_V1_0 = test -> test.options != null && !JsonLdVersion.V1_0.equals(test.options.version);

    public String id;

    public String name;

    public URI input;

    public URI context;

    public URI expect;

    public URI frame;

    public JsonLdErrorCode expectErrorCode;

    public String baseUri;

    public String uri;

    public Set<Type> type;

    public JsonLdTestOptions options;

    public String testsBase;

    private final DocumentLoader loader;

    public JsonLdTestCase(final String testsBase, final DocumentLoader loader) {
        this.testsBase = testsBase;
        this.loader = loader;
    }

    public static final JsonLdTestCase of(
            final Object node,
            final NodeAdapter adapter,
            final String manifestUri,
            final String manifestBase,
            final String baseUri,
            final DocumentLoader loader) {

        final var testCase = new JsonLdTestCase(manifestBase, loader);
        testCase.baseUri = baseUri;

        for (final var entry : adapter.entries(node)) {

            final var key = adapter.stringValue(entry.getKey());

            switch (key) {
            case Keywords.ID:
                testCase.id = adapter.stringValue(entry.getValue());
                testCase.uri = baseUri + manifestUri.substring(0, manifestUri.length() - ".jsonld".length()) + testCase.id;
                break;

            case Keywords.TYPE:
                testCase.type = adapter.elementStream(entry.getValue())
                        .map(adapter::stringValue)
                        .map(Type::of)
                        .collect(Collectors.toSet());
                break;

            case "name":
                testCase.name = adapter.asString(entry.getValue());
                break;

            case "input":
                testCase.input = adapter.isNull(entry.getValue())
                        ? null
                        : URI.create(baseUri + adapter.stringValue(entry.getValue()));
                break;

            case "context":
                testCase.context = adapter.isNull(entry.getValue())
                        ? null
                        : URI.create(baseUri + adapter.stringValue(entry.getValue()));
                break;

            case "expect":
                testCase.expect = adapter.isNull(entry.getValue())
                        ? null
                        : URI.create(baseUri + adapter.stringValue(entry.getValue()));
                break;

            case "expectErrorCode":
                testCase.expectErrorCode = errorCode(adapter.stringValue(entry.getValue()));
                break;

            case "frame":
                testCase.frame = adapter.isNull(entry.getValue())
                        ? null
                        : URI.create(baseUri + adapter.stringValue(entry.getValue()));
                break;

            case "option":
                testCase.options = JsonLdTestOptions.of(entry.getValue(), adapter, baseUri);
                break;

            case "purpose":
                break;

            default:
                System.err.println("An unknown test case property " + key + " = " + entry.getValue() + ".");

            }
        }

        if (testCase.options == null) {
            testCase.options = JsonLdTestOptions.newOptions();
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

    public JsonLdOptions getOptions() {
        return options.setup(
                JsonLdOptions.with(
                        new UriBaseRewriter(
                                baseUri,
                                testsBase,
                                loader))
                        .ordered(true));
    }

    public static final JsonLdErrorCode errorCode(String errorCode) {

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
            return JsonLdErrorCode.CONTEXT_OVERFLOW;
        }
        if ("list of lists".equalsIgnoreCase(errorCode)) {
            return JsonLdErrorCode.UNSPECIFIED;
        }
        if ("compaction to list of lists".equalsIgnoreCase(errorCode)) {
            return JsonLdErrorCode.UNSPECIFIED;
        }

        return JsonLdErrorCode.valueOf(errorCode.strip().toUpperCase().replace(" ", "_").replace("-", "_").replaceAll("\\_\\@", "_KEYWORD_"));
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

        static Type of(final String value) {

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

    public RdfQuadSet readQuads(byte[] bytes) throws NQuadsReaderException, RdfConsumerException, JsonLdException {
        return readQuads(new ByteArrayInputStream(bytes));
    }

    public RdfQuadSet readQuads(InputStream is) throws NQuadsReaderException, RdfConsumerException, JsonLdException {

        var content = new OrderedQuadSet();

        new NQuadsReader(new InputStreamReader(is))
                .provide(new QuadAcceptor(content));

        return content;
    }

    public URI rebase(URI url) throws NQuadsReaderException, RdfConsumerException, JsonLdException {

        final var target = url.toString();

        final var relativePath = target.substring(baseUri.length());

        return URI.create(testsBase + relativePath);
    }

}
