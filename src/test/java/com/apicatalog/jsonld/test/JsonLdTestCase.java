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
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.api.StringUtils;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.UriBaseRewriter;
import com.apicatalog.tree.io.NodeAdapter;

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

    public JsonLdTestCaseOptions options;

    public MediaType contentType;

    public URI redirectTo;

    public Integer httpStatus;

    public Set<String> httpLink;

    private final String testsBase;

    public JsonLdOptions.ProcessingPolicy undefinedTermPolicy = JsonLdOptions.ProcessingPolicy.Ignore;

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

        for (var entry : adapter.entries(node)) {

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

            case "frame":
                testCase.frame = adapter.isNull(entry.getValue())
                        ? null
                        : URI.create(baseUri + adapter.stringValue(entry.getValue()));
                break;

            case "option":
                testCase.options = JsonLdTestCaseOptions.of(entry.getValue(), adapter, baseUri);
                break;
                
            case "purpose":
                break;

            default:
                System.err.println("An unknown test case property = '" + key + "'.");

            }
        }

//        testCase.type = node.get(Keywords.TYPE).asJsonArray().stream()
//                .map(JsonString.class::cast)
//                .map(JsonString::getString)
//                .map(Type::of)
//                .collect(Collectors.toSet());

//        testCase.name = node.getString("name");

//        testCase.input = node.containsKey("input")
//                ? URI.create(baseUri + node.getString("input"))
//                : null;
//
//        testCase.context = node.containsKey("context")
//                ? URI.create(baseUri + node.getString("context"))
//                : null;
//
//        testCase.expect = node.containsKey("expect")
//                ? URI.create(baseUri + node.getString("expect"))
//                : null;
//
//        testCase.frame = node.containsKey("frame")
//                ? URI.create(baseUri + node.getString("frame"))
//                : null;
//
//        testCase.expectErrorCode = node.containsKey("expectErrorCode")
//                ? errorCode((node.getString("expectErrorCode")))
//                : null;
//
//        testCase.options = node.containsKey("option")
//                ? JsonLdTestCaseOptions.of(node.getJsonObject("option"), baseUri)
//                : new JsonLdTestCaseOptions();
//
//
//        testCase.contentType = node.containsKey("option") && node.getJsonObject("option").containsKey("contentType")
//                ? MediaType.of(node.getJsonObject("option").getString("contentType"))
//                : null;
//
//        if (testCase.contentType == null && testCase.input != null) {
//
//            if (testCase.input.toString().endsWith(".jsonld")) {
//                testCase.contentType = MediaType.JSON_LD;
//
//            } else if (testCase.input.toString().endsWith(".json")) {
//                testCase.contentType = MediaType.JSON;
//
//            } else if (testCase.input.toString().endsWith(".html")) {
//                testCase.contentType = MediaType.HTML;
//            }
//        }
//
//        testCase.redirectTo = node.containsKey("option") && node.getJsonObject("option").containsKey("redirectTo")
//                ? URI.create(baseUri + node.getJsonObject("option").getString("redirectTo"))
//                : null;
//
//        testCase.httpStatus = node.containsKey("option")
//                ? node.getJsonObject("option").getInt("httpStatus", 301)
//                : null;
//
//        if (node.containsKey("option") && node.getJsonObject("option").containsKey("httpLink")) {
//
//            JsonValue links = node.getJsonObject("option").get("httpLink");
//
//            if (JsonUtils.isArray(links)) {
//                testCase.httpLink = links.asJsonArray().stream()
//                        .map(JsonString.class::cast)
//                        .map(JsonString::getString)
//                        .collect(Collectors.toSet());
//            } else {
//                testCase.httpLink = new HashSet<>();
//                testCase.httpLink.add(((JsonString) links).getString());
//            }
//        }
//
//        testCase.undefinedTermPolicy = node.containsKey("option")
//                ? JsonLdOptions.ProcessingPolicy.valueOf(node.getJsonObject("option").getString("undefinedTermPolicy", JsonLdOptions.ProcessingPolicy.Fail.name()))
//                : JsonLdOptions.ProcessingPolicy.Ignore;

        return testCase;
    }

    public JsonLdOptions getOptions() {

        final DocumentLoader rewriter = new UriBaseRewriter(
                baseUri,
                testsBase,
                loader);

        JsonLdOptions jsonLdOptions = new JsonLdOptions(rewriter);
        jsonLdOptions.setOrdered(true);

        options.setup(jsonLdOptions);

        return jsonLdOptions;
    }

    public static final JsonLdErrorCode errorCode(String errorCode) {

        if (errorCode == null || StringUtils.isBlank(errorCode)) {
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

        return JsonLdErrorCode.valueOf(StringUtils.strip(errorCode).toUpperCase().replace(" ", "_").replace("-", "_").replaceAll("\\_\\@", "_KEYWORD_"));
    }

    public enum Type {

        EXPAND_TEST,
        COMPACT_TEST,
        FLATTEN_TEST,
        TO_RDF_TEST,
        FROM_RDF_TEST,
        FRAME_TEST,

        POSITIVE_EVALUATION_TEST,
        NEGATIVE_EVALUATION_TEST,
        POSITIVE_SYNTAX_TEST;

        static Type of(String value) {

            if (value == null) {
                throw new IllegalArgumentException("Test @type cannot be null.");
            }

            switch (value) {
            case "jld:ExpandTest":
                return EXPAND_TEST;
            case "jld:CompactTest":
                return COMPACT_TEST;
            case "jld:FlattenTest":
                return FLATTEN_TEST;
            case "jld:ToRDFTest":
                return TO_RDF_TEST;
            case "jld:FromRDFTest":
                return FROM_RDF_TEST;
            case "jld:FrameTest":
                return FRAME_TEST;

            case "jld:PositiveEvaluationTest":
                return POSITIVE_EVALUATION_TEST;
            case "jld:NegativeEvaluationTest":
                return NEGATIVE_EVALUATION_TEST;

            case "jld:PositiveSyntaxTest":
                return POSITIVE_SYNTAX_TEST;
            }

            throw new IllegalArgumentException("Unknown test @type '" + value + "'");
        }
    }

    @Override
    public String toString() {
        return id + ": " + name;
    }
}
