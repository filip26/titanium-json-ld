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
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.JsonLdVersion;
import no.hasmac.jsonld.StringUtils;
import no.hasmac.jsonld.http.media.MediaType;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.loader.DocumentLoader;
import no.hasmac.jsonld.loader.UriBaseRewriter;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public final class JsonLdTestCase {

    public static final String TESTS_BASE = "https://w3c.github.io";

    public static final Predicate<JsonLdTestCase> IS_NOT_V1_0 = test -> !JsonLdVersion.V1_0.equals(test.options.specVersion);

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

    private final DocumentLoader loader;

    public JsonLdTestCase(final String testsBase, final DocumentLoader loader) {
        this.testsBase = testsBase;
        this.loader = loader;
    }

    public static JsonLdTestCase of(JsonObject o, String manifestUri, String manifestBase, String baseUri, final DocumentLoader loader) {

        final JsonLdTestCase testCase = new JsonLdTestCase(manifestBase, loader);

        testCase.id = o.getString(Keywords.ID);

        testCase.uri = baseUri + manifestUri.substring(0, manifestUri.length() - ".jsonld".length()) + testCase.id;

        testCase.type = o.get(Keywords.TYPE).asJsonArray().stream()
                            .map(JsonString.class::cast)
                            .map(JsonString::getString)
                            .map(Type::of)
                            .collect(Collectors.toSet());

        testCase.name = o.getString("name");

        testCase.input = o.containsKey("input")
                            ? URI.create(baseUri + o.getString("input"))
                            : null;

        testCase.context = o.containsKey("context")
                                ? URI.create(baseUri + o.getString("context"))
                                : null;

        testCase.expect = o.containsKey("expect")
                                ? URI.create(baseUri + o.getString("expect"))
                                : null;

        testCase.frame = o.containsKey("frame")
                                ? URI.create(baseUri + o.getString("frame"))
                                : null;

        testCase.expectErrorCode = o.containsKey("expectErrorCode")
                                            ? errorCode((o.getString("expectErrorCode")))
                                            : null;

        testCase.options = o.containsKey("option")
                                ? JsonLdTestCaseOptions.of(o.getJsonObject("option"), baseUri)
                                : new JsonLdTestCaseOptions();

        testCase.baseUri = baseUri;


        testCase.contentType = o.containsKey("option") && o.getJsonObject("option").containsKey("contentType")
                                    ? MediaType.of(o.getJsonObject("option").getString("contentType"))
                                    : null;

        if (testCase.contentType == null && testCase.input != null) {

            if (testCase.input.toString().endsWith(".jsonld")) {
                testCase.contentType = MediaType.JSON_LD;

            } else if (testCase.input.toString().endsWith(".json")) {
                testCase.contentType = MediaType.JSON;

            } else if (testCase.input.toString().endsWith(".html")) {
                testCase.contentType = MediaType.HTML;
            }
        }

        testCase.redirectTo = o.containsKey("option") && o.getJsonObject("option").containsKey("redirectTo")
                                ? URI.create(baseUri + o.getJsonObject("option").getString("redirectTo"))
                                : null;

        testCase.httpStatus = o.containsKey("option")
                                    ? o.getJsonObject("option").getInt("httpStatus", 301)
                                    : null
                                    ;

        if (o.containsKey("option") &&  o.getJsonObject("option").containsKey("httpLink")) {

            JsonValue links = o.getJsonObject("option").get("httpLink");

            if (JsonUtils.isArray(links)) {
                testCase.httpLink = links.asJsonArray().stream()
                                            .map(JsonString.class::cast)
                                            .map(JsonString::getString)
                                            .collect(Collectors.toSet());
            } else {
                testCase.httpLink = new HashSet<>();
                testCase.httpLink.add(((JsonString)links).getString());
            }
        }

        return testCase;
    }

    public JsonLdOptions getOptions() {

        final DocumentLoader rewriter =
                new UriBaseRewriter(
                            baseUri,
                            testsBase,
                            loader
                        );

        JsonLdOptions jsonLdOptions = new JsonLdOptions(rewriter);
        jsonLdOptions.setOrdered(true);

        options.setup(jsonLdOptions);

        return jsonLdOptions;
    }

    public static JsonLdErrorCode errorCode(String errorCode) {

        if (errorCode == null ||  StringUtils.isBlank(errorCode)) {
            return null;
        }

        /*
         * Because scoped contexts can lead to contexts being reloaded,
         * replace the recursive context inclusion error with a context overflow error.
         *
         * @see <a href="https://www.w3.org/TR/json-ld11-api/#changes-from-cg">Changes since JSON-LD Community Group Final Report</a>
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

        return JsonLdErrorCode.valueOf(StringUtils.strip(errorCode).toUpperCase().replace(" ", "_").replace("-", "_").replaceAll("\\_\\@", "_KEYWORD_" ));
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
        POSITIVE_SYNTAX_TEST
        ;

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
