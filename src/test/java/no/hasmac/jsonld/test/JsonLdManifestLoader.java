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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.net.URI;
import java.util.stream.Stream;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.loader.DocumentLoader;
import no.hasmac.jsonld.loader.DocumentLoaderOptions;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;


public final class JsonLdManifestLoader {

    public static final String JSON_LD_API_BASE = "zip://json-ld-test-suite-20230601.zip/com/github/w3c/json-ld-api/tests/";
    public static final String JSON_LD_FRAMING_BASE = "zip://json-ld-framing-test-suite-20200609.zip/com/github/w3c/json-ld-framing/tests/";

    // Extension: JSON-LD-STAR (Experimental)
    public static final String JSON_LD_STAR_BASE = "zip://json-ld-star-test-suite-20210521.zip/com/github/json-ld/json-ld-star/tests/";

    private final String manifestName;
    private final String manifestBase;

    private final DocumentLoader loader;

    private JsonLdManifestLoader(final String manifestBase, final String manifestName, final DocumentLoader loader) {
        this.manifestBase = manifestBase;
        this.manifestName = manifestName;

        this.loader = loader;
    }

    public static JsonLdManifestLoader load(final String manifestBase, final String manifestName, final DocumentLoader loader) {
        return new JsonLdManifestLoader(manifestBase, manifestName, loader);
    }

    public Stream<JsonLdTestCase> stream() {

        try {

            Document manifest = loader.loadDocument(URI.create(manifestBase + manifestName), new DocumentLoaderOptions());

            assertNotNull(manifest);

            final JsonObject manifestObject = manifest
                                                    .getJsonContent()
                                                    .filter(JsonUtils::isObject)
                                                    .map(JsonObject.class::cast)
                                                    .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED));

            String baseUri = manifestObject.getString("baseIri");

            return manifestObject
                    .getJsonArray("sequence")
                        .stream()
                            .map(JsonValue::asJsonObject)
                            .map(o -> JsonLdTestCase.of(o, manifestName, manifestBase, baseUri, loader));

        } catch (JsonLdError e) {
            fail(e.getMessage());
        }

        return Stream.empty();
    }
}
