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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.net.URI;
import java.util.stream.Stream;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.LoaderOptions;

public final class JsonLdTestManifest {

    public static final String JSON_LD_API_BASE = "zip://json-ld-test-suite-20251009.zip/com/github/w3c/json-ld-api/tests/";
    public static final String JSON_LD_FRAMING_BASE = "zip://json-ld-framing-test-suite-20251009.zip/com/github/w3c/json-ld-framing/tests/";

    // Extension: JSON-LD-STAR (Experimental)
    public static final String JSON_LD_STAR_BASE = "zip://json-ld-star-test-suite-20251009.zip/com/github/json-ld/json-ld-star/tests/";


    public static final String EXPAND = "expand-manifest.jsonld";
    
    private final String manifestName;
    private final String manifestBase;

    private final DocumentLoader loader;

    private JsonLdTestManifest(final String manifestBase, final String manifestName, final DocumentLoader loader) {
        this.manifestBase = manifestBase;
        this.manifestName = manifestName;

        this.loader = loader;
    }

    public static JsonLdTestManifest load(final String manifestBase, final String manifestName, final DocumentLoader loader) {
        return new JsonLdTestManifest(manifestBase, manifestName, loader);
    }

    public Stream<JsonLdTestCase> stream() {

        try {

            final var manifest = loader.loadDocument(URI.create(manifestBase + manifestName), new LoaderOptions());

            assertNotNull(manifest);

            final var node = manifest.content();

            final var baseUri = node.adapter().stringValue(node.property("baseIri"));

            return node.adapter()
                    .elementStream(node.property("sequence"))
                    .map(test -> JsonLdTestCase.of(
                            test,
                            node.adapter(),
                            manifestName,
                            manifestBase,
                            baseUri,
                            loader));

        } catch (JsonLdError e) {
            fail(e);
        }

        return Stream.empty();
    }
}
