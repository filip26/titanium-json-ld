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
package no.hasmac.jsonld.api;

import java.net.URI;

import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.JsonLdVersion;
import no.hasmac.jsonld.StringUtils;
import no.hasmac.jsonld.uri.UriUtils;

public interface CommonApi<R> {

    /**
     * Override an existing settings with {@link JsonLdOptions}.
     *
     * @param options {@link JsonLdOptions}
     * @return builder instance
     */
    R options(JsonLdOptions options);

    /**
     * Set <code>JSON-LD</code> processing mode. JSON-LD 1.1 is set by default.
     *
     * @param processingMode to set
     * @return builder instance
     */
    R mode(JsonLdVersion processingMode);

    /**
     * Set the base <code>IRI</code>. If set, this overrides the input document's IRI.
     *
     * @param baseUri
     * @return builder instance
     */
    R base(URI baseUri);

    /**
     * Set the base {@link URI}. If set, this overrides the input document's IRI.
     *
     * @param baseLocation
     * @return builder instance
     */
    default R base(String baseLocation) {
        URI baseUri = null;

        if (StringUtils.isNotBlank(baseLocation)) {

            baseUri = UriUtils.create(baseLocation);

            if (baseUri == null) {
                throw new IllegalArgumentException("Base location must be valid URI or null but is [" + baseLocation + "].");
            }
        }
        return base(baseUri);
    }

    /**
     * If set to <code>true</code>, certain algorithm processing steps
     * are ordered lexicographically. If <code>false</code>, order is not
     * considered in processing.
     *
     * @param enable
     * @return builder instance
     */
    R ordered(boolean enable);

    /**
     * Certain algorithm processing steps are ordered lexicographically.
     *
     * @return builder instance
     */
    default R ordered() {
        return ordered(true);
    }
}
