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
package com.apicatalog.jsonld.http.media;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 *
 * @see <a href="https://tools.ietf.org/html/rfc7231#section-3.1.1.1">Media
 *      Type</a>
 *
 */
public record MediaType(
        String type,
        String subtype,
        Parameters parameters) {

    private static final String TYPE_APPLICATION = "application";
    private static final String TYPE_TEXT = "text";
    private static final String WILDCARD = "*";

    public static final MediaType HTML = new MediaType(TYPE_TEXT, "html");

    public static final MediaType JSON_LD = new MediaType(TYPE_APPLICATION, "ld+json");

    public static final MediaType JSON = new MediaType(TYPE_APPLICATION, "json");

    public static final MediaType XHTML = new MediaType(TYPE_APPLICATION, "xhtml+xml");

    public static final MediaType N_QUADS = new MediaType(TYPE_APPLICATION, "n-quads");

    public static final MediaType ANY = new MediaType(WILDCARD, WILDCARD);

    public MediaType(String type, String subtype) {
        this(type, subtype, Parameters.EMPTY);
    }

    public MediaType(String type, String subtype, Parameters parameters) {
        this.type = Objects.requireNonNull(type);
        this.subtype = Objects.requireNonNull(subtype);
        this.parameters = parameters != null
                ? parameters
                : Parameters.EMPTY;
    }

    public boolean match(MediaType mediaType) {
        return mediaType != null
                && (WILDCARD.equals(type) || WILDCARD.equals(mediaType.type) || Objects.equals(type, mediaType.type))
                && (WILDCARD.equals(subtype) || WILDCARD.equals(mediaType.subtype)
                        || Objects.equals(subtype, mediaType.subtype));
    }

    @Override
    public String toString() {
        return type + "/" + subtype;
    }

    public static final MediaType of(final String value) {
        if (Objects.requireNonNull(value).isBlank()) {
            return null;
        }
        return new MediaTypeParser(value).parse();
    }

    public static record Parameters(Map<String, List<String>> parameters) {

        protected static final Parameters EMPTY = new Parameters(Collections.emptyMap());

        public Set<String> names() {
            return parameters.keySet();
        }

        public List<String> values(final String name) {
            return parameters.containsKey(name)
                    ? Collections.unmodifiableList(parameters.get(name))
                    : Collections.emptyList();
        }

        public Optional<String> firstValue(final String name) {
            return parameters.containsKey(name)
                    ? Optional.of(parameters.get(name).get(0))
                    : Optional.empty();
        }

        public boolean isEmpty() {
            return parameters.isEmpty();
        }
    }
}
