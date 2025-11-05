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
package com.apicatalog.web.media;

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
        Map<String, List<String>> parameters) {

    private static final String TYPE_APPLICATION = "application";
    private static final String TYPE_TEXT = "text";
    private static final String WILDCARD = "*";

    public static final MediaType HTML = new MediaType(TYPE_TEXT, "html");

    public static final MediaType JSON_LD = new MediaType(TYPE_APPLICATION, "ld+json");

    public static final MediaType JSON = new MediaType(TYPE_APPLICATION, "json");

    public static final MediaType XHTML = new MediaType(TYPE_APPLICATION, "xhtml+xml");

    public static final MediaType N_QUADS = new MediaType(TYPE_APPLICATION, "n-quads");

    public static final MediaType ANY = new MediaType(WILDCARD, WILDCARD);

    public MediaType {
        Objects.requireNonNull(type);
        Objects.requireNonNull(subtype);

        parameters = parameters == null ? Map.of() : Map.copyOf(parameters);
    }

    public MediaType(String type, String subtype) {
        this(type, subtype, null);
    }

    public static final MediaType of(final String value) {
        if (Objects.requireNonNull(value).isBlank()) {
            return null;
        }
        return MediaTypeParser.parse(value);
    }

    public boolean match(final MediaType mediaType) {
        return mediaType != null && match(mediaType.type, mediaType.subtype);
    }

    public boolean match(final String type, final String subtype) {
        return (WILDCARD.equals(this.type)
                || WILDCARD.equals(type)
                || Objects.equals(this.type, type))
                && (WILDCARD.equals(this.subtype)
                        || WILDCARD.equals(subtype)
                        || Objects.equals(this.subtype, subtype));
    }

    @Override
    public String toString() {
        return type + "/" + subtype;
    }

    public Set<String> parameterNames() {
        return parameters.keySet();
    }

    public List<String> parameters(final String name) {
        return parameters.containsKey(name)
                ? List.copyOf(parameters.get(name))
                : List.of();
    }

    public Optional<String> findFirstParameter(final String name) {
        return parameters.containsKey(name)
                ? Optional.of(parameters.get(name).get(0))
                : Optional.empty();
    }
}
