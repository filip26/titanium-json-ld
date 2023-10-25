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
package no.hasmac.jsonld.http.media;

import java.util.Objects;

import no.hasmac.jsonld.StringUtils;

/**
 *
 * @see <a href="https://tools.ietf.org/html/rfc7231#section-3.1.1.1">Media Type</a>
 *
 */
public final class MediaType {

    private static final String TYPE_APPLICATION = "application";
    private static final String TYPE_TEXT = "text";
    private static final String WILDCARD = "*";

    public static final MediaType HTML = new MediaType(TYPE_TEXT, "html");

    public static final MediaType JSON_LD = new MediaType(TYPE_APPLICATION, "ld+json");

    public static final MediaType JSON = new MediaType(TYPE_APPLICATION, "json");

    public static final MediaType XHTML = new MediaType(TYPE_APPLICATION, "xhtml+xml");

    public static final MediaType N_QUADS = new MediaType(TYPE_APPLICATION, "n-quads");

    public static final MediaType ANY = new MediaType(WILDCARD, WILDCARD);

    private final String type;
    private final String subtype;

    private final MediaTypeParameters parameters;

    protected MediaType(String type, String subtype, MediaTypeParameters parameters) {
        this.type = type;
        this.subtype = subtype;
        this.parameters = parameters;
    }

    protected MediaType(String type, String subtype) {
        this(type, subtype, MediaTypeParameters.EMPTY);
    }

    public boolean match(MediaType mediaType) {
        return mediaType != null
                && (WILDCARD.equals(type) || WILDCARD.equals(mediaType.type) || Objects.equals(type, mediaType.type))
                && (WILDCARD.equals(subtype) || WILDCARD.equals(mediaType.subtype) || Objects.equals(subtype, mediaType.subtype))
                ;
    }

    public String type() {
        return type;
    }

    public String subtype() {
        return subtype;
    }

    public MediaTypeParameters parameters() {
        return parameters;
    }

    @Override
    public String toString() {
        return String.valueOf(type).concat("/").concat(subtype);
    }

    public static MediaType of(String type, String subtype) {
        if (type == null || subtype == null) {
            throw new IllegalArgumentException();
        }

        return new MediaType(type, subtype);
    }

    public static MediaType of(final String value) {
        if (value == null) {
            throw new IllegalArgumentException();
        }
        if (StringUtils.isBlank(value)) {
            return null;
        }
        return new MediaTypeParser(value).parse();
    }
}
