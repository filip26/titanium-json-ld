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
 * Represents an immutable <i>media type</i> (also known as a MIME type) as
 * defined in
 * <a href="https://datatracker.ietf.org/doc/html/rfc7231#section-3.1.1.1"
 * target="_blank"> RFC 7231 §3.1.1.1</a>.
 * <p>
 * A media type consists of a primary {@code type}, a {@code subtype}, and an
 * optional set of parameters. For example,
 * {@code application/ld+json; profile="http://www.w3.org/ns/json-ld#expanded"}.
 * </p>
 * <p>
 * This record provides a type-safe representation of media types used in HTTP
 * message headers (e.g. {@code Content-Type}, {@code Accept}). Common constants
 * such as {@link #JSON_LD} and {@link #HTML} are predefined.
 * </p>
 *
 * <h2>Examples</h2>
 * 
 * <pre>{@code
 * MediaType jsonLd = MediaType.JSON_LD;
 * MediaType html = MediaType.of("text/html");
 *
 * boolean matches = jsonLd.match(MediaType.of("application/ld+json"));
 * }</pre>
 *
 * @see <a href=
 *      "https://datatracker.ietf.org/doc/html/rfc7231#section-3.1.1.1">RFC 7231
 *      §3.1.1.1</a>
 * @see <a href=
 *      "https://www.iana.org/assignments/media-types/media-types.xhtml">IANA
 *      Media Types Registry</a>
 */
public record MediaType(
        String type,
        String subtype,
        Map<String, List<String>> parameters) {

    private static final String TYPE_APPLICATION = "application";
    private static final String TYPE_TEXT = "text";
    private static final String WILDCARD = "*";

    /** {@code text/html} */
    public static final MediaType HTML = new MediaType(TYPE_TEXT, "html");

    /** {@code application/ld+json} */
    public static final MediaType JSON_LD = new MediaType(TYPE_APPLICATION, "ld+json");

    /** {@code application/json} */
    public static final MediaType JSON = new MediaType(TYPE_APPLICATION, "json");

    /** {@code application/xhtml+xml} */
    public static final MediaType XHTML = new MediaType(TYPE_APPLICATION, "xhtml+xml");

    /** {@code application/n-quads} */
    public static final MediaType N_QUADS = new MediaType(TYPE_APPLICATION, "n-quads");

    /** {@code application/ld+cbor} */
    public static final MediaType CBOR_LD = new MediaType(TYPE_APPLICATION, "ld+cbor");

    /** {@code application/cbor} */
    public static final MediaType CBOR = new MediaType(TYPE_APPLICATION, "cbor");

    /** {@code application/ld+yaml} */
    public static final MediaType YAML_LD = new MediaType(TYPE_APPLICATION, "ld+yaml");

    /** {@code application/yaml} */
    public static final MediaType YAML = new MediaType(TYPE_APPLICATION, "yaml");

    public static final MediaType ANY = new MediaType(WILDCARD, WILDCARD);

    /**
     * Creates a new {@code MediaType} instance.
     *
     * @param type       the primary type, e.g. {@code application}
     * @param subtype    the subtype, e.g. {@code json}
     * @param parameters an optional map of parameters (may be {@code null})
     * @throws NullPointerException if {@code type} or {@code subtype} is
     *                              {@code null}
     */
    public MediaType {
        Objects.requireNonNull(type);
        Objects.requireNonNull(subtype);

        parameters = parameters == null ? Map.of() : Map.copyOf(parameters);
    }

    /**
     * Creates a new {@code MediaType} instance without parameters.
     *
     * @param type    the primary type, e.g. {@code application}
     * @param subtype the subtype, e.g. {@code json}
     */
    public MediaType(String type, String subtype) {
        this(type, subtype, null);
    }

    /**
     * Parses the given string into a {@code MediaType}.
     *
     * @param value a string representation such as {@code "application/json"}
     * @return a new {@code MediaType} instance, or {@code null} if the value is
     *         blank
     * @throws NullPointerException if {@code value} is {@code null}
     * @see MediaTypeParser
     */
    public static final MediaType of(final String value) {
        if (Objects.requireNonNull(value).isBlank()) {
            return null;
        }
        return MediaTypeParser.parse(value);
    }

    /**
     * Tests whether this media type matches another, considering wildcards.
     *
     * @param mediaType the media type to compare against
     * @return {@code true} if both types and subtypes match, or if either uses a
     *         wildcard
     */
    public boolean match(final MediaType mediaType) {
        return mediaType != null && match(mediaType.type, mediaType.subtype);
    }

    /**
     * Tests whether this media type matches the specified type and subtype. A
     * wildcard ({@code *}) in either type or subtype matches any value.
     *
     * @param type    the type to compare
     * @param subtype the subtype to compare
     * @return {@code true} if both type and subtype match according to wildcard
     *         rules
     */
    public boolean match(final String type, final String subtype) {
        return (WILDCARD.equals(this.type)
                || WILDCARD.equals(type)
                || Objects.equals(this.type, type))
                && (WILDCARD.equals(this.subtype)
                        || WILDCARD.equals(subtype)
                        || Objects.equals(this.subtype, subtype));
    }

    /**
     * Returns the media type formatted as a standard MIME string.
     * <p>
     * The result includes the primary type, subtype, and any parameters in the
     * conventional {@code type/subtype; name=value} form defined by
     * <a href="https://datatracker.ietf.org/doc/html/rfc7231#section-3.1.1.1">
     * RFC&nbsp;7231&nbsp;Section&nbsp;3.1.1.1</a>.
     * </p>
     *
     * <h2>Examples</h2>
     * 
     * <pre>{@code
     * new MediaType("application", "ld+json").toString();
     * // application/ld+json
     *
     * new MediaType(
     *         "application",
     *         "ld+json",
     *         Map.of("profile", List.of("http://www.w3.org/ns/json-ld#expanded"))).toString();
     * // application/ld+json; profile="http://www.w3.org/ns/json-ld#expanded"
     * }</pre>
     *
     * @return the canonical string representation of this media type
     */
    @Override
    public String toString() {
        if (parameters.isEmpty()) {
            return type + "/" + subtype;
        }
        var builder = new StringBuilder(type).append('/').append(subtype);

        parameters.forEach((name, values) -> {
            for (var value : values) {
                // Quote parameter values that contain characters outside token chars,
                // e.g. URIs with ':' or '#'. Using simple heuristic: quote when value contains
                // space or non-token char.
                final boolean needsQuotes = value.indexOf(' ') >= 0 || !value.matches("[A-Za-z0-9!#$%&'*+.^_`|~-]+");
                if (needsQuotes) {
                    builder.append("; ").append(name).append("=\"").append(value).append('"');
                } else {
                    builder.append("; ").append(name).append('=').append(value);
                }
            }
        });
        return builder.toString();
    }

    /**
     * Returns the set of parameter names defined for this media type.
     *
     * @return an unmodifiable set of parameter names
     */
    public Set<String> parameterNames() {
        return parameters.keySet();
    }

    /**
     * Returns all values of a given parameter.
     *
     * @param name the parameter name
     * @return an unmodifiable list of values, or an empty list if not present
     */
    public List<String> parameters(final String name) {
        return parameters.containsKey(name)
                ? List.copyOf(parameters.get(name))
                : List.of();
    }

    /**
     * Returns the first value of a given parameter, if present.
     *
     * @param name the parameter name
     * @return an {@link Optional} containing the first parameter value, or empty if
     *         not found
     */
    public Optional<String> findFirstParameter(final String name) {
        return parameters.containsKey(name)
                ? Optional.of(parameters.get(name).get(0))
                : Optional.empty();
    }
}
