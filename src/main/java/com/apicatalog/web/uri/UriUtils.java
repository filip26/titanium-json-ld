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
package com.apicatalog.web.uri;

import java.net.URI;
import java.util.Objects;

import com.apicatalog.jsonld.lang.Keywords;

public final class UriUtils {

    private UriUtils() {
    }

    public static final boolean isURI(final String value) {
        return value != null
                && !value.isBlank()
                && !Keywords.matchForm(value.strip())
                && create(value.strip()) != null;
    }

    public static final URI create(final String uri) {

        var uriValue = Objects.requireNonNull(
                uri, "The paramenter 'uri' cannot be null").strip();

        if (uriValue.isEmpty()) {
            return null;
        }

        if (uriValue.endsWith(":")) {
            uriValue += ".";

        } else if (uriValue.endsWith("[") || uriValue.endsWith("]")) {
            uriValue = uriValue.substring(0, uriValue.length() - 1);
        }

        try {

            return URI.create(uriValue);

        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    /**
     * Check if the provided URI ends with generic delimiter.
     *
     * @see <a href="https://tools.ietf.org/html/rfc3986#section-2.2">URI - Reserved
     *      Characters </a>
     *
     * @param uri to check
     * @return <code>true</code> if the provided URI ends with delimiter
     */
    public static final boolean endsWithGenDelim(final String uri) {
        return uri.endsWith(":") || uri.endsWith("/") || uri.endsWith("?") || uri.endsWith("#") || uri.endsWith("[")
                || uri.endsWith("]") || uri.endsWith("@");
    }

    public static final boolean isNotURI(final String uri) {
        return uri == null
                || uri.isBlank()
                || Keywords.matchForm(uri.strip())
                || create(uri.strip()) == null;
    }

    public static final boolean isNotAbsoluteUri(final String uri, UriValidationPolicy policy) {
        return !isAbsoluteUri(uri, policy);
    }

    public static final boolean isAbsoluteUri(final String uri, final UriValidationPolicy policy) {
        return switch (policy) {
        case None -> true;

        case SchemeOnly -> startsWithScheme(uri);

        case Full -> {
            if (uri == null || uri.length() < 3) { // minimal form s(1):ssp(1)
                yield false;
            }
            try {
                yield URI.create(uri).isAbsolute();
            } catch (IllegalArgumentException e) {
                yield false;
            }
        }

        default -> false;
        };

    }

    private static final boolean startsWithScheme(final String uri) {

        if (uri == null
                || uri.length() < 2 // a scheme must have at least one letter followed by ':'
                || !Character.isLetter(uri.codePointAt(0)) // a scheme name must start with a letter
        ) {
            return false;
        }

        for (int i = 1; i < uri.length(); i++) {

            if (
            // a scheme name must start with a letter followed by a letter/digit/+/-/.
            Character.isLetterOrDigit(uri.codePointAt(i))
                    || uri.charAt(i) == '-' || uri.charAt(i) == '+' || uri.charAt(i) == '.') {
                continue;
            }

            // a scheme name must be terminated by ':'
            return uri.charAt(i) == ':';
        }
        return false;
    }

    protected static final String recompose(
            final String scheme, 
            final String authority, 
            final String path, 
            final String query, 
            final String fragment) {

        final var builder = new StringBuilder();

        if (scheme != null) {
            builder.append(scheme).append(":");
        }
        if (authority != null) {
            builder.append("//").append(authority);
        }
        if (path != null) {
            builder.append(path);
        }
        if (query != null) {
            builder.append('?').append(query);
        }
        if (fragment != null) {
            builder.append('#').append(fragment);
        }
        return builder.toString();
    }
}
