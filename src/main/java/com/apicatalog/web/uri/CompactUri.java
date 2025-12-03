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

import java.util.Objects;

/**
 * Represents an immutable Compact URI (CURIE).
 * <p>
 * A CURIE provides a shortened way to express a URI, consisting of a
 * {@code prefix} and a {@code suffix} separated by a colon (e.g.,
 * {@code "ex:resource"}).
 *
 * @see <a href="https://www.w3.org/TR/curie/">W3C CURIE Syntax 1.0</a>
 */
public record CompactUri(
        String prefix,
        String suffix) {

    /**
     * Constructs a new {@code CompactUri}.
     *
     * @param prefix the CURIE prefix, which must not be {@code null}.
     * @param suffix the CURIE suffix, which must not be {@code null}.
     * @throws NullPointerException if prefix or suffix is {@code null}.
     */
    public CompactUri {
        Objects.requireNonNull(prefix, "Prefix must not be null");
        Objects.requireNonNull(suffix, "Suffix must not be null");
    }

    /**
     * Parses a string value into a {@code CompactUri}.
     * <p>
     * The method validates the input string to ensure it contains a colon separator
     * and that the prefix and suffix conform to basic CURIE rules (e.g., a prefix
     * starting with a letter or being an underscore for blank nodes).
     *
     * @param value the string to parse, e.g., {@code "ex:foo"}.
     * @return a new {@code CompactUri} instance if the value is a valid CURIE, or
     *         {@code null} otherwise.
     */
    public static CompactUri of(String value) {

        if (value == null || value.length() < 3) {
            return null;
        }

        final int splitIndex = value.indexOf(':', 1);

        if (splitIndex != -1) {

            final var prefix = value.substring(0, splitIndex);
            final var suffix = value.substring(splitIndex + 1);

            if (!suffix.startsWith("/") && ("_".equals(prefix) || Character.isAlphabetic(prefix.charAt(0)))) {
                return new CompactUri(prefix, suffix);
            }
        }
        return null;
    }

    /**
     * Checks if this CURIE does not represent a blank node.
     * <p>
     * This is the logical opposite of {@link #isBlank()}.
     *
     * @return {@code true} if the prefix is not an underscore, {@code false}
     *         otherwise.
     */
    public boolean isNotBlank() {
        return !isBlank();
    }

    /**
     * Checks if this CURIE represents a blank node.
     * <p>
     * A blank node CURIE is defined by convention as having a prefix equal to an
     * underscore ({@code _}), for example, {@code "_:b1"}.
     *
     * @return {@code true} if the prefix is an underscore, {@code false} otherwise.
     */
    public boolean isBlank() {
        return "_".equals(prefix);
    }

    /**
     * Returns the string representation of this CURIE in the format
     * {@code "prefix:suffix"}.
     *
     * @return the fully composed CURIE string.
     */
    @Override
    public String toString() {
        return prefix + ":" + suffix;
    }
}
