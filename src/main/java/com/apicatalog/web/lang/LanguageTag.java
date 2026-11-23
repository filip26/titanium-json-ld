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
package com.apicatalog.web.lang;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * Represents a structured language tag as defined by RFC 5646.
 *
 * <p>
 * Collection components are normalized to empty lists when {@code null} and
 * defensively copied to preserve immutability.
 *
 * @param language           the primary language subtag (shortest ISO 639 code,
 *                           reserved code for future use, or registered
 *                           language subtag)
 * @param languageExtensions a collection of extended language subtags (ISO 639
 *                           extension codes)
 * @param script             the ISO 15924 script subtag, or {@code null} if
 *                           absent
 * @param region             the ISO 3166-1 or UN M.49 region subtag, or
 *                           {@code null} if absent
 * @param extensions         a collection of {@link Extension} subtag groups
 * @param variants           a collection of registered variant subtags
 * @param privateUse         a collection of private-use subtags following the
 *                           {@code x-} singleton
 * 
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646">RFC 5646 - Tags
 *      for Identifying Languages</a>
 */
public record LanguageTag(
        String language,
        Collection<String> languageExtensions,
        String script,
        String region,

        Collection<Extension> extensions,
        Collection<String> variants,
        Collection<String> privateUse) {

    /**
     * Constructs a {@code LanguageTag} with defensive immutable copies for
     * collection components.
     */
    public LanguageTag {
        // Defensive copies to preserve immutability
        languageExtensions = languageExtensions == null ? List.of() : List.copyOf(languageExtensions);
        extensions = extensions == null ? List.of() : List.copyOf(extensions);
        variants = variants == null ? List.of() : List.copyOf(variants);
        privateUse = privateUse == null ? List.of() : List.copyOf(privateUse);
    }

    /**
     * Checks whether the given string is a well-formed RFC 5646 language tag.
     *
     * @param languageTag the string to validate
     * @return {@code true} if {@code languageTag} is well-formed, {@code false}
     *         otherwise
     * @throws NullPointerException if {@code languageTag} is {@code null}
     * 
     * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#section-2.1">RFC
     *      5646 - 2.1 Syntax</a>
     */
    public static boolean isWellFormed(final String languageTag) {
        return LanguageTagParser.isWellFormed(
                Objects.requireNonNull(
                        languageTag,
                        "The parameter 'laguageTag' must not be null"));
    }

    /**
     * Parses a well-formed RFC 5646 language tag string into a {@code LanguageTag}.
     *
     * @param languageTag the string to parse
     * @return the parsed {@code LanguageTag}
     * @throws IllegalArgumentException if {@code languageTag} is not a well-formed
     *                                  language tag
     * 
     * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#section-2.1">RFC
     *      5646 - 2.1 Syntax</a>
     */
    public static LanguageTag of(final String languageTag) {
        return LanguageTagParser.create(
                Objects.requireNonNull(
                        languageTag,
                        "The parameter 'laguageTag' must not be null"))
                .parse();
    }

    /**
     * Formats this language tag according to RFC 5646 subtag ordering.
     *
     * @return the formatted language tag string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();

        sb.append(language);

        if (!languageExtensions.isEmpty()) {
            languageExtensions.forEach(tag -> sb.append('-').append(tag));
        }

        if (script != null) {
            sb.append('-').append(script);
        }

        if (region != null) {
            sb.append('-').append(region);
        }

        if (!variants.isEmpty()) {
            variants.forEach(tag -> sb.append('-').append(tag));
        }

        if (!extensions.isEmpty()) {
            extensions.forEach(tag -> sb.append('-').append(tag));
        }

        if (!privateUse.isEmpty()) {
            sb.append('-').append('x');
            privateUse.forEach(tag -> sb.append('-').append(tag));
        }

        return sb.toString();
    }

    /**
     * Represents an RFC 5646 extension consisting of a single-character singleton
     * code and associated subtags.
     *
     * @param code the single-character extension identifier
     * @param tags the extension subtags; copied into an unmodifiable list on
     *             construction
     * 
     * @see <a href=
     *      "https://datatracker.ietf.org/doc/html/rfc5646#section-2.2.6">RFC 5646 -
     *      2.2.6 Extensions</a>
     */
    public static record Extension(
            char code,
            Collection<String> tags) {

        /**
         * Constructs an {@code Extension} with a non-null tag collection copied into an
         * unmodifiable list.
         *
         * @throws NullPointerException if {@code tags} is {@code null}
         */
        public Extension {
            Objects.requireNonNull(code, "The parameter 'code' must not be null");

            // Defensive copies to preserve immutability
            tags = List.copyOf(Objects.requireNonNull(
                    tags,
                    "The parameter 'tags' must not be null"));
        }

        /**
         * Formats this extension as {@code <code>-<tag1>-<tag2>...}.
         *
         * @return the formatted extension string
         */
        @Override
        public String toString() {
            final var sb = new StringBuilder().append(code);

            tags.forEach(tag -> sb.append('-').append(tag));

            return sb.toString();
        }
    }
}