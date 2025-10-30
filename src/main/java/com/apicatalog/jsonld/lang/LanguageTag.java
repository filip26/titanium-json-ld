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
package com.apicatalog.jsonld.lang;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

public record LanguageTag(
        String language,
        Collection<String> languageExtensions,
        String script,
        String region,

        Collection<Extension> extensions,
        Collection<String> variants,
        Collection<String> privateUse) {

    public LanguageTag {
        // Defensive copies to preserve immutability
        languageExtensions = languageExtensions == null ? List.of() : List.copyOf(languageExtensions);
        extensions = extensions == null ? List.of() : List.copyOf(extensions);
        variants = variants == null ? List.of() : List.copyOf(variants);
        privateUse = privateUse == null ? List.of() : List.copyOf(privateUse);
    }

    /**
     * Language tags are used to help identify languages and are defined by
     * <code>RFC 5646</code>
     *
     * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#section-2.1">RFC
     *      5643 - 2.1 Syntax</a>
     *
     * @param languageTag to check
     * @return <code>true</code> if the provided value is well-formed language tag
     *
     */
    public static boolean isWellFormed(final String languageTag) {
        return LanguageTagParser.isWellFormed(
                Objects.requireNonNull(
                        languageTag,
                        "The parameter 'laguageTag' must not be null"));
    }

    /**
     * Creates a language tag by parsing the given string as defined by
     * RFC&nbsp;5646.
     *
     * @param languageTag the string to be parsed into a language tag
     * @return The new language tag
     *
     * @throws IllegalArgumentException if the given string is not well-formed
     *                                  language tag
     */
    public static LanguageTag of(final String languageTag) {
        return LanguageTagParser.create(
                Objects.requireNonNull(
                        languageTag,
                        "The parameter 'laguageTag' must not be null"))
                .parse();
    }

    /**
     * Language as shortest ISO 639 code or reserved code for future use or
     * registered language subtag code.
     *
     * @return the language code
     */
    public String language() {
        return language;
    }

    /**
     * Collection of ISO 639 codes.
     *
     * @return the extension codes
     */
    public Collection<String> languageExtensions() {
        return languageExtensions;
    }

    /**
     * Script as ISO 15924 code.
     *
     * @return the script name code
     */
    public String script() {
        return script;
    }

    /**
     * Region as ISO 3166-1 or UN M.49 code
     *
     * @return the region code
     */
    public String region() {
        return region;
    }

    /**
     * Collection of registered variant codes.
     *
     * @return the variant codes
     */
    public Collection<String> variants() {
        return variants;
    }

    /**
     * Collection of extension sub-tags.
     *
     * @return a collection of sub-tags
     */
    public Collection<Extension> extensions() {
        return extensions;
    }

    /**
     * Collection of private sub-tags.
     *
     * @return a collection of private sub-tags
     */
    public Collection<String> privateUse() {
        return privateUse;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();

        sb.append(language);

        if (languageExtensions != null && !languageExtensions.isEmpty()) {
            languageExtensions.forEach(tag -> sb.append('-').append(tag));
        }

        if (script != null) {
            sb.append('-').append(script);
        }

        if (region != null) {
            sb.append('-').append(region);
        }

        if (variants != null && !variants.isEmpty()) {
            variants.forEach(tag -> sb.append('-').append(tag));
        }

        if (extensions != null && !extensions.isEmpty()) {
            extensions.forEach(tag -> sb.append('-').append(tag));
        }

        if (privateUse != null && !privateUse.isEmpty()) {
            sb.append('-').append('x');
            privateUse.forEach(tag -> sb.append('-').append(tag));
        }

        return sb.toString();
    }

    public static record Extension(
            char code,
            Collection<String> tags) {

        public Extension {
            Objects.requireNonNull(code, "The parameter 'code' must not be null");

            // Defensive copies to preserve immutability
            tags = List.copyOf(Objects.requireNonNull(
                    tags,
                    "The parameter 'tags' must not be null"));
        }

        @Override
        public String toString() {
            final var sb = new StringBuilder().append(code);

            tags.forEach(tag -> sb.append('-').append(tag));

            return sb.toString();
        }
    }
}