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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.IntPredicate;
import java.util.regex.Pattern;

import com.apicatalog.jsonld.lang.LanguageTag.Extension;

/**
 * Language tags are used to help identify languages and are defined by
 * <code>RFC 5646</code>.
 *
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646">RFC 5643</a>
 */
final class LanguageTagParser {

    private static final IntPredicate ASCII_ALPHA = ch -> 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z';

    private static final IntPredicate ASCII_DIGIT = ch -> '0' <= ch && ch <= '9';

    private static final IntPredicate ASCII_ALPHA_NUM = ASCII_DIGIT.or(ASCII_ALPHA);

    private static final Pattern LANG_DEL_RE = Pattern.compile("-");

    private final String languageTag;
    private final String[] tags;

    private int tagIndex;
    private boolean verifierMode;

    // parsed values
    private String language;
    private Collection<String> languageExtensions;
    private String script;
    private String region;

    private Collection<Extension> extensions;
    private Collection<String> variants;
    private Collection<String> privateUse;

    LanguageTagParser(final String languageTag, final String[] tags, final boolean verifierMode) {
        this.languageTag = languageTag;
        this.tags = tags;
        this.verifierMode = verifierMode;
        this.tagIndex = 0;
    }

    /**
     * Creates a new {@link LanguageTagParser} instance.
     *
     * @param languageTag used to initialize the parser
     * @return a new instance
     */
    public static final LanguageTagParser create(final String languageTag) {
        return create(languageTag, false);
    }

    public static final boolean isWellFormed(final String languageTag) {

        try {

            return languageTag != null
                    && !languageTag.isBlank()
                    && create(languageTag, true).parse() == null;

        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private static final LanguageTagParser create(final String languageTag, boolean verifierMode) {

        if (languageTag == null) {
            throw new IllegalArgumentException();
        }

        final var stripped = Objects.requireNonNull(
                languageTag,
                "The parameter 'laguageTag' must not be null")
                .trim();

        // must start with ALPHA and ends with ALPHANUM
        if (stripped.length() == 0
                || ASCII_ALPHA.negate().test(stripped.codePointAt(0))
                || ASCII_ALPHA_NUM.negate().test(stripped.codePointAt(stripped.length() - 1))) {
            return new LanguageTagParser(languageTag, null, verifierMode);
        }

        final String[] tags = LANG_DEL_RE.split(stripped);

        if (tags == null || tags.length == 0) {
            return new LanguageTagParser(languageTag, null, verifierMode);
        }

        return new LanguageTagParser(languageTag, tags, verifierMode);
    }

    /**
     * Parses the language tag.
     *
     * @throws IllegalArgumentException if the language tag is not well-formed
     *
     * @return the language tag
     */
    LanguageTag parse() throws IllegalArgumentException {

        if (tags == null || tags.length == 0) {
            throw new IllegalArgumentException("The language tag [" + languageTag + "] is null or blank.");
        }

        tagIndex = 0;

        // language - 2*3ALPHA
        if (acceptAlpha(2, 3, this::setLanguage)) {

            // extlang 3ALPHA
            if (acceptAlpha(3, this::addLanguageExtension)) {

                // *2("-" 3ALPHA)
                acceptAlpha(3, this::addLanguageExtension);
                acceptAlpha(3, this::addLanguageExtension);
            }

            // reserved 4ALPHA or registered for future use 5*8ALPHA
        } else if (acceptAlpha(4, 8, this::setLanguage)) {

            // private use
        } else if (acceptPrivateUse()) {

            if (tagIndex != tags.length) {
                throw new IllegalArgumentException("The language tag [" + languageTag + "] is not well-formed.");
            }

            return verifierMode
                    ? null
                    : new LanguageTag(
                            language,
                            languageExtensions,
                            script,
                            region,
                            extensions,
                            variants,
                            privateUse);

        } else {
            throw new IllegalArgumentException("The language tag [" + languageTag + "] is not well-formed.");
        }

        // ["-" script]
        acceptAlpha(4, this::setScript); // script = 4ALPHA

        // ["-" region]
        if (!acceptAlpha(2, this::setRegion)) { // region = 2ALPHA | 3DIGIT
            acceptDigit(3, this::setRegion);
        }

        // *("-" variant)
        // variant = 5*8alphanum | (DIGIT 3alphanum)
        while (acceptAlphaNun(5, 8, this::addVariant)
                || (digitRange(0, 1) && alphaNumRange(1, 3) && accept(4, this::addVariant)))
            ;

        // *("-" extension)
        // extension = singleton 1*("-" (2*8alphanum))
        // singleton = DIGIT | a-z !- x
        while (acceptDigit(1) || (alphaRange(0, 1) && !tags[tagIndex].equalsIgnoreCase("x") && accept(1))) {

            final var extensionCode = tags[tagIndex - 1].charAt(0);
            final List<String> extensionTags = verifierMode
                    ? List.of()
                    : new ArrayList<>();

            // 1*("-" (2*8alphanum))
            if (!acceptAlphaNun(2, 8, extensionTags::add)) {
                tagIndex--;
                break;
            }

            while (acceptAlphaNun(2, 8, extensionTags::add))
                ;

            if (!verifierMode) {
                addExtension(new Extension(extensionCode, extensionTags));
            }
        }

        acceptPrivateUse();

        if (tagIndex != tags.length) {
            throw new IllegalArgumentException("The language tag [" + languageTag + "] is not well-formed.");
        }

        return verifierMode
                ? null
                : new LanguageTag(
                        language,
                        languageExtensions,
                        script,
                        region,
                        extensions,
                        variants,
                        privateUse);
    }

    private boolean acceptPrivateUse() {
        // ["-" privateuse]
        // privateuse = "x" 1*("-" (1*8alphanum))
        if (alphaRange(0, 1) && tags[tagIndex].equalsIgnoreCase("x") && accept(1)) {

            // 1*("-" (1*8alphanum))
            if (!acceptAlphaNun(1, 8, this::addPrivateUse)) {
                tagIndex--;

            } else {
                while (acceptAlphaNun(1, 8, this::addPrivateUse))
                    ;
                return true;
            }
        }
        return false;
    }

    private boolean acceptAlpha(int length, Consumer<String> consumer) {
        return acceptAlpha(length, length, consumer);
    }

    private boolean acceptAlpha(int min, int max, Consumer<String> consumer) {
        return accept(min, max, ASCII_ALPHA, consumer);
    }

    private boolean acceptDigit(int length) {
        return acceptDigit(length, length, null);
    }

    private boolean acceptDigit(int length, Consumer<String> consumer) {
        return acceptDigit(length, length, consumer);
    }

    private boolean acceptDigit(int min, int max, Consumer<String> consumer) {
        return accept(min, max, ASCII_DIGIT, consumer);
    }

    private boolean acceptAlphaNun(int min, int max, Consumer<String> consumer) {
        return accept(min, max, ASCII_ALPHA_NUM, consumer);
    }

    private boolean accept(int min, int max, IntPredicate predicate, Consumer<String> consumer) {
        if (tagIndex < tags.length
                && tags[tagIndex].length() >= min
                && tags[tagIndex].length() <= max
                && tags[tagIndex].chars().allMatch(predicate)) {

            if (!verifierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }

    private boolean accept(int length) {
        return accept(length, null);
    }

    private boolean accept(int length, Consumer<String> consumer) {
        if (tagIndex < tags.length && tags[tagIndex].length() == length) {

            if (!verifierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }

    private boolean alphaRange(int index, int length) {
        return range(index, length, ASCII_ALPHA);
    }

    private boolean alphaNumRange(int index, int length) {
        return range(index, length, ASCII_ALPHA_NUM);
    }

    private boolean digitRange(int index, int length) {
        return range(index, length, ASCII_DIGIT);
    }

    private boolean range(int index, int length, IntPredicate predicate) {
        return tagIndex < tags.length
                && index < tags[tagIndex].length()
                && (index + length) <= tags[tagIndex].length()
                && tags[tagIndex].substring(index, index + length).chars().allMatch(predicate);
    }

    private void setLanguage(String language) {
        this.language = language;
    }

    private void addLanguageExtension(String languageExtension) {
        if (this.languageExtensions == null) {
            this.languageExtensions = new ArrayList<>();
        }
        this.languageExtensions.add(languageExtension);
    }

    private void setScript(String script) {
        this.script = script;
    }

    private void setRegion(String region) {
        this.region = region;
    }

    private void addVariant(String variant) {
        if (this.variants == null) {
            this.variants = new ArrayList<>();
        }
        this.variants.add(variant);
    }

    private void addExtension(Extension extension) {
        if (this.extensions == null) {
            this.extensions = new ArrayList<>();
        }
        this.extensions.add(extension);
    }

    private void addPrivateUse(String privateTag) {
        if (this.privateUse == null) {
            this.privateUse = new ArrayList<>();
        }
        this.privateUse.add(privateTag);
    }
}