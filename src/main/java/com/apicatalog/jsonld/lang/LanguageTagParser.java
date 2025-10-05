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

    static final IntPredicate ASCII_ALPHA = ch -> 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z';

    static final IntPredicate ASCII_DIGIT = ch -> '0' <= ch && ch <= '9';

    static final IntPredicate ASCII_ALPHA_NUM = ASCII_DIGIT.or(ASCII_ALPHA);

  
    
    static final Pattern LANG_DEL_RE = Pattern.compile("-");

    final String languageTag;
    final String[] tags;

    int tagIndex;

    boolean verififierMode;

    LanguageTagParser(final String languageTag, final String[] tags, final boolean verifierMode) {
        this.languageTag = languageTag;
        this.tags = tags;
        this.verififierMode = verifierMode;
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
            return create(languageTag, true).parse() != null;

        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private static final LanguageTagParser create(final String languageTag, boolean verifierMode) {

        if (languageTag == null) {
            throw new IllegalArgumentException("The parameter 'laguageTag' must not be null");
        }

        final String stripped = languageTag.trim();

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
            return null;
        }

        final LanguageTag tag = new LanguageTag();

        tagIndex = 0;

        // language - 2*3ALPHA
        if (acceptAlpha(2, 3, tag::setLanguage)) {

            // extlang 3ALPHA
            if (acceptAlpha(3, tag::addLanguageExtension)) {

                // *2("-" 3ALPHA)
                acceptAlpha(3, tag::addLanguageExtension);
                acceptAlpha(3, tag::addLanguageExtension);
            }

            // reserved 4ALPHA or registered for future use 5*8ALPHA
        } else if (acceptAlpha(4, 8, tag::setLanguage)) {

            // private use
        } else if (acceptPrivateUse(tag)) {

            if (tagIndex != tags.length) {
                throw new IllegalArgumentException("The language tag [" + languageTag + "] is not well-formed.");
            }

            return tag;

        } else {
            throw new IllegalArgumentException("The language tag [" + languageTag + "] is not well-formed.");
        }

        // ["-" script]
        acceptAlpha(4, tag::setScript); // script = 4ALPHA

        // ["-" region]
        if (!acceptAlpha(2, tag::setRegion)) { // region = 2ALPHA | 3DIGIT
            acceptDigit(3, tag::setRegion);
        }

        // *("-" variant)
        // variant = 5*8alphanum | (DIGIT 3alphanum)
        while (acceptAlphaNun(5, 8, tag::addVariant)
                || (digitRange(0, 1) && alphaNumRange(1, 3) && accept(4, tag::addVariant)))
            ;

        // *("-" extension)
        // extension = singleton 1*("-" (2*8alphanum))
        // singleton = DIGIT | a-z !- x
        while (acceptDigit(1) || (alphaRange(0, 1) && !tags[tagIndex].equalsIgnoreCase("x") && accept(1))) {

            final Extension extension = new Extension(tags[tagIndex - 1].charAt(0), new ArrayList<>());

            // 1*("-" (2*8alphanum))
            if (!acceptAlphaNun(2, 8, extension::addTag)) {
                tagIndex--;
                break;
            }

            while (acceptAlphaNun(2, 8, extension::addTag))
                ;

            tag.addExtension(extension);
        }

        acceptPrivateUse(tag);

        if (tagIndex != tags.length) {
            throw new IllegalArgumentException("The language tag [" + languageTag + "] is not well-formed.");
        }

        return tag;
    }

    boolean acceptPrivateUse(final LanguageTag tag) {
        // ["-" privateuse]
        // privateuse = "x" 1*("-" (1*8alphanum))
        if (alphaRange(0, 1) && tags[tagIndex].equalsIgnoreCase("x") && accept(1)) {

            // 1*("-" (1*8alphanum))
            if (!acceptAlphaNun(1, 8, tag::addPrivateUse)) {
                tagIndex--;

            } else {
                while (acceptAlphaNun(1, 8, tag::addPrivateUse))
                    ;
                return true;
            }
        }
        return false;
    }

    boolean acceptAlpha(int length, Consumer<String> consumer) {
        return acceptAlpha(length, length, consumer);
    }

    boolean acceptAlpha(int min, int max, Consumer<String> consumer) {
        return accept(min, max, ASCII_ALPHA, consumer);
    }

    boolean acceptDigit(int length) {
        return acceptDigit(length, length, null);
    }

    boolean acceptDigit(int length, Consumer<String> consumer) {
        return acceptDigit(length, length, consumer);
    }

    boolean acceptDigit(int min, int max, Consumer<String> consumer) {
        return accept(min, max, ASCII_DIGIT, consumer);
    }

    boolean acceptAlphaNun(int min, int max, Consumer<String> consumer) {
        return accept(min, max, ASCII_ALPHA_NUM, consumer);
    }

    boolean accept(int min, int max, IntPredicate predicate, Consumer<String> consumer) {
        if (tagIndex < tags.length
                && tags[tagIndex].length() >= min
                && tags[tagIndex].length() <= max
                && tags[tagIndex].chars().allMatch(predicate)) {

            if (!verififierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }

    boolean accept(int length) {
        return accept(length, null);
    }

    boolean accept(int length, Consumer<String> consumer) {
        if (tagIndex < tags.length && tags[tagIndex].length() == length) {

            if (!verififierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }

    boolean alphaRange(int index, int length) {
        return range(index, length, ASCII_ALPHA);
    }

    boolean alphaNumRange(int index, int length) {
        return range(index, length, ASCII_ALPHA_NUM);
    }

    boolean digitRange(int index, int length) {
        return range(index, length, ASCII_DIGIT);
    }

    boolean range(int index, int length, IntPredicate predicate) {
        return tagIndex < tags.length
                && index < tags[tagIndex].length()
                && (index + length) <= tags[tagIndex].length()
                && tags[tagIndex].substring(index, index + length).chars().allMatch(predicate);
    }
}