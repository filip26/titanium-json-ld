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

import java.util.function.IntPredicate;

import com.apicatalog.rdf.lang.RdfAlphabet;

/**
 * Language tags are used to help identify languages and are defined by <code>RFC 5646</code>
 *
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#section-2.1">RFC 5643 - 2.1 Syntax</a>
 */
final class LanguageTagParser {

    final String languageTag;
    final String[] tags;

    int tagIndex;

    private LanguageTagParser(String languageTag, String[] tags) {
        this.languageTag = languageTag;
        this.tags = tags;
        this.tagIndex = 0;
    }

    /**
     * Creates a new {@link LanguageTagParser} instance.
     * @param languageTag used to initialize the parser
     * @return a new instance
     */
    public static final LanguageTagParser create(final String languageTag) {

        if (languageTag == null) {
            throw new IllegalArgumentException("The parameter 'laguageTag' must not be null");
        }

        final String stripped = languageTag.trim();

        // must start with ALPHA  and ends with ALPHANUM
        if (stripped.length() == 0
                || RdfAlphabet.ASCII_ALPHA.negate().test(stripped.codePointAt(0))
                || RdfAlphabet.ASCII_ALPHA_NUM.negate().test(stripped.codePointAt(stripped.length() - 1))
                ) {
            return new LanguageTagParser(languageTag, null);
        }

        final String[] tags = stripped.split("-");


        if (tags == null || tags.length == 0) {
            return new LanguageTagParser(languageTag, null);
        }

        return new LanguageTagParser(languageTag, tags);
    }

    /**
     * Checks if the language tag is well-formed
     *
     * @return <code>true</code> if the tag is well-formed language tag
     */
    public boolean isWellFormed() {

        if (tags == null || tags.length == 0) {
            return false;
        }

        if (tagIndex == tags.length) {
            return true;
        }

        // language - 2*3ALPHA
        if (acceptAlpha(2, 3)) {

            // extlang 3ALPHA
            if (acceptAlpha(3)) {

                // *2("-" 3ALPHA)
                acceptAlpha(3);
                acceptAlpha(3);
            }

        // reserved 4ALPHA or registered for future use 5*8ALPHA
        } else if (acceptAlpha(4, 8)) {

        // private use
        } else if (acceptPrivateUse()) {
            return tagIndex == tags.length;

        } else {
            return false;
        }

        // ["-" script]
        acceptAlpha(4); // script = 4ALPHA

        // ["-" region]
        if (!acceptAlpha(2)) {  // region = 2ALPHA | 3DIGIT
            acceptDigit(3);
        }

        // *("-" variant)
        // variant = 5*8alphanum | (DIGIT 3alphanum)
        while (acceptAlphaNun(5, 8) || (digitRange(0, 1) && alphaNumRange(1, 3) && accept(4)));

        // *("-" extension)
        // extension = singleton 1*("-" (2*8alphanum))
        // singleton = DIGIT | a-z !- x
        while (acceptDigit(1) || (alphaRange(0, 1) && !tags[tagIndex].equalsIgnoreCase("x") && accept(1))) {

            // 1*("-" (2*8alphanum))
            if (!acceptAlphaNun(2, 8)) {
                tagIndex--;
                break;
            }

            while (acceptAlphaNun(2, 8));
        }

        acceptPrivateUse();

        return tagIndex == tags.length;
    }

    boolean acceptPrivateUse() {
        // ["-" privateuse]
        // privateuse = "x" 1*("-" (1*8alphanum))
        if (alphaRange(0, 1) && tags[tagIndex].equalsIgnoreCase("x") && accept(1)) {

            // 1*("-" (2*8alphanum))
            if (!acceptAlphaNun(1, 8)) {
                tagIndex--;

            } else {
                while (acceptAlphaNun(1, 8));
                return true;
            }
        }
        return false;
    }

    boolean acceptAlpha(int length) {
        return acceptAlpha(length, length);
    }

    boolean acceptAlpha(int min, int max) {
        return accept(min, max, RdfAlphabet.ASCII_ALPHA);
    }

    boolean acceptDigit(int length) {
        return acceptDigit(length, length);
    }

    boolean acceptDigit(int min, int max) {
        return accept(min, max, RdfAlphabet.ASCII_DIGIT);
    }

    boolean acceptAlphaNun(int min, int max) {
        return accept(min, max, RdfAlphabet.ASCII_ALPHA_NUM);
    }

    boolean accept(int min, int max, IntPredicate predicate) {

        if (tagIndex >= tags.length) {
            return false;
        }

        if (tags[tagIndex].length() >= min
                && tags[tagIndex].length() <= max
                && tags[tagIndex].chars().allMatch(predicate)) {
            tagIndex++;
            return true;
        }
        return false;
    }

    boolean alphaRange(int index, int length) {
        return range(index, length, RdfAlphabet.ASCII_ALPHA);
    }

    boolean alphaNumRange(int index, int length) {
        return range(index, length, RdfAlphabet.ASCII_ALPHA_NUM);
    }

    boolean digitRange(int index, int length) {
        return range(index, length, RdfAlphabet.ASCII_DIGIT);
    }

    boolean range(int index, int length, IntPredicate predicate) {
        if (tagIndex >= tags.length) {
            return false;
        }

        return (index < tags[tagIndex].length()
            && (index + length) <= tags[tagIndex].length()
            && tags[tagIndex].substring(index, index + length).chars().allMatch(predicate)
            );
    }

    boolean accept(int length) {
        if (tagIndex < tags.length && tags[tagIndex].length() == length) {
            tagIndex++;
            return true;
        }
        return false;
    }

}