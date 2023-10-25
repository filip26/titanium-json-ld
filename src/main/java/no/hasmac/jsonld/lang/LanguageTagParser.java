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
package no.hasmac.jsonld.lang;

import no.hasmac.jsonld.lang.LanguageTag.Extension;
import no.hasmac.rdf.lang.RdfAlphabet;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * Language tags are used to help identify languages and are defined by <code>RFC 5646</code>.
 *
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646">RFC 5643</a>
 */
final class LanguageTagParser {

    final String languageTag;
    final String[] tags;

    int tagIndex;

    boolean verifierMode;

    LanguageTagParser(final String languageTag, final String[] tags, final boolean verifierMode) {
        this.languageTag = languageTag;
        this.tags = tags;
        this.verifierMode = verifierMode;
        this.tagIndex = 0;
    }

    LanguageTagParser(final String languageTag, String tags, final boolean verifierMode) {
        this.languageTag = languageTag;
        this.tags = new String[]{tags};
        this.verifierMode = verifierMode;
        this.tagIndex = 0;
    }

    /**
     * Creates a new {@link LanguageTagParser} instance.
     *
     * @param languageTag used to initialize the parser
     * @return a new instance
     */
    public static LanguageTagParser create(final String languageTag) {
        return create(languageTag, false);
    }

    public static boolean isWellFormed(final String languageTag) {

        try {
            return create(languageTag, true).parse() != null;

        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private static LanguageTagParser create(final String languageTag, boolean verifierMode) {

        if (languageTag == null) {
            throw new IllegalArgumentException("The parameter 'laguageTag' must not be null");
        }

        if(languageTag.isEmpty()){
            return new LanguageTagParser(languageTag, (String[]) null, verifierMode);
        }

        final String stripped = languageTag.trim();

        // must start with ALPHA  and ends with ALPHANUM
        if (stripped.isEmpty()
                || doesNotStartWithAlpha(stripped)
                || doesNotEndWithAlphanum(stripped)
        ) {
            return new LanguageTagParser(languageTag, (String[]) null, verifierMode);
        }

        if(stripped.contains("-")){
            return new LanguageTagParser(languageTag, stripped.split("-"), verifierMode);
        }else {
            return new LanguageTagParser(languageTag, stripped, verifierMode);
        }


    }

    private static boolean doesNotEndWithAlphanum(String stripped) {
        int ch = stripped.codePointAt(stripped.length()-1);
        return !('a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ('0' <= ch && ch <= '9'));
    }

    private static boolean doesNotStartWithAlpha(String stripped) {
        int ch = stripped.codePointAt(0);
        return !('a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z');
    }

    /**
     * Parses the language tag.
     *
     * @return the language tag
     * @throws IllegalArgumentException if the language tag is not well-formed
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
        if (!acceptAlpha(2, tag::setRegion)) {  // region = 2ALPHA | 3DIGIT
            acceptDigit(tag::setRegion);
        }

        // *("-" variant)
        // variant = 5*8alphanum | (DIGIT 3alphanum)
        while (acceptAlphaNun(5, tag::addVariant)
                || (digitRange() && alphaNumRange() && accept(tag::addVariant))) ;

        // *("-" extension)
        // extension = singleton 1*("-" (2*8alphanum))
        // singleton = DIGIT | a-z !- x
        while (acceptDigit() || (alphaRange() && !tags[tagIndex].equalsIgnoreCase("x") && accept())) {

            final Extension extension = new Extension(tags[tagIndex - 1].charAt(0), new ArrayList<>());

            // 1*("-" (2*8alphanum))
            if (!acceptAlphaNun(2, extension::addTag)) {
                tagIndex--;
                break;
            }

            while (acceptAlphaNun(2, extension::addTag)) ;

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
        if (alphaRange() && tags[tagIndex].equalsIgnoreCase("x") && accept()) {

            // 1*("-" (1*8alphanum))
            if (!acceptAlphaNun(1, tag::addPrivateUse)) {
                tagIndex--;

            } else {
                while (acceptAlphaNun(1, tag::addPrivateUse)) ;
                return true;
            }
        }
        return false;
    }

    boolean acceptAlpha(int length, Consumer<String> consumer) {
        if (tagIndex < tags.length
                && tags[tagIndex].length() >= length
                && tags[tagIndex].length() <= length
                && allAsciiAlpha()) {

            if (!verifierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }

    private boolean allAsciiAlpha() {
        String tag = tags[tagIndex];

        for (int i = 0; i < tag.length(); i++) {
            int ch = tag.codePointAt(i);
            if (!('a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z')) {
                return false;
            }
        }

        return true;
    }

    boolean acceptAlpha(int min, int max, Consumer<String> consumer) {
        if (tagIndex < tags.length
                && tags[tagIndex].length() >= min
                && tags[tagIndex].length() <= max
                && allAsciiAlpha()) {

            if (!verifierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }

    boolean acceptDigit() {
        if (tagIndex < tags.length
                && tags[tagIndex].length() == 1
                && tags[tagIndex].chars().allMatch(RdfAlphabet.ASCII_DIGIT)) {

            tagIndex++;
            return true;
        }
        return false;
    }

    boolean acceptDigit(Consumer<String> consumer) {
        if (tagIndex < tags.length
                && tags[tagIndex].length() == 3
                && tags[tagIndex].chars().allMatch(RdfAlphabet.ASCII_DIGIT)) {

            if (!verifierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }


    boolean acceptAlphaNun(int min, Consumer<String> consumer) {
        if (tagIndex < tags.length
                && tags[tagIndex].length() >= min
                && tags[tagIndex].length() <= 8
                && tags[tagIndex].chars().allMatch(RdfAlphabet.ASCII_ALPHA_NUM)) {

            if (!verifierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }

    boolean accept() {
        if (tagIndex < tags.length && tags[tagIndex].length() == 1) {

            tagIndex++;
            return true;
        }
        return false;
    }

    boolean accept(Consumer<String> consumer) {
        if (tagIndex < tags.length && tags[tagIndex].length() == 4) {

            if (!verifierMode && consumer != null) {
                consumer.accept(tags[tagIndex]);
            }

            tagIndex++;
            return true;
        }
        return false;
    }

    boolean alphaRange() {
        return
                tagIndex < tags.length
                        && !tags[tagIndex].isEmpty()
                        && tags[tagIndex].substring(0, 1).chars().allMatch(RdfAlphabet.ASCII_ALPHA)
                ;
    }

    boolean alphaNumRange() {
        return
                tagIndex < tags.length
                        && 1 < tags[tagIndex].length()
                        && (4) <= tags[tagIndex].length()
                        && tags[tagIndex].substring(1, 4).chars().allMatch(RdfAlphabet.ASCII_ALPHA_NUM)
                ;
    }

    boolean digitRange() {
        return
                tagIndex < tags.length
                        && !tags[tagIndex].isEmpty()
                        && tags[tagIndex].substring(0, 1).chars().allMatch(RdfAlphabet.ASCII_DIGIT)
                ;
    }


}
