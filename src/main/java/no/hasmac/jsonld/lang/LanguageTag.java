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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;

public final class LanguageTag {

    private String language;
    private Collection<String> languageExtensions;
    private String script;
    private String region;

    private Collection<Extension> extensions;
    private Collection<String> variants;
    private Collection<String> privateUse;

    LanguageTag() {
    }

    /**
     * Language tags are used to help identify languages and are defined by <code>RFC 5646</code>
     *
     * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#section-2.1">RFC 5643 - 2.1 Syntax</a>
     *
     * @param languageTag to check
     * @return <code>true</code> if the provided value is well-formed language tag
     *
     */
    public static boolean isWellFormed(final String languageTag) {

        if (languageTag == null) {
            throw new IllegalArgumentException("The parameter 'laguageTag' must not be null");
        }

       return LanguageTagParser.isWellFormed(languageTag);
    }

    /**
     * Creates a language tag by parsing the given string as defined by RFC&nbsp;5646.
     *
     * @param  languageTag the string to be parsed into a language tag
     * @return The new language tag
     *
     * @throws IllegalArgumentException
     *         if the given string is not well-formed
     */
    public static LanguageTag create(final String languageTag) {
        if (languageTag == null) {
            throw new IllegalArgumentException("The parameter 'laguageTag' must not be null");
        }

        return LanguageTagParser.create(languageTag).parse();
    }

    /**
     * Language as shortest ISO 639 code or reserved code for future use or registered language subtag code.
     *
     * @return the language code
     */
    public String getLanguage() {
        return language;
    }

    /**
     * Collection of ISO 639 codes.
     *
     * @return the extension codes
     */
    public Collection<String> getLanguageExtensions() {
        return languageExtensions;
    }

    /**
     * Script as ISO 15924 code.
     *
     * @return the script name code
     */
    public String getScript() {
        return script;
    }

    /**
     * Region as ISO 3166-1 or UN M.49 code
     *
     * @return the region code
     */
    public String getRegion() {
        return region;
    }

    /**
     * Collection of registered variant codes.
     *
     * @return the variant codes
     */
    public Collection<String> getVariants() {
        return variants;
    }

    /**
     * Collection of extension sub-tags.
     *
     * @return a collection of sub-tags
     */
    public Collection<Extension> getExtensions() {
        return extensions;
    }

    /**
     * Collection of private sub-tags.
     *
     * @return a collection of private sub-tags
     */
    public Collection<String> getPrivateUse() {
        return privateUse;
    }

    protected void setLanguage(String language) {
        this.language = language;
    }

    protected void addLanguageExtension(String languageExtension) {
        if (this.languageExtensions == null) {
            this.languageExtensions = new ArrayList<>();
        }
        this.languageExtensions.add(languageExtension);
    }

    protected void setScript(String script) {
        this.script = script;
    }

    protected void setRegion(String region) {
        this.region = region;
    }

    protected void addVariant(String variant) {
        if (this.variants == null) {
            this.variants = new ArrayList<>();
        }
        this.variants.add(variant);
    }

    protected void addExtension(Extension extension) {
        if (this.extensions == null) {
            this.extensions = new ArrayList<>();
        }
        this.extensions.add(extension);
    }

    protected void addPrivateUse(String privateTag) {
        if (this.privateUse == null) {
            this.privateUse = new ArrayList<>();
        }
        this.privateUse.add(privateTag);
    }

    @Override
    public int hashCode() {
        return Objects.hash(extensions, language, languageExtensions, privateUse, region, script, variants);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        LanguageTag other = (LanguageTag) obj;
        return Objects.equals(extensions, other.extensions) && Objects.equals(language, other.language)
                && Objects.equals(languageExtensions, other.languageExtensions)
                && Objects.equals(privateUse, other.privateUse) && Objects.equals(region, other.region)
                && Objects.equals(script, other.script) && Objects.equals(variants, other.variants);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();

        sb.append(language);

        if (languageExtensions != null) {
            languageExtensions.forEach(tag -> { sb.append('-'); sb.append(tag); });
        }

        if (script != null) {
            sb.append('-');
            sb.append(script);
        }

        if (region != null) {
            sb.append('-');
            sb.append(region);
        }

        if (variants != null) {
            variants.forEach(tag -> { sb.append('-'); sb.append(tag); });
        }

        if (extensions != null) {
            extensions.forEach(tag -> { sb.append('-'); sb.append(tag); });
        }

        if (privateUse != null) {
            sb.append('-');
            sb.append('x');
            privateUse.forEach(tag -> { sb.append('-'); sb.append(tag); });
        }

        return sb.toString();
    }

    public static class Extension {

        private char code;
        private Collection<String> tags;

        public Extension(char code, Collection<String> tags) {
            this.code = code;
            this.tags = tags;
        }

        protected void addTag(String tag) {
            this.tags.add(tag);
        }

        public char getCode() {
            return code;
        }

        public Collection<String> getTags() {
            return tags;
        }

        @Override
        public int hashCode() {
            return Objects.hash(code, tags);
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            Extension other = (Extension) obj;
            return code == other.code && Objects.equals(tags, other.tags);
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();

            sb.append(code);

            tags.forEach(tag -> { sb.append('-'); sb.append(tag); });

            return sb.toString();
        }
    }
}
