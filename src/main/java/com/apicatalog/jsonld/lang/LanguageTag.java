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

public final class LanguageTag {

    private LanguageTag() {
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

        return LanguageTagParser.create(languageTag).isWellFormed();
    }
}