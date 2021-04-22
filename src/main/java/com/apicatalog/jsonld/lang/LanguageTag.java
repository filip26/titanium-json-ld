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

import com.apicatalog.rdf.lang.RdfAlphabet;

public final class LanguageTag {

    private LanguageTag() {
    }

    /**
     * LANGTAG  ::= [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
     *
     * @see <a href="https://www.w3.org/TR/n-quads/#sec-grammar">N-Quads Grammar</a>
     *
     * @param languageTag to check
     * @return <code>true</code> if the provided value is well-formed language tag
     *
     */
    public static boolean isWellFormed(final String languageTag) {

        if (languageTag == null) {
            throw new IllegalArgumentException();
        }

        final int[] chars = languageTag.trim().codePoints().toArray();
        
        if (chars.length == 0 || RdfAlphabet.ASCII_ALPHA.negate().test(chars[0])) {
            return false;
        }

        if (chars.length == 1) {
            return true;
        }


        int index = 1;
        
        // [a-zA-Z]+
        for (; index < chars.length; index++) {
            
            // ('-' [a-zA-Z0-9]+)*
            if (chars[index] == '-') {
                break;
            }

            // [a-zA-Z]+
            if (RdfAlphabet.ASCII_ALPHA.test(chars[index])) {
                continue;
            }
            
            return false;
        }

        if (index == chars.length - 1) {
            return chars[index] != '-';
        }
        
        index++;

        // ('-' [a-zA-Z0-9]+)*
        for (; index < chars.length; index++) {
            
            // [a-zA-Z0-9]+
            if (RdfAlphabet.ASCII_ALPHA_NUM.test(chars[index])) {
                continue;
            }
            
            return false;            
        }
        
        return true;
    }
}