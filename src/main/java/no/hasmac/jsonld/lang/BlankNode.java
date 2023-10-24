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

import no.hasmac.rdf.lang.RdfAlphabet;

/**
 *
 * @see <a href="https://www.w3.org/TR/curie/">A syntax for expressing Compact
 *      URIs</a>
 *
 */
public final class BlankNode {

    private BlankNode() {

    }

    /**
     * Returns <code>true</code> if the given value starts with a blank node prefix '<code>_:</code>'.
     *
     * @param value to check
     * @return <code>true</code> if the give value has blank node prefix
     */
    public static boolean hasPrefix(final String value) {
        return value.startsWith("_:");
    }

    /**
     * BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
     *
     * @see <a href="https://www.w3.org/TR/n-quads/#sec-grammar">N-Quads Grammar</a>
     *
     * @param blankNodeId to check
     * @return <code>true</code> if the provided string is well formed blank node identifier
     */
    public static boolean isWellFormed(final String blankNodeId) {

        if (blankNodeId == null) {
            throw new IllegalArgumentException();
        }

        if (blankNodeId.length() < 3) {
            return false;
        }



        if (blankNodeId.codePointAt(0) != '_'
                || blankNodeId.codePointAt(1) != ':'
                || (RdfAlphabet.PN_CHARS_U.negate().test(blankNodeId.codePointAt(2))
                        && RdfAlphabet.ASCII_DIGIT.negate().test(blankNodeId.codePointAt(2)))
                || blankNodeId.endsWith(".")){
            return false;
        }

        int codePointCount = blankNodeId.codePointCount(0, blankNodeId.length());

        if (codePointCount == 3) {
            return true;
        }



        return blankNodeId.codePoints().skip(3).limit(codePointCount-4).allMatch(RdfAlphabet.PN_CHARS.or(ch -> ch == '.'));
//        return Arrays.stream(chars, 3, chars.length - 1).allMatch(RdfAlphabet.PN_CHARS.or(ch -> ch == '.'));
    }
}
