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
package com.apicatalog.web.uri;

import java.util.function.IntPredicate;

public final class BlankNode {

    static final IntPredicate ASCII_ALPHA = ch -> 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z';

    static final IntPredicate ASCII_DIGIT = ch -> '0' <= ch && ch <= '9';

    static final IntPredicate PN_CHARS_BASE = ASCII_ALPHA.or(ch -> (0x00C0 <= ch && ch <= 0x00D6)
            || (0x00D8 <= ch && ch <= 0x00F6)
            || (0x00F8 <= ch && ch <= 0x02FF)
            || (0x0370 <= ch && ch <= 0x037D)
            || (0x037F <= ch && ch <= 0x1FFF)
            || (0x200C <= ch && ch <= 0x200D)
            || (0x2070 <= ch && ch <= 0x218F)
            || (0x2C00 <= ch && ch <= 0x2FEF)
            || (0x3001 <= ch && ch <= 0xD7FF)
            || (0xF900 <= ch && ch <= 0xFDCF)
            || (0xFDF0 <= ch && ch <= 0xFFFD)
            || (0x10000 <= ch && ch <= 0xEFFFF));

    static final IntPredicate PN_CHARS_U = PN_CHARS_BASE.or(ch -> '_' == ch || ':' == ch);

    static final IntPredicate PN_CHARS = PN_CHARS_U.or(ASCII_DIGIT).or(ch -> '-' == ch
            || 0x00B7 == ch
            || (0x0300 <= ch && ch <= 0x036F)
            || (0x203F <= ch && ch <= 0x2040));

    private BlankNode() {

    }

    /**
     * Returns <code>true</code> if the given value starts with a blank node prefix
     * '<code>_:</code>'.
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
     * @return <code>true</code> if the provided string is well formed blank node
     *         identifier
     */
    public static boolean isWellFormed(final String blankNodeId) {

        if (blankNodeId.length() < 3) {
            return false;
        }

        int[] chars = blankNodeId.codePoints().toArray();

        if (chars[0] != '_'
                || chars[1] != ':'
                || (PN_CHARS_U.negate().test(chars[2])
                        && ASCII_DIGIT.negate().test(chars[2]))
                || chars[chars.length - 1] == '.') {
            return false;
        }

        if (chars.length == 3) {
            return true;
        }

        for (int i = 3; i < chars.length; i++) {
            if (PN_CHARS.test(chars[i]) || chars[i] == '.') {
                continue;
            }
            return false;
        }
        return true;
    }
}
