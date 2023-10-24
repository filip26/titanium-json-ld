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
package no.hasmac.jsonld.http;

import java.util.function.IntPredicate;

public final class HttpAlphabet {

    public static final IntPredicate ALPHA = ch -> ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z';

    public static final IntPredicate DIGIT = ch -> ch >= '0' && ch <= '9';

    public static final IntPredicate WHITESPACE = ch -> ch == 0x20 || ch == 0x09;

    public static final IntPredicate T_CHAR =
                                        ALPHA.or(DIGIT.or(ch ->
                                        ch == '!' || ch == '#' || ch == '$' || ch == '%' || ch == '&'
                                        || ch == '\'' || ch == '*' || ch == '+' || ch == '-' || ch == '.'
                                        || ch == '^' || ch == '_' || ch == '`' || ch == '|' || ch == '~'
                                        ));

    public static final IntPredicate OBS_TEXT = ch -> ch >= 0x80 && ch <= 0xff;

    public static final IntPredicate QD_TEXT = OBS_TEXT.or(ch ->
                                        ch == 0x09 || ch == 0x20 || ch == 0x21 || ch >= 0x23 && ch <= 0x5b
                                        || ch >= 0x5d && ch <= 0x7e
                                        );

    private HttpAlphabet() {
    }
}
