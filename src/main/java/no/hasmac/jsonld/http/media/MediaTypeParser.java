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
package no.hasmac.jsonld.http.media;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.IntPredicate;

import no.hasmac.jsonld.StringUtils;
import no.hasmac.jsonld.http.HttpAlphabet;

/**
 *
 * @see <a href="https://tools.ietf.org/html/rfc6838#section-4.2">Media Type Specifications - </a>
 *
 */
final class MediaTypeParser {

    protected static final IntPredicate NAME_FIRST = ch -> ch >= '0' && ch <= '9'
                                                    || ch >= 'a' && ch <= 'z'
                                                    || ch >= 'A' && ch <= 'Z';

    protected static final IntPredicate NAME_CHARS = ch -> ch >= '0' && ch <= '9'
                                                    || ch >= 'a' && ch <= 'z'
                                                    || ch >= 'A' && ch <= 'Z'
                                                    || ch == '!' || ch == '#'
                                                    || ch == '$' || ch == '&'
                                                    || ch == '-' || ch == '^'
                                                    || ch == '_' || ch == '.'
                                                    || ch == '+';

    private enum State { INIT, TYPE, SUBTYPE, PARAMS, PARAM_NAME, PARAM_VALUE, STRING_VALUE, LITERAL_VALUE, ESCAPE }

    private final char[] input;

    public MediaTypeParser(String input) {
        this.input = input.toCharArray();
    }

    public MediaType parse() {

        State state = State.INIT;

        String type = null;
        String subtype = null;
        String paramName = null;

        StringBuilder stringValue = new StringBuilder();

        Map<String, List<String>> params = new LinkedHashMap<>();

        int index = -1;

        for (int i=0; i < input.length; i++) {

            char ch = input[i];

            switch (state) {
            case INIT:
                if (HttpAlphabet.WHITESPACE.test(ch)) {
                    break;
                }
                if (NAME_FIRST.test(ch)) {
                    state = State.TYPE;
                    index = i;
                    break;
                }
                return null;

            case TYPE:
                if (NAME_CHARS.test(ch)) {
                    break;
                }
                if (ch == '/') {
                    type = StringUtils.strip(String.valueOf(input, index,  i - index));
                    state = State.SUBTYPE;
                    index = i + 1;
                    break;
                }
                return null;

            case SUBTYPE:
                if (i == index&& NAME_FIRST.test(ch)
                    || i > index && NAME_CHARS.test(ch)
                    ) {
                    break;
                }
                if (ch == ';') {
                    subtype = StringUtils.strip(String.valueOf(input, index,  i - index));
                    state = State.PARAM_NAME;
                    index = i + 1;
                    break;
                }
                if (HttpAlphabet.WHITESPACE.test(ch)) {
                    subtype = StringUtils.strip(String.valueOf(input, index,  i - index));
                    state = State.PARAMS;
                    break;
                }

                return null;

            case PARAMS:
                if (HttpAlphabet.WHITESPACE.test(ch)) {
                    break;
                }
                if (ch != ';') {
                    return new MediaType(type, subtype, new MediaTypeParameters(params));
                }
                state = State.PARAM_NAME;
                index = i + 1;
                break;

            case PARAM_NAME:
                if (ch == '=') {
                    paramName = StringUtils.strip(String.valueOf(input, index,  i - index)).toLowerCase();
                    state = State.PARAM_VALUE;
                    break;
                }
                if (ch == ';') {
                    paramName = StringUtils.strip(String.valueOf(input, index,  i - index)).toLowerCase();
                    params.computeIfAbsent(paramName, p -> new ArrayList<>()).add(paramName);
                    index = i + 1;
                    break;
                }
                break;

            case PARAM_VALUE:
                if (Character.isSpaceChar(ch) || ch == '\t') {
                    break;
                }

                if (ch == '"') {
                    index = i + 1;
                    state = State.STRING_VALUE;
                    break;
                }

                index = i;
                state = State.LITERAL_VALUE;
                break;

            case LITERAL_VALUE:
                if (ch == ';') {

                    params.computeIfAbsent(paramName, p -> new ArrayList<>()).add(StringUtils.strip(String.valueOf(input, index,  i - index)));
                    index = i + 1;
                    paramName = null;
                    state = State.PARAM_NAME;
                    break;
                }
                break;

            case STRING_VALUE:

                if (ch == '"') {
                    params.computeIfAbsent(paramName, p -> new ArrayList<>()).add(stringValue.toString());
                    stringValue.setLength(0);
                    paramName = null;
                    state = State.PARAMS;
                    break;
                }
                if (ch == '\\') {
                    state = State.ESCAPE;
                    break;
                }
                stringValue.append(ch);
                break;

            case ESCAPE:
                stringValue.append(ch);
                state = State.STRING_VALUE;
                break;
            }
        }

        switch (state) {
        case SUBTYPE:
            if (index < input.length) {
                subtype = StringUtils.strip(String.valueOf(input, index,  input.length - index)).toLowerCase();
            }
            break;

        case PARAM_NAME:
            if (index < input.length) {
                paramName = StringUtils.strip(String.valueOf(input, index,  input.length - index)).toLowerCase();
            }
            break;

        case LITERAL_VALUE:
            if (index < input.length) {
                params.computeIfAbsent(paramName, p -> new ArrayList<>()).add(StringUtils.strip(String.valueOf(input, index,  input.length - index)));
                paramName = null;
            }
            break;

        default:
            break;
        }

        if (paramName != null) {
            if (stringValue.length() > 0) {
                params.computeIfAbsent(paramName, p -> new ArrayList<>()).add(stringValue.toString());
            } else {
                params.computeIfAbsent(paramName, p -> new ArrayList<>()).add(paramName);
            }
        }
        if (type == null || subtype == null) {
            return null;
        }
        return new MediaType(type, subtype, new MediaTypeParameters(params));
    }
}
