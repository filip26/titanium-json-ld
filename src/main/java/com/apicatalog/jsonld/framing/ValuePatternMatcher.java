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
package com.apicatalog.jsonld.framing;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import com.apicatalog.jsonld.lang.Keywords;

/**
 *
 * @see <a href="https://w3c.github.io/json-ld-framing/#value-matching">Value
 *      Pattern Matching Algorithm</a>
 *
 */
public final class ValuePatternMatcher {

    // required
    private Map<String, ?> pattern;
    private Map<?, ?> value;

    private ValuePatternMatcher(final Map<String, ?> pattern, final Map<?, ?> value) {
        this.pattern = pattern;
        this.value = value;
    }

    public static final ValuePatternMatcher with(final Map<String, ?> pattern, final Map<?, ?> value) {
        return new ValuePatternMatcher(pattern, value);
    }

    public boolean match() {

        final var valuePattern = pattern.getOrDefault(Keywords.VALUE, null);

        final var typePattern = pattern.getOrDefault(Keywords.TYPE, null);

        final var langPattern = pattern.getOrDefault(Keywords.LANGUAGE, null);

        return (valuePattern == null && typePattern == null && langPattern == null)
                || (matchValue(valuePattern) && matchType(typePattern) && matchLanguage(langPattern));
    }

    private boolean matchValue(final Object value2) {

        final var value1 = value.getOrDefault(Keywords.VALUE, null);

        return (value1 != null && isWildcard(value2))
                || value2 != null && value1 != null
                        && (value2 instanceof Collection array && array.contains(value1)
                                || value2.equals(value1));

//        return (JsonUtils.isNotNull(value1) && isWildcard(value2))
//                    || (JsonUtils.isNotNull(value2) && JsonUtils.toJsonArray(value2).contains(value1))
//                    ;
    }

    private boolean matchType(final Object type2) {

        final var type1 = value.getOrDefault(Keywords.TYPE, null);

        return (type1 != null && isWildcard(type2))
                || (type1 == null && isNone(type2))
                || (type2 != null
                        && (type2 instanceof Collection array && array.contains(type1)
                                || type2.equals(type1)));
    }

    private boolean matchLanguage(final Object lang2) {

        final String lang1 = value.get(Keywords.LANGUAGE) instanceof String lang
                ? lang.toLowerCase()
                : null;

        return ((lang1 != null && isWildcard(lang2)) || (lang1 == null && isNone(lang2)))
                || (lang1 != null && lang2 != null
                        && ((lang2 instanceof Collection<?> col
                                && col.stream().map(String.class::cast).anyMatch(lang1::equalsIgnoreCase))
                                || lang2.equals(lang1)));
//                        && JsonUtils.toStream(lang2)
//                                .map(JsonString.class::cast)
//                                .map(JsonString::getString)
//                                .anyMatch(x -> x.equalsIgnoreCase(lang1)));
    }

    protected static final boolean isWildcard(final Object value, final String... except) {

        Map<String, ?> frame = null;

        if (value instanceof Map map) {

            // wildcard
            if (map.isEmpty()) {
                return true;
            }
            
            frame = map;

        } else if (value instanceof Collection array
                && array.size() == 1
                && array.iterator().next() instanceof Map map) {
            frame = map;
        }

        return frame != null && (frame.isEmpty() || Arrays.asList(
                Keywords.DEFAULT,
                Keywords.OMIT_DEFAULT,
                Keywords.EMBED,
                Keywords.EXPLICIT,
                Keywords.REQUIRE_ALL,
                except).containsAll(frame.keySet()));
    }

    protected static final boolean isNone(Object value) {
        return value == null || value instanceof Collection array && array.isEmpty();
//        return JsonUtils.isNull(value) || JsonUtils.isEmptyArray(value);
    }
}
