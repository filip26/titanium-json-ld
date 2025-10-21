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

    /**
     *
     * @see <a href="https://w3c.github.io/json-ld-framing/#value-matching">Value
     *      Pattern Matching Algorithm</a>
     *
     */
    public static boolean match(final Map<String, ?> pattern, final Map<?, ?> node) {

        final var valuePattern = pattern.getOrDefault(Keywords.VALUE, null);

        final var typePattern = pattern.getOrDefault(Keywords.TYPE, null);

        final var langPattern = pattern.getOrDefault(Keywords.LANGUAGE, null);

        return (valuePattern == null && typePattern == null && langPattern == null)
                || (matchValue(valuePattern, node)
                        && matchType(typePattern, node)
                        && matchLanguage(langPattern, node));
    }

    private static boolean matchValue(final Object pattern, final Map<?, ?> node) {

        final var value = node.getOrDefault(Keywords.VALUE, null);

        return (value != null && isWildcard(pattern))
                || pattern != null && value != null
                        && (pattern instanceof Collection array && array.contains(value)
                                || pattern.equals(value));
    }

    private static boolean matchType(final Object pattern, final Map<?, ?> node) {

        final var type = node.getOrDefault(Keywords.TYPE, null);

        return (type != null && isWildcard(pattern))
                || (type == null && isNone(pattern))
                || (pattern != null
                        && (pattern instanceof Collection array && array.contains(type)
                                || pattern.equals(type)));
    }

    private static boolean matchLanguage(final Object pattern, final Map<?, ?> node) {

        final String lang = node.get(Keywords.LANGUAGE) instanceof String stringValue
                ? stringValue.toLowerCase()
                : null;

        return ((lang != null && isWildcard(pattern)) || (lang == null && isNone(pattern)))
                || (lang != null && pattern != null
                        && ((pattern instanceof Collection<?> col
                                && col.stream().map(String.class::cast).anyMatch(lang::equalsIgnoreCase))
                                || pattern.equals(lang)));
    }

    protected static final boolean isWildcard(final Object value, final String... except) {

        Map<?, ?> frame = null;

        if (value instanceof Map map) {

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
    }
}
