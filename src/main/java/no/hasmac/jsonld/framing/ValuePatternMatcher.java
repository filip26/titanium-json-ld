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
package no.hasmac.jsonld.framing;

import java.util.Arrays;

import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://w3c.github.io/json-ld-framing/#value-matching">Value Pattern Matching Algorithm</a>
 *
 */
public final class ValuePatternMatcher {

    // required
    private JsonObject pattern;
    private JsonObject value;

    private ValuePatternMatcher(final JsonObject pattern, final JsonObject value) {
        this.pattern = pattern;
        this.value = value;
    }

    public static ValuePatternMatcher with(final JsonObject pattern, final JsonObject value) {
        return new ValuePatternMatcher(pattern, value);
    }

    public boolean match() {

        final JsonValue value2 = pattern.getOrDefault(Keywords.VALUE, null);

        final JsonValue type2 = pattern.getOrDefault(Keywords.TYPE, null);

        final JsonValue lang2 = pattern.getOrDefault(Keywords.LANGUAGE, null);

        return (value2 == null && type2 == null && lang2 == null)
                || (matchValue(value2) && matchType(type2) && matchLanguage(lang2));
    }

    private boolean matchValue(final JsonValue value2) {

        final JsonValue value1 = value.getOrDefault(Keywords.VALUE, null);

        return (JsonUtils.isNotNull(value1) && isWildcard(value2))
                    || (JsonUtils.isNotNull(value2) && JsonUtils.toJsonArray(value2).contains(value1))
                    ;
    }

    private boolean matchType(final JsonValue type2) {

        final JsonValue type1 = value.getOrDefault(Keywords.TYPE, null);

        return (JsonUtils.isNotNull(type1) && isWildcard(type2))
                    || (JsonUtils.isNull(type1) && isNone(type2))
                    || (JsonUtils.isNotNull(type2) && JsonUtils.toJsonArray(type2).contains(type1))
                ;
    }

    private boolean matchLanguage(final JsonValue lang2) {

        final String lang1 = value.containsKey(Keywords.LANGUAGE)
                                    ? value.getString(Keywords.LANGUAGE).toLowerCase()
                                    : null;

        return ((lang1 != null && isWildcard(lang2)) || (lang1 == null && isNone(lang2)))
                || (lang1 != null && lang2 != null
                        && JsonUtils.isNotNull(lang2)
                        && JsonUtils.toStream(lang2)
                                        .map(JsonString.class::cast)
                                        .map(JsonString::getString)
                                        .anyMatch(x -> x.equalsIgnoreCase(lang1)));
    }

    protected static boolean isWildcard(final JsonValue value, final String... except) {

        if (JsonUtils.isEmptyObject(value)) {
            return true;
        }

        JsonObject frame = null;

        if (JsonUtils.isObject(value)) {

            frame = (JsonObject)value;

        } else if (JsonUtils.isArray(value)
                    && value.asJsonArray().size() == 1
                    && JsonUtils.isObject(value.asJsonArray().get(0))) {

            frame = value.asJsonArray().getJsonObject(0);
        }

        return frame != null && (frame.isEmpty() || Arrays.asList(
                Keywords.DEFAULT,
                Keywords.OMIT_DEFAULT,
                Keywords.EMBED,
                Keywords.EXPLICIT,
                Keywords.REQUIRE_ALL,
                except
        ).containsAll(frame.keySet()));
    }

    protected static boolean isNone(JsonValue value) {
        return JsonUtils.isNull(value) || JsonUtils.isEmptyArray(value);
    }
}
