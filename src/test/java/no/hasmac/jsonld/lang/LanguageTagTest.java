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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import no.hasmac.jsonld.lang.LanguageTag.Extension;

class LanguageTagTest {

    @Test
    void testNull() {
        assertThrows(IllegalArgumentException.class, () -> LanguageTag.isWellFormed(null));
    }

    @ParameterizedTest(name = "isWellFormed({0}): {1}")
    @MethodSource("data")
    void testWellFormed(final String tag, final boolean expected) {
        assertEquals(expected, LanguageTag.isWellFormed(tag));
    }

    @ParameterizedTest(name = "isWellFormed({0}): {1}")
    @MethodSource("validTags")
    void testToString(final String tag) {

        final LanguageTag languageTag = LanguageTag.create(tag);

        assertNotNull(languageTag);
        assertEquals(tag, languageTag.toString());
    }

    @ParameterizedTest(name = "parse({0}): {1}")
    @MethodSource("validTags")
    void testParse(final String tag,
            final String language,
            final Collection<String> langExt,
            final String script,
            final String region) {

        final LanguageTag languageTag = LanguageTag.create(tag);

        assertNotNull(languageTag);
        assertEquals(language, languageTag.getLanguage());
        assertEquals(langExt, languageTag.getLanguageExtensions());
        assertEquals(script, languageTag.getScript());
        assertEquals(region, languageTag.getRegion());
    }

    static Stream<Arguments> data() {
        return Stream.of(
                arguments("", false),
                arguments("   ", false),
                arguments("cs--CZ", false),
                arguments("cs-a-CZ", true),
                arguments("cs-", false),
                arguments("-cs", false),
                arguments("c#-CZ", false),
                arguments("A1B2", false),
                arguments("1", false),
                arguments("abcd1234-abcd1234", false),
                arguments("a-b-", false),

                arguments("cs", true),
                arguments("cs-CZ", true),
                arguments("en-US", true),
                arguments("en-US", true),

                arguments("A", false),
                arguments("A-0", false),
                arguments("abcd-1234", true),
                arguments("abcd-1234567890", false),

                // extension subtags
                arguments("en-x-US", true),
                arguments("el-x-koine", true),
                arguments("el-x-attic", true),
                arguments("en-x-lgr", true),
                arguments("de-CH-x-phonebk", true),
                arguments("az-Arab-x-AZE-derbend", true),
                arguments("zh-variant1-variant2", true),
                arguments("zh-variant1-1abc", true),
                arguments("zh-Latn-CN-variant1-a-extend1-b-extend2", true),
                arguments("zh-Latn-CN-variant1-a-extend1-x-wadegile", true),
                arguments("en-Latn-GB-boont-r-extended-sequence-x-private", true),
                arguments("en-Latn-GB-boont-r-extended-sequence-x-private-b-private2", true),
                arguments("en-Latn-GB-boont-r-extended-sequence-x-private-private2", true),

                arguments("de-419-DE", false),
                arguments("a-DE", false),

                // private use
                arguments("x-private", true),

                // grandfathered regular
                arguments("zh-min-nan", true));
    }

    static Stream<Arguments> validTags() {
        return Stream.of(
                arguments("cs", "cs", null, null, null),
                arguments("cs-CZ", "cs", null, null, "CZ"),
                arguments("en-US", "en", null, null, "US"),

                arguments("en-Latn-GB-boont-r-extended-sequence-x-private-b-private2",
                        "en", null, "Latn", "GB", Arrays.asList("boont"),
                        Arrays.asList(new Extension('r', Arrays.asList("extended", "sequence"))),
                        Arrays.asList("private", "b", "private2")));
    }
}
