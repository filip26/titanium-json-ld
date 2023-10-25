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

import java.util.Arrays;
import java.util.Collection;
import java.util.Set;

public final class Keywords {

    public static final String ANY = "@any";

    public static final String BASE = "@base";

    public static final String CONTAINER = "@container";

    public static final String CONTEXT = "@context";

    public static final String DIRECTION = "@direction";

    public static final String GRAPH = "@graph";

    public static final String ID = "@id";

    public static final String IMPORT = "@import";

    public static final String INCLUDED = "@included";

    public static final String INDEX = "@index";

    public static final String JSON = "@json";

    public static final String LANGUAGE = "@language";

    public static final String LIST = "@list";

    public static final String NEST = "@nest";

    public static final String NONE = "@none";

    public static final String PREFIX = "@prefix";

    public static final String PRESERVE = "@preserve";

    public static final String PROPAGATE = "@propagate";

    public static final String PROTECTED = "@protected";

    public static final String REVERSE = "@reverse";

    public static final String SET = "@set";

    public static final String TYPE = "@type";

    public static final String VALUE = "@value";

    public static final String VERSION = "@version";

    public static final String VOCAB = "@vocab";

    // framing
    public static final String DEFAULT = "@default";

    public static final String EMBED = "@embed";

    public static final String ALWAYS = "@always";

    public static final String ONCE = "@once";

    public static final String NEVER = "@never";

    public static final String EXPLICIT = "@explicit";

    public static final String NULL = "@null";

    public static final String OMIT_DEFAULT = "@omitDefault";

    public static final String REQUIRE_ALL = "@requireAll";

    public static final String MERGED = "@merged";

    // Extension: JSON-LD-STAR (Experimental)
    public static final String ANNOTATION = "@annotation";

    private static final Set<String> ALL_KEYWORDS = Set.of(ANY, BASE, CONTAINER, CONTEXT, DIRECTION, GRAPH,
            ID, IMPORT, INCLUDED, INDEX, JSON, LANGUAGE, LIST, NEST, NONE, PREFIX, PRESERVE, PROPAGATE, PROTECTED, REVERSE, SET,
            TYPE, VALUE, VERSION, VOCAB,
            // framing
            DEFAULT, EMBED, ALWAYS, ONCE, NEVER, EXPLICIT, NULL, OMIT_DEFAULT, REQUIRE_ALL, MERGED,
            // star
            ANNOTATION
            );

    private static final int ALL_KEYWORDS_MAX_LENGTH = ALL_KEYWORDS.stream().mapToInt(String::length).max().getAsInt();
    private static final int ALL_KEYWORDS_MIN_LENGTH = ALL_KEYWORDS.stream().mapToInt(String::length).min().getAsInt();

    protected Keywords() {
    }

    public static boolean contains(final String value) {
        if (value == null) {
            return false;
        }
        int length = value.length();
        if (length >= ALL_KEYWORDS_MIN_LENGTH && length <= ALL_KEYWORDS_MAX_LENGTH) {
            return ALL_KEYWORDS.contains(value);
        }

        return false;
    }

    /**
     * If value has the form of a keyword (i.e., it matches the ABNF rule "@"1*ALPHA
     * from [RFC5234])
     *
     * @param value to check
     * @return <code>true</code> if the provided value has keyword form
     */
    public static boolean matchForm(final String value) {

        if (value.length() < 2 || value.charAt(0) != '@') {
            return false;
        }

        // vanilla approach is 3 times faster than stream.allMatch
        for (int i=1; i < value.length(); i++) {
            if (!Character.isAlphabetic(value.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static boolean noneMatch(final String key, String keyword1, String keyword2) {
        if(key.equals(keyword1) || key.equals(keyword2)){
            return false;
        }
        return true;
    }

    public static boolean noneMatch(final String key, final String... keywords) {
        // vanilla approach is 3 times faster than stream.noneMatch
        for (String k : keywords) {
            if (k.equals(key)) {
                return false;
            }
        }
        return true;
    }

    public static boolean anyMatch(final String key, final String... keywords) {
        return Arrays.asList(keywords).contains(key);
    }

    public static boolean allMatch(final Collection<String> values, final String... keywords) {
        return Arrays.asList(keywords).containsAll(values);
    }
}
