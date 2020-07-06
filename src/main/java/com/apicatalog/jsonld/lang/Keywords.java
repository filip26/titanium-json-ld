package com.apicatalog.jsonld.lang;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.IntStream;

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
    
    private static final Collection<String> ALL_KEYWORDS = Arrays.asList(ANY, BASE, CONTAINER, CONTEXT, DIRECTION, GRAPH,
            ID, IMPORT, INCLUDED, INDEX, JSON, LANGUAGE, LIST, NEST, NONE, PREFIX, PRESERVE, PROPAGATE, PROTECTED, REVERSE, SET,
            TYPE, VALUE, VERSION, VOCAB,
            // framing
            DEFAULT, EMBED, ALWAYS, ONCE, NEVER, EXPLICIT, NULL, OMIT_DEFAULT, REQUIRE_ALL, MERGED
            );


    protected Keywords() {
    }

    public static boolean contains(final String value) {
        return ALL_KEYWORDS.contains(value);
    }

    /**
     * If value has the form of a keyword (i.e., it matches the ABNF rule "@"1*ALPHA
     * from [RFC5234])
     * 
     * @param value to check
     * @return <code>true</code> if the provided value has keyword form
     */
    public static boolean matchForm(final String value) {
        return value.startsWith("@") && value.length() > 1
                && IntStream.range(1, value.length()).map(value::charAt).allMatch(Character::isAlphabetic);
    }

    public static boolean noneMatch(final String key, final String... keywords) {
        return Arrays.stream(keywords).noneMatch(key::equals);
    }

    public static boolean anyMatch(final String key, final String... keywords) {
        return Arrays.asList(keywords).contains(key);
    }

    public static boolean allMatch(final Collection<String> values, final String... keywords) {
        return Arrays.asList(keywords).containsAll(values);
    }
}
