package com.apicatalog.jsonld.grammar;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.IntStream;

public final class Keywords {

	public static final String BASE = "@base";
	
	public static final String CONTAINER = "@container";
	
	public static final String CONTEXT = "@context";
	
	public static final String DEFAULT = "@default";
	
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
	
	public static final String PROPAGATE = "@propagate";
	
	public static final String PROTECTED = "@protected";
	
	public static final String REVERSE = "@reverse";
	
	public static final String SET = "@set";
	
	public static final String TYPE = "@type";
	
	public static final String VALUE = "@value";
	
	public static final String VERSION = "@version";
	
	public static final String VOCAB = "@vocab";
	
	static final Collection<String> ALL_KEYWORDS = Arrays.asList(
			BASE,
			CONTAINER,
			CONTEXT,
			DEFAULT,
			DIRECTION,
			GRAPH,
			ID,
			IMPORT,
			INCLUDED,
			INDEX,
			JSON,
			LANGUAGE,
			LIST,
			NEST,
			NONE,
			PREFIX,
			PROPAGATE,
			PROTECTED,
			REVERSE,
			SET,
			TYPE,
			VALUE,
			VERSION,
			VOCAB
			);

	protected Keywords() {
	}

	public static boolean contains(final String value) {
		return ALL_KEYWORDS.contains(value);
	}
	
	/**
	 * If value has the form of a keyword (i.e., it matches the ABNF rule "@"1*ALPHA from [RFC5234])
	 * 
	 * @param value
	 * @return
	 */
	public static boolean hasForm(final String value) {
		return value.startsWith("@") 
					&& value.length() > 1 
					&& IntStream.range(1, value.length()).map(value::charAt).allMatch(Character::isAlphabetic);
	}

	public static boolean isNot(String key, String...keywords) {
		return Arrays.stream(keywords).noneMatch(keyword -> keyword.equals(key));		
	}	
	
	public static boolean allIsOneOf(final Collection<String> values, final String...keywords) {
		return values.stream().allMatch(v -> Arrays.asList(keywords).contains(v));
	}
	
}
