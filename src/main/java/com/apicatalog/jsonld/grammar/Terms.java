package com.apicatalog.jsonld.grammar;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11/#terms">Terms</a>
 *
 */
public final class Terms {

	public static final boolean isTerm(final String name) {

		if ((name == null) || name.isBlank()) {
			return false;
		}
		
		if (Keywords.hasForm(name)) {
			return Keywords.TYPE.equals(name);
		}
		
//TODO 		if (name.contains(":")) {
//			throw new IllegalArgumentException("A term should not include a colon (:)");
//		}
		
		return true;
	}
		
}
