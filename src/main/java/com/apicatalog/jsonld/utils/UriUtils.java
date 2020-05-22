package com.apicatalog.jsonld.utils;

import java.net.URI;

import com.apicatalog.jsonld.grammar.Keywords;

public class UriUtils {

	UriUtils() {
	}

	public static boolean isURI(String value) {
		try {

			// TODO ':' 1-length-1 indices only
			return value != null && !value.isBlank()/* && value.indexOf(':', 1) != -1 */ && !Keywords.hasForm(value)
					&& URI.create(value) != null;

		} catch (IllegalArgumentException e) {
			return false;
		}
	}

	/**
	 * @see <a href="https://tools.ietf.org/html/rfc3986#section-2.2">URI - Reserved
	 *      Characters </a>
	 * @param uri
	 * @return
	 */
	public static boolean endsWithGenDelim(String uri) {
		return uri.endsWith(":") || uri.endsWith("/") || uri.endsWith("?") || uri.endsWith("#") || uri.endsWith("[")
				|| uri.endsWith("]") || uri.endsWith("@");
	}

	public static boolean isNotURI(String expandedTypeString) {
		return !isURI(expandedTypeString);
	}

	public static boolean isNotAbsoluteURI(String uri) {
		try {
			return !URI.create(uri).isAbsolute();
		} catch (IllegalArgumentException e) {
			return true;
		}
	}
}
