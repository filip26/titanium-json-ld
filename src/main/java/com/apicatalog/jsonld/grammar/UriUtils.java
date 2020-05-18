package com.apicatalog.jsonld.grammar;

import java.net.URI;

public class UriUtils {

	UriUtils() {
	}
	
	public static boolean isURI(String value) {
		
		try {
			
			return URI.create(value) != null;
			
		} catch (IllegalArgumentException e) {
			
		}
		return false;
	}
	
	/**
	 * @see <a href="https://tools.ietf.org/html/rfc3986#section-2.2">URI - Reserved Characters </a>
	 * @param uri
	 * @return
	 */
	public static boolean endsWithGenDelim(String uri) {
		return uri.endsWith(":") 
				|| uri.endsWith("/") 
				|| uri.endsWith("?") 
				|| uri.endsWith("#") 
				|| uri.endsWith("[")
				|| uri.endsWith("]")
				|| uri.endsWith("@")
				;
	}

	public static boolean isNotURI(String expandedTypeString) {
		return !isURI(expandedTypeString);
	}
}
