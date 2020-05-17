package com.apicatalog.jsonld.grammar;

import java.net.URI;

public class Commons {

	public static boolean isURI(String value) {
		
		try {
			
			return URI.create(value) != null;
			
		} catch (IllegalArgumentException e) {
			
		}
		return false;
	}
	
}
