package com.apicatalog.jsonld;

public class JsonProvider {

	private static jakarta.json.spi.JsonProvider provider;
	
	public static jakarta.json.spi.JsonProvider instance() {
		if (provider == null) {
			provider = jakarta.json.spi.JsonProvider.provider();
		}
		return provider;
	}
}
