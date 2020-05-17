package com.apicatalog.jsonld;

import java.net.URI;

public final class JsonLdOptionsBuilder {

	JsonLdOptions options;
	
	public JsonLdOptionsBuilder() {
		this.options = new JsonLdOptions(JsonLdOptions.DEFAULT);
	}

	public JsonLdOptionsBuilder baseUri(URI value) {
		options.setBaseUri(value);
		return this;
	}
		
	public JsonLdOptions create() {
		return options;
	}
}
