package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonObject;

import com.apicatalog.jsonld.grammar.Version;

public class JsonLdTestOptions {

	public String specVersion;
	public String base;
	public String processingMode;
	public Boolean normative;
	public String expandContext;
	
	public static final JsonLdTestOptions of(JsonObject o) {
		final JsonLdTestOptions options = new JsonLdTestOptions();
		
		options.specVersion = o.getString("specVersion", null);
		options.base = o.getString("base", null);
		options.processingMode = o.getString("processingMode", null);
		if (o.containsKey("normative")) {
			options.normative = o.getBoolean("normative");
		}
		options.expandContext = o.getString("expandContext", null);
		
		return options;
	}

	public void setup(JsonLdOptionsBuilder optionsBuilder) {

		if (processingMode != null) {
			optionsBuilder.mode(Version.of(processingMode));
		}
		if (base != null) {
			optionsBuilder.baseUri(URI.create(base));
		}
		
	}
	
}
