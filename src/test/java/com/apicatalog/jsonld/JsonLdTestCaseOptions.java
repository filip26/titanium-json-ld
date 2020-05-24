package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.grammar.Version;

public class JsonLdTestCaseOptions {

	public Version specVersion;
	public String base;
	public String processingMode;
	public Boolean normative;
	public String expandContext;
	
	public static final JsonLdTestCaseOptions of(JsonObject o) {
		
		final JsonLdTestCaseOptions options = new JsonLdTestCaseOptions();
		
		if (o.containsKey("specVersion")) {
			options.specVersion = Version.of(o.getString("specVersion"));
		}
		options.base = o.getString("base", null);
		options.processingMode = o.getString("processingMode", null);

		if (o.containsKey("normative")) {
			options.normative = o.getBoolean("normative");
		}

		options.expandContext = o.getString("expandContext", null);
		
		return options;
	}

	public void setup(JsonLdOptions options) {

		if (processingMode != null) {
			options.setProcessingMode(Version.of(processingMode));
		}
				
		if (base != null) {
			options.setBase(URI.create(base));
		}
		
		if (expandContext != null) {
			options.setExpandContext(URI.create(expandContext));
		}
	}
	
}
