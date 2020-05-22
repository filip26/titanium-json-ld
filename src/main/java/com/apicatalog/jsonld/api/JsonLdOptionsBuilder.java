package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.JsonObject;

import com.apicatalog.jsonld.grammar.Version;

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

	public JsonLdOptionsBuilder mode(Version processingMode) {
		options.setProcessingMode(processingMode);
		return this;
	}

	public JsonLdOptionsBuilder ordered(boolean ordered) {
		options.setOrdered(ordered);
		return this;
	}

	public JsonLdOptionsBuilder expandContext(URI contextLocation) {
		options.setExpandContext(contextLocation);
		return this;
	}
	
	public JsonLdOptionsBuilder expandContext(JsonObject contextObject) {
		options.setExpandContext(contextObject);
		return this;
	}
}
