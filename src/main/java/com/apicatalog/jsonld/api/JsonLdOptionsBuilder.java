package com.apicatalog.jsonld.api;

import java.net.URI;

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
}
