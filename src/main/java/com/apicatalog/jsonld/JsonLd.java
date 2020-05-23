package com.apicatalog.jsonld;

import com.apicatalog.jsonld.api.JsonLdProcessor;

public interface JsonLd {

	public static JsonLdProcessor createProcessor() {
		return new JsonLd11Processor();
	}
	
}
