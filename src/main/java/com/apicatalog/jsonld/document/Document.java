package com.apicatalog.jsonld.document;

import java.util.Collection;

import com.apicatalog.json.JsonParser;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdRecord;

public interface Document {
	
	Collection<JsonLdRecord> parse(final JsonParser parser) throws JsonLdError;
	
}
