package com.apicatalog.jsonld.document;

import java.util.Collection;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdRecord;

public interface Document {
	
	Collection<JsonLdRecord> parse() throws JsonLdError;
	
}
