package com.apicatalog.jsonld.document;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;

public interface Document {

	JsonStructure asJsonStructure() throws JsonLdError;

}
