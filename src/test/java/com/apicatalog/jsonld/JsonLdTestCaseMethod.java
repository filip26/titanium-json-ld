package com.apicatalog.jsonld;

import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;

@FunctionalInterface
public interface JsonLdTestCaseMethod {

    JsonValue invoke(JsonLdOptions options) throws JsonLdError;
    
}
