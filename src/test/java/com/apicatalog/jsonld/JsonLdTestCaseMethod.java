package com.apicatalog.jsonld;

import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;

@FunctionalInterface
public interface JsonLdTestCaseMethod {

    JsonValue invoke() throws JsonLdError;
    
}
