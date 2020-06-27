package com.apicatalog.jsonld.suite;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;

@FunctionalInterface
public interface JsonLdTestCaseMethod {

    Document invoke(JsonLdOptions options) throws JsonLdError;
    
}
