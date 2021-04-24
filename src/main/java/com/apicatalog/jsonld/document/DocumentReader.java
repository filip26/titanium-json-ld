package com.apicatalog.jsonld.document;

import com.apicatalog.jsonld.JsonLdError;

public interface DocumentReader<S> {

    Document read(S input) throws JsonLdError;
    
}
