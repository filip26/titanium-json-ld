package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;

interface DocumentReader<S> {

    Document read(S input) throws JsonLdError;
    
}
