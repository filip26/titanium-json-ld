package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;

public interface DocumentRequest {

    DocumentRequest onSuccess(Document document);
    DocumentRequest onError(JsonLdError error);
    
}
