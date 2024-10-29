package com.apicatalog.jsonld.loader;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;

interface DocumentReader<S> {

    Document read(URI targetUri, URI contextUrl, S input) throws JsonLdError;

}
