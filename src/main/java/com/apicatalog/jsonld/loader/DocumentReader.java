package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;

@Deprecated
interface DocumentReader<S> {

    Document read(S input) throws JsonLdException;

}
