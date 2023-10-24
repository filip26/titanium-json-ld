package com.hasmac.jsonld.loader;

import com.hasmac.jsonld.JsonLdError;
import com.hasmac.jsonld.document.Document;

interface DocumentReader<S> {

    Document read(S input) throws JsonLdError;

}
