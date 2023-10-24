package no.hasmac.jsonld.loader;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.document.Document;

interface DocumentReader<S> {

    Document read(S input) throws JsonLdError;

}
