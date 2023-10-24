package no.hasmac.jsonld.http;

import java.net.URI;

import no.hasmac.jsonld.JsonLdError;

public interface HttpClient {

    HttpResponse send(URI targetUri, String requestProfile) throws JsonLdError;

}
