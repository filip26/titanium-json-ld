package com.hasmac.jsonld.http;

import java.net.URI;

import com.hasmac.jsonld.JsonLdError;

public interface HttpClient {

    HttpResponse send(URI targetUri, String requestProfile) throws JsonLdError;

}
