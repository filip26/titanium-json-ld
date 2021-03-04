package com.apicatalog.jsonld.http;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdError;

public interface HttpClient {

    HttpResponse send(URI targetUri, String requestProfile) throws JsonLdError;

}
