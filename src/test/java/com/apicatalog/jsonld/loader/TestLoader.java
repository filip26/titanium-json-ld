package com.apicatalog.jsonld.loader;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdException;

public interface TestLoader {

    byte[] fetchBytes(URI create) throws JsonLdException;

}
