package com.hasmac.jsonld.loader;

import java.net.URI;

import com.hasmac.jsonld.JsonLdError;

public interface TestLoader {

    byte[] fetchBytes(URI create) throws JsonLdError;

}
