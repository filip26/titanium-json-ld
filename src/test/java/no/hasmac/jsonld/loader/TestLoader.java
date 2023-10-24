package no.hasmac.jsonld.loader;

import java.net.URI;

import no.hasmac.jsonld.JsonLdError;

public interface TestLoader {

    byte[] fetchBytes(URI create) throws JsonLdError;

}
